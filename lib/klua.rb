# frozen_string_literal: true

require_relative "klua/version"

module Klua
  class Token
    def initialize(type, value)
      @type = type
      @value = value
    end
    attr_reader :type, :value

    def inspect
      "<Token type: #{@type} value: #{@value.inspect}>"
    end
  end

  class Node
    def initialize(type, nodes, term)
      @type = type
      @nodes = nodes
      @term = term
    end
    attr_reader :type, :nodes, :term

    def inspect
      "<Node type: #{@type} nodes: #{@nodes.inspect} term: #{@term.inspect}>"
    end
  end

  class Root < Node
    def initialize(root)
      @root = root
      @type = :root
      @nodes = []
      @term = nil
    end

    attr_reader :root

    def inspect
      "<Node type: #{@type} root: #{@root.inspect}>"
    end
  end

  class Scanner
    def initialize
    end

    def scan(source)
      @source = source.bytes
      @start = 0
      @current = 0
      @tokens = []

      while !at_end?
        @start = @current
        scan_token
      end

      @tokens << Token.new(:eof, "")

      @tokens
    end

    private
    def scan_token
      char = succ
      case char
      when '+'.ord
        add_token(:+)
      when '-'.ord
        add_token(:-)
      when '*'.ord
        add_token(:*)
      when '/'.ord
        add_token(:/)
      when '<'.ord
        add_token(:<)
      when '>'.ord
        add_token(:>)
      when '='.ord
        if match('='.ord)
          add_token(:==)
        else
          add_token(:assign)
        end
      when '~'.ord
        if match('='.ord)
          add_token(:noteq)
        else
          raise "Unexpected character"
        end
      when ','.ord
        add_token(:comma)
      when ';'.ord
        add_token(:semicolon)
      when '('.ord
        add_token(:lbrace)
      when ')'.ord
        add_token(:rbrace)
      when ' '.ord, "\r".ord, "\n".ord, "\t".ord
        # Skip!
        return
      when '"'.ord
        as_string
      else
        if digit?(char)
          as_number
        elsif alpha?(char)
          as_identifier
        else
          raise "Unexpected character"
        end
      end
    end

    def add_token(type)
      value = @source[@start...@current]&.map{|v| v.chr }.join
      raise "Invalid token range" unless value
      @tokens << Token.new(type, value)
    end

    def as_string
      while peek() != '"'.ord && !at_end?
        succ()
      end

      if at_end?
        raise "Unterminated string."
      end
      # consume "
      succ()

      start = @start + 1
      end_ = @current - 1
      lit = @source[start...end_]&.map{|v| v.chr }.join
      raise "Invalid literal range" unless lit
      @tokens << Token.new(:literal_str, lit)
    end

    def as_number
      succ() while digit?(peek)

      lit = @source[@start...@current]&.map{|v| v.chr }.join
      raise "Invalid number range" unless lit

      @tokens << Token.new(:number, lit)
    end

    def as_identifier
      succ() while alphanumeric?(peek)
      lit = @source[@start...@current]&.map{|v| v.chr }.join
      raise "Invalid number range" unless lit

      if sym = get_reserved_sym(lit)
        @tokens << Token.new(sym, lit)
      else
        @tokens << Token.new(:identifier, lit)
      end
    end

    def get_reserved_sym(lit)
      case lit
      when "local"
        :local
      when "if"
        :if
      when "then"
        :then
      when "else"
        :else
      when "end"
        :end
      when "and"
        :and
      when "or"
        :or
      when "not"
        :not
      else
        nil
      end
    end

    def succ
      char = @source[@current]
      # unless char
      #   raise "Overgone"
      # end
      @current += 1
      char
    end

    def match(expected)
      return false if at_end?

      char = @source[@current]
      return false if char != expected

      @current += 1
      true
    end

    def peek
      if at_end?
        0
      else
        @source[@current]
      end
    end

    def peek_next
      if @current + 1 >= @source.size
        0
      else
        @source[@current + 1]
      end
    end

    def digit?(char)
      char >= '0'.ord && char <= '9'.ord
    end

    def alpha?(char)
      (char >= 'a'.ord && char <= 'z'.ord) ||
      (char >= 'A'.ord && char <= 'Z'.ord) ||
      char == '_'.ord
    end

    def alphanumeric?(char)
      digit?(char) || alpha?(char)
    end

    def at_end?
      @current >= @source.size
    end
  end

  class Parser
    def self.parse_through(string)
      tokens = Klua::Scanner.new.scan string
      ast = self.new.parse(tokens)
      [tokens, ast]
    end

    def initialize
      @tokens = [Token.new(:dummy, "")]
      @current = 0
      @block_level = 0
    end

    def parse(tokens)
      @current = 0
      @block_level = 0
      @tokens = tokens

      @node = block()
      Root.new(@node)
    end

    private
    def block
      @block_level += 1
      ret = Node.new(:block, stats(), nil)
      @block_level -= 1
      if toplevel? && !at_end?
        raise "Unexpedted token: #{peek.inspect}"
      end
    end

    def stats
      statements = []
      while !at_end?
        if got = stat()
          statements.push got
        else
          break
        end
      end
      statements
    end

    def stat
      if match(:local)
        varstat()
      elsif match(:if)
        ifstat()
      elsif peek_next.type == :assign
        assignstat()
      elsif check(:lbrace) || check(:identifier)
        funcallstat()
      else
        nil
      end
    end

    def varstat
      nodes = [var()]
      if match(:assign)
        nodes << exp()
      end
      consume(:semicolon, "Expect ; at the end of stat")

      Node.new(:varstat, nodes, nil)
    end

    def ifstat
      nodes = []
      nodes << exp()
      consume(:then, "Expect then after if condition")
      nodes << block()
      if match(:else)
        nodes << block()
      end
      consume(:end, "Expect end after if stat")
      consume(:semicolon, "Expect ; at the end of stat")

      Node.new(:ifstat, nodes, nil)
    end

    def assignstat
      nodes = [var()]
      consume(:assign, "Expect = for assignment")
      nodes << exp()
      consume(:semicolon, "Expect ; at the end of stat")
      Node.new(:assignstat, [], nil)
    end

    def funcallstat
      fc = functioncall()
      consume(:semicolon, "Expect ; at the end of stat")
      Node.new(:funcallstat, [fc], nil)
    end

    def var
      token = consume(:identifier, "Expect identifier for variable name")
      Node.new(:term, [], token)
    end

    def exp
      Node.new(:exp, [binary()], nil)
    end

    def binary
      left = unary()
      if op = binop?()
        right = unary()
        Node.new(:binary, [left, op ,right], nil)
      else
        Node.new(:binary, [left], nil)
      end
    end

    def unary
      if op = unop?()
        right = unary()
        Node.new(:unary, [op, right], nil)
      elsif peek_next.type == :lbrace
        Node.new(:unary, [functioncall()], nil)
      else
        Node.new(:unary, [primary()], nil)
      end
    end

    def functioncall
      nodes = [primary()]
      nodes.push(args())
      Node.new(:functioncall, nodes, nil)
    end

    def primary
      if match(:nil)
        Node.new(:primary, [term!(:nil)], nil)
      elsif match(:false)
        Node.new(:primary, [term!(:false)], nil)
      elsif match(:true)
        Node.new(:primary, [term!(:true)], nil)
      elsif match(:number)
        Node.new(:primary, [term!(:number)], nil)
      elsif match(:literal_str)
        Node.new(:primary, [term!(:literal_str)], nil)
      elsif match(:lbrace)
        r = Node.new(:primary, [exp()], nil)
        consume(:identifier, "Expect ) afrer (")
        r
      else
        Node.new(:primary, [var()], nil)
      end
    end

    def args
      nodes = []
      consume(:lbrace, "Expect ( on starting args")
      if ! match(:rbrace)
        nodes << exp()
        consume(:rbrace, "Expect ) afrer (")
      end
      Node.new(:args, nodes, nil)
    end

    def binop?
      if check(:+) || check(:-) || check(:*) || check(:/) || \
         check(:<) || check(:>) || check(:==) || check(:noteq)
        return term(succ())
      else
        nil
      end
    end

    def unop?
      if check(:-) || check(:not)
        return term(succ())
      else
        nil
      end
    end

    def term!(token_type)
      token = Token.new(token, token.to_s)
      term(token)
    end

    def term(token)
      Node.new(:term, [], token)
    end

    def match(*types)
      types.each do |type|
        if check(type)
          succ()
          return true
        end
      end

      false
    end

    def check(type)
      if at_end?
        false
      else
        peek.type == type
      end
    end

    def succ
      if !at_end?
        @current += 1
      end
      previous
    end

    def consume(type, message)
      if check(type)
        succ()
      else
        raise "#{message} - expected #{type} but got <token: #{peek.inspect}>"
      end
    end

    def at_end?
      peek.type == :eof
    end

    def peek
      @tokens[@current] || raise("EOF or out of range")
    end

    def peek_next
      if at_end?
        peek
      else
        @tokens[@current + 1] || raise("EOF or out of range")
      end
    end

    def previous
      @tokens[@current - 1] || raise("EOF or out of range")
    end

    def toplevel?
      @block_level <= 0
    end
  end
end
