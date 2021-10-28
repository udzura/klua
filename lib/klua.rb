# frozen_string_literal: true

require_relative "klua/version"

module Klua
  class Token
    def initialize(type, value)
      @type = type
      @value = value
    end
    attr_reader :type, :value
  end

  class Node
    def initialize(type, nodes, term)
      @type = type
      @nodes = nodes
      @term = term
    end
    attr_reader :type, :nodes, :term
  end

  class Root < Node
    def initialize(root)
      @root = root
      @type = :root
      @nodes = []
      @term = nil
    end

    attr_reader :root
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
    def initialize
    end

    def parse(tokens)
      Root.new(
        Node.new(:dummy, [], nil)
      )
    end
  end
end
