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

      @tokens
    end

    private
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
