module Gitter
  class Commit
    attr_reader :author, :id

    def self.parse_next(input)
      message = header(input) + message(input)
      author = nil
      id = nil
      input.each do |line|
        case line
        when /^commit ([a-f0-9]+)$/
          id = $1
        when /^Author:\s*([^<]*)<[^>]*>$/
          author = $1.chomp(' ')
        end
      end
      Gitter::Commit.new(author, id)
    end

    def initialize(author, id)
      @author = author
      @id = id
    end

  private

    def self.header(input)
      line = input.next
      while line =~ /^\S/
    end
  end
end
