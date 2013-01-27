require 'gitter/commit'

module Gitter
  class Historian
    def initialize(git)
      @git = git
    end

    def all_history
      author = nil
      id = nil
      @git.log do |line|
        case line
        when /^commit ([a-f0-9]+)$/
          id = $1
        when /^Author:\s*([^<]*)<[^>]*>$/
          author = $1.chomp(' ')
        end
      end
      [Gitter::Commit.new(author, id)]
    end
  end
end
