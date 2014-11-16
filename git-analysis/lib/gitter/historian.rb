require 'gitter/commit'

module Gitter
  class Historian
    def initialize(git)
      @git = git
    end

    def latest
      Gitter::Commit.parse_next(@git.log)
    end
  end
end
