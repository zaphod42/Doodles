module Gitter
  class FakeGit
    def initialize(outputs)
      @outputs = outputs
    end

    def log(&block)
      @outputs[:log].each_line &block
    end
  end
end

