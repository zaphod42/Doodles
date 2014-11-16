module MaybeSome
  def when_defined(primary, alternate)
    primary.call(self)
  end
end

module MaybeNothing
  def when_defined(primary, alternate)
    alternate.call(self)
  end
end

class Object
  include MaybeSome
end

class NilClass
  include MaybeNothing
end

class TrueClass
  def choose(primary, alternate)
    primary.call
  end
end

class FalseClass
  def choose(primary, alternate)
    alternate.call
  end
end

class Fixnum
  def unfold(finished = ->(v) { v == 0 }, &block)
    finished[self].choose ->() { [] },
                          ->() do
                            unfolded, cont = block[self]
                            [unfolded] + cont.unfold(finished, &block)
                          end
  end
end

module Bank
  module_function

  COINS = [25, 10, 5, 1]

  def change(amount)
    amount.unfold do |a|
      available_coin = ->(coin) { a >= coin }
      COINS.
        find(&available_coin).
        when_defined ->(coin) { [coin, a - coin] },
                     ->(coin) { [nil, 0] }
    end
  end
end
