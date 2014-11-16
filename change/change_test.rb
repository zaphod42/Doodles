require 'minitest/autorun'
require 'change'

describe Bank do
  it "changes a penny" do
    assert_change 1, [1]
  end

  it "changes a 2 cents into 2 pennies" do
    assert_change 2, [1, 1]
  end

  it "changes a 5 cents into a nickel" do
    assert_change 5, [5]
  end

  it "changes a 6 cents into a nickel and a penny" do
    assert_change 6, [1, 5]
  end

  it "changes a 10 cents into a dime" do
    assert_change 10, [10]
  end

  it "changes a 11 cents into a dime and a penny" do
    assert_change 11, [10, 1]
  end

  it "changes a 20 cents into 2 dimes" do
    assert_change 20, [10, 10]
  end

  it "changes a 24 cents into 2 dimes and 4 pennies" do
    assert_change 24, [10, 10, 1, 1, 1, 1]
  end

  it "changes a 25 cents into a quarter" do
    assert_change 25, [25]
  end

  it "changes a 26 cents into a quarter and a penny" do
    assert_change 26, [25, 1]
  end

  def assert_change(amount, coins)
    assert_equal coins.sort, Bank.change(amount).sort
  end
end
