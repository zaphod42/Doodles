require 'minitest/autorun'
require 'ramcrest'

require 'gitter/commit'

describe Gitter::Commit do
  include Ramcrest::EqualTo
  include Ramcrest::HasAttribute

  let(:commit_id) { '7e9f60ed80b6728fcb19a7a94bda50da45b2a692' }
  let(:name_of_commit_author) { 'Andrew Parker' }
  let(:single_commit_log) do
    <<-LOG
commit #{commit_id}
Merge: aa58201 88d210e
Author: #{name_of_commit_author} <andy@puppetlabs.com>
Date:   Thu Jan 24 18:11:44 2013 -0800

    Merge pull request #1427 from hlindberg/deprecate-ruby-dsl

    Add deprecation warning for use of ruby dsl.

    LOG
  end

  let(:second_commit_id) { '88d210ee297feb7ae48aa66a9e42ea27fe19e63d' }
  let(:name_of_second_commit_author) { 'Henrik Lindberg' }
  let(:multiple_commit_log) do
    <<-LOG
commit #{commit_id}
Merge: aa58201 88d210e
Author: #{name_of_commit_author} <andy@puppetlabs.com>
Date:   Thu Jan 24 18:11:44 2013 -0800

    Merge pull request #1427 from hlindberg/deprecate-ruby-dsl

    Add deprecation warning for use of ruby dsl.

commit #{second_commit_id}
Author: #{name_of_second_commit_author} <henrik.lindberg@cloudsmith.com>
Date:   Fri Jan 25 02:27:01 2013 +0100

    (#18876) Add deprecation warning for use of ruby dsl.

    This adds a deprecation warning for parsing of ruby based manifests.
    Tests are updated to check that the deprecation warning is issued for
    ruby manifest, but not otherwise.

    LOG
  end

  it "parses the author" do
    assert_that the_commit_by_parsing(single_commit_log), a_commit_by(name_of_commit_author)
  end

  it "parses the commit id" do
    assert_that the_commit_by_parsing(single_commit_log), a_commit_identified_by(commit_id)
  end

  it "does not parse the commit afterward" do
    assert_that the_commit_by_parsing(multiple_commit_log), a_commit_identified_by(commit_id)
  end

  def the_commit_by_parsing(text)
    Gitter::Commit.parse_next(text.lines)
  end

  def a_commit_by(name)
    has_attribute(:author, equal_to(name))
  end

  def a_commit_identified_by(id)
    has_attribute(:id, equal_to(id))
  end
end
