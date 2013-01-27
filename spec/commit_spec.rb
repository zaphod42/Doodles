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

  it "parses the author" do
    assert_that the_commit_by_parsing(single_commit_log), a_commit_by(name_of_commit_author)
  end

  it "parses the commit id" do
    assert_that the_commit_by_parsing(single_commit_log), a_commit_identified_by(commit_id)
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
