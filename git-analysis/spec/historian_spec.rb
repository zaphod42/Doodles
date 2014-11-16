require 'minitest/autorun'
require 'ramcrest'

require 'gitter/historian'
require 'gitter/fake_git'

describe Gitter::Historian do
  include Ramcrest::HasSize
  include Ramcrest::IncludesExactly
  include Ramcrest::HasAttribute
  include Ramcrest::EqualTo
  include Ramcrest::SuchThat

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

  it "reads the author" do
    git = Gitter::FakeGit.new(:log => single_commit_log)
    historian = Gitter::Historian.new(git)

    assert_that historian.latest, a_commit_by(name_of_commit_author)
  end

  it "reads the commit id" do
    git = Gitter::FakeGit.new(:log => single_commit_log)
    historian = Gitter::Historian.new(git)

    assert_that historian.latest, a_commit_identified_by(commit_id)
  end

  def a_commit_by(name)
    has_attribute(:author, equal_to(name))
  end

  def a_commit_identified_by(id)
    has_attribute(:id, equal_to(id))
  end
end
