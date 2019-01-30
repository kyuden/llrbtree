require "bundler/gem_tasks"
require "rake/testtask"
require "rake/extensiontask"

Rake::TestTask.new(:test) do |t|
  t.libs << "test"
  t.libs << "lib"
  t.test_files = FileList["test/**/*_test.rb"]
end

Rake::Task[:test].prerequisites << :compile

Rake::ExtensionTask.new 'llrbtree' do |ext|
  ext.lib_dir = 'lib/llrbtree'
end

task default: %i(compile test)
