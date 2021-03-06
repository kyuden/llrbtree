Gem::Specification.new do |spec|
  spec.name          = 'llrbtree'
  spec.version       = '0.0.3'
  spec.authors       = ['kyuden']
  spec.email         = ['msmsms.um@gmail.com']

  spec.summary       = %q{Left-Leaning Red-Black Tree for Ruby.}
  spec.description   = %q{Left-Leaning Red-Black Tree for Ruby.}
  spec.homepage      = 'https://github.com/kyuden/llrbtree'

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files = Dir.chdir(File.expand_path('..', __FILE__)) do
    `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }
  end
  spec.bindir        = 'exe'
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ['lib']
  spec.extensions = %w[ext/llrbtree/extconf.rb]

  spec.add_development_dependency 'bundler'
  spec.add_development_dependency 'rake'
  spec.add_development_dependency 'test-unit'
  spec.add_development_dependency 'rake-compiler'
end
