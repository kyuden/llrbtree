require 'llrbtree'
require 'test/unit.rb'

class LLRBTreeTest < Test::Unit::TestCase
  def setup
    @llrbtree = LLRBTree['b', 'B', 'd', 'D', 'a', 'A', 'c', 'C']
  end

  def have_enumerator?
    defined?(Enumerable::Enumerator) || defined?(Enumerator)
  end

  def test_neb
    assert_nothing_raised do
      LLRBTree.new
      LLRBTree.new('a')
      LLRBTree.new { 'a' }
    end
    assert_raises(ArgumentError) { LLRBTree.new('a') {} }
    assert_raises(ArgumentError) { LLRBTree.new('a', 'a') }

    assert_nothing_raised do
      LLRBTree.new(&->(a, b) {})
      LLRBTree.new(&->(*a) {})
      LLRBTree.new(&->(a, *b) {})
      LLRBTree.new(&->(a, b, *c) {})
    end
    assert_raises(TypeError) { LLRBTree.new(&->(a) {}) }
    assert_raises(TypeError) { LLRBTree.new(&->(a, b, c) {}) }
    assert_raises(TypeError) { LLRBTree.new(&->(a, b, c, *d) {}) }
  end

  def test_aref
    assert_equal('A', @llrbtree['a'])
    assert_equal('B', @llrbtree['b'])
    assert_equal('C', @llrbtree['c'])
    assert_equal('D', @llrbtree['d'])

    assert_equal(nil, @llrbtree['e'])
    @llrbtree.default = 'E'
    assert_equal('E', @llrbtree['e'])
  end

  def test_size
    assert_equal(4, @llrbtree.size)
  end

  def test_create
    llrbtree = LLRBTree[]
    assert_equal(0, llrbtree.size)

    llrbtree = LLRBTree[@llrbtree]
    assert_equal(4, llrbtree.size)
    assert_equal('A', @llrbtree['a'])
    assert_equal('B', @llrbtree['b'])
    assert_equal('C', @llrbtree['c'])
    assert_equal('D', @llrbtree['d'])

    llrbtree = LLRBTree[LLRBTree.new('e')]
    assert_equal(nil, llrbtree.default)
    llrbtree = LLRBTree[LLRBTree.new { 'e' }]
    assert_equal(nil, llrbtree.default_proc)
    @llrbtree.readjust { |a, b| b <=> a }
    assert_equal(nil, LLRBTree[@llrbtree].cmp_proc)

    assert_raises(ArgumentError) { LLRBTree['e'] }

    llrbtree = LLRBTree[Hash['b', 'B', 'd', 'D', 'a', 'A', 'c', 'C']]
    assert_equal(4, llrbtree.size)
    assert_equal('A', llrbtree['a'])
    assert_equal('B', llrbtree['b'])
    assert_equal('C', llrbtree['c'])
    assert_equal('D', llrbtree['d'])

    llrbtree = LLRBTree[[%w[a A], %w[b B], %w[c C], %w[d D]]]
    assert_equal(4, llrbtree.size)
    assert_equal('A', llrbtree['a'])
    assert_equal('B', llrbtree['b'])
    assert_equal('C', llrbtree['c'])
    assert_equal('D', llrbtree['d'])

    # assert_raises(ArgumentError) { LLRBTree[["a"]] }

    llrbtree = LLRBTree[[['a']]]
    assert_equal(1, llrbtree.size)
    assert_equal(nil, llrbtree['a'])

    # assert_raises(ArgumentError) { LLRBTree[[["a", "A", "b", "B"]]] }
  end

  def test_clear
    @llrbtree.clear
    assert_equal(0, @llrbtree.size)
  end

  def test_aset
    @llrbtree['e'] = 'E'
    assert_equal(5, @llrbtree.size)
    assert_equal('E', @llrbtree['e'])

    @llrbtree['c'] = 'E'
    assert_equal(5, @llrbtree.size)
    assert_equal('E', @llrbtree['c'])

    assert_raises(ArgumentError) { @llrbtree[100] = 100 }
    assert_equal(5, @llrbtree.size)

    key = 'f'
    @llrbtree[key] = 'F'
    cloned_key = @llrbtree.last[0]
    assert_equal('f', cloned_key)
    assert_not_same(key, cloned_key)
    assert_equal(true, cloned_key.frozen?)

    @llrbtree['f'] = 'F'
    assert_same(cloned_key, @llrbtree.last[0])

    llrbtree = LLRBTree.new
    key = ['g']
    llrbtree[key] = 'G'
    assert_same(key, llrbtree.first[0])
    assert_equal(false, key.frozen?)
  end

  def test_clone
    clone = @llrbtree.clone
    assert_equal(4, @llrbtree.size)
    assert_equal('A', @llrbtree['a'])
    assert_equal('B', @llrbtree['b'])
    assert_equal('C', @llrbtree['c'])
    assert_equal('D', @llrbtree['d'])

    llrbtree = LLRBTree.new('e')
    clone = llrbtree.clone
    assert_equal('e', clone.default)

    llrbtree = LLRBTree.new { 'e' }
    clone = llrbtree.clone
    assert_equal('e', clone.default(nil))

    llrbtree = LLRBTree.new
    llrbtree.readjust { |a, b| a <=> b }
    clone = llrbtree.clone
    assert_equal(llrbtree.cmp_proc, clone.cmp_proc)
  end

  def test_default
    llrbtree = LLRBTree.new('e')
    assert_equal('e', llrbtree.default)
    assert_equal('e', llrbtree.default('f'))
    assert_raises(ArgumentError) { llrbtree.default('e', 'f') }

    llrbtree = LLRBTree.new { |_tree, key| @llrbtree[key || 'c'] }
    assert_equal(nil, llrbtree.default)
    assert_equal('C', llrbtree.default(nil))
    assert_equal('B', llrbtree.default('b'))
  end

  def test_set_default
    llrbtree = LLRBTree.new { 'e' }
    llrbtree.default = 'f'
    assert_equal('f', llrbtree.default)
    assert_equal(nil, llrbtree.default_proc)

    llrbtree = LLRBTree.new { 'e' }
    llrbtree.default = nil
    assert_equal(nil, llrbtree.default)
    assert_equal(nil, llrbtree.default_proc)
  end

  def test_default_proc
    llrbtree = LLRBTree.new('e')
    assert_equal(nil, llrbtree.default_proc)

    llrbtree = LLRBTree.new { 'f' }
    assert_equal('f', llrbtree.default_proc.call)
  end

  def test_set_default_proc
    llrbtree = LLRBTree.new('e')
    llrbtree.default_proc = proc { 'f' }
    assert_equal(nil, llrbtree.default)
    assert_equal('f', llrbtree.default_proc.call)

    llrbtree = LLRBTree.new('e')
    llrbtree.default_proc = nil
    assert_equal(nil, llrbtree.default)
    assert_equal(nil, llrbtree.default_proc)

    if Symbol.method_defined?(:to_proc)
      @llrbtree.default_proc = :upper_bound
      assert_equal(%w[d D], @llrbtree['e'])
    end

    assert_raises(TypeError) { llrbtree.default_proc = 'f' }

    if RUBY_VERSION >= '1.9.2'
      assert_nothing_raised do
        @llrbtree.default_proc = ->(a, b) {}
        @llrbtree.default_proc = ->(*a) {}
        @llrbtree.default_proc = ->(a, *b) {}
        @llrbtree.default_proc = ->(a, b, *c) {}
      end
      assert_raises(TypeError) { @llrbtree.default_proc = ->(a) {} }
      assert_raises(TypeError) { @llrbtree.default_proc = ->(a, b, c) {} }
      assert_raises(TypeError) { @llrbtree.default_proc = ->(a, b, c, *d) {} }
    end
  end

  def test_equal
    assert_equal(LLRBTree.new, LLRBTree.new)
    assert_equal(@llrbtree, @llrbtree)
    assert_not_equal(@llrbtree, LLRBTree.new)

    llrbtree = LLRBTree['b', 'B', 'd', 'D', 'a', 'A', 'c', 'C']
    assert_equal(@llrbtree, llrbtree)
    llrbtree['d'] = 'A'
    assert_not_equal(@llrbtree, llrbtree)
    llrbtree['d'] = 'D'
    llrbtree['e'] = 'E'
    assert_not_equal(@llrbtree, llrbtree)
    @llrbtree['e'] = 'E'
    assert_equal(@llrbtree, llrbtree)

    llrbtree.default = 'e'
    assert_equal(@llrbtree, llrbtree)
    @llrbtree.default = 'f'
    assert_equal(@llrbtree, llrbtree)

    a = LLRBTree.new('e')
    b = LLRBTree.new { 'f' }
    assert_equal(a, b)
    assert_equal(b, b.clone)

    a = LLRBTree.new
    b = LLRBTree.new
    a.readjust { |x, y| x <=> y }
    assert_not_equal(a, b)
    b.readjust(a.cmp_proc)
    assert_equal(a, b)

    if RUBY_VERSION >= '1.8.7'
      a = LLRBTree.new
      a[1] = a
      b = LLRBTree.new
      b[1] = b
      assert_equal(a, b)
    end
  end

  def test_fetch
    assert_equal('A', @llrbtree.fetch('a'))
    assert_equal('B', @llrbtree.fetch('b'))
    assert_equal('C', @llrbtree.fetch('c'))
    assert_equal('D', @llrbtree.fetch('d'))

    assert_raises(IndexError) { @llrbtree.fetch('e') }

    assert_equal('E', @llrbtree.fetch('e', 'E'))
    assert_equal('E', @llrbtree.fetch('e') { 'E' })
    assert_equal('E', @llrbtree.fetch('e', 'F') { 'E' })

    assert_raises(ArgumentError) { @llrbtree.fetch }
    assert_raises(ArgumentError) { @llrbtree.fetch('e', 'E', 'E') }
  end

  def test_key
    assert_equal('a', @llrbtree.key('A'))
    assert_equal(nil, @llrbtree.key('E'))
  end

  def test_empty_p
    assert_equal(false, @llrbtree.empty?)
    @llrbtree.clear
    assert_equal(true, @llrbtree.empty?)
  end

  def test_each
    result = []
    @llrbtree.each { |key, val| result << key << val }
    assert_equal(%w[a A b B c C d D], result)

    assert_raises(TypeError) do
      @llrbtree.each { @llrbtree['e'] = 'E' }
    end
    assert_equal(4, @llrbtree.size)

    @llrbtree.each do
      @llrbtree.each {}
      assert_raises(TypeError) do
        @llrbtree['e'] = 'E'
      end
      break
    end
    assert_equal(4, @llrbtree.size)

    if have_enumerator?
      enumerator = @llrbtree.each
      assert_equal(%w[a A b B c C d D], enumerator.to_a.flatten)
    end
  end

  def test_each_key
    result = []
    @llrbtree.each_key { |key| result.push(key) }
    assert_equal(%w[a b c d], result)

    assert_raises(TypeError) do
      @llrbtree.each_key { @llrbtree['e'] = 'E' }
    end
    assert_equal(4, @llrbtree.size)

    @llrbtree.each_key do
      @llrbtree.each_key {}
      assert_raises(TypeError) do
        @llrbtree['e'] = 'E'
      end
      break
    end
    assert_equal(4, @llrbtree.size)

    if have_enumerator?
      enumerator = @llrbtree.each_key
      assert_equal(%w[a b c d], enumerator.to_a.flatten)
    end
  end

  def test_each_value
    result = []
    @llrbtree.each_value { |val| result.push(val) }
    assert_equal(%w[A B C D], result)

    assert_raises(TypeError) do
      @llrbtree.each_value { @llrbtree['e'] = 'E' }
    end
    assert_equal(4, @llrbtree.size)

    @llrbtree.each_value do
      @llrbtree.each_value {}
      assert_raises(TypeError) do
        @llrbtree['e'] = 'E'
      end
      break
    end
    assert_equal(4, @llrbtree.size)

    if have_enumerator?
      enumerator = @llrbtree.each_value
      assert_equal(%w[A B C D], enumerator.to_a.flatten)
    end
  end

  def test_shift
    result = @llrbtree.shift
    assert_equal(3, @llrbtree.size)
    assert_equal(%w[a A], result)
    assert_equal(nil, @llrbtree['a'])

    3.times { @llrbtree.shift }
    assert_equal(0, @llrbtree.size)
    assert_equal(nil, @llrbtree.shift)
    @llrbtree.default = 'e'
    assert_equal('e', @llrbtree.shift)

    llrbtree = LLRBTree.new { 'e' }
    assert_equal('e', llrbtree.shift)
  end

  def test_pop
    result = @llrbtree.pop
    assert_equal(3, @llrbtree.size)
    assert_equal(%w[d D], result)
    assert_equal(nil, @llrbtree['d'])

    3.times { @llrbtree.pop }
    assert_equal(0, @llrbtree.size)
    assert_equal(nil, @llrbtree.pop)
    @llrbtree.default = 'e'
    assert_equal('e', @llrbtree.pop)

    llrbtree = LLRBTree.new { 'e' }
    assert_equal('e', llrbtree.pop)
  end

  def test_delete
    result = @llrbtree.delete('c')
    assert_equal('C', result)
    assert_equal(3, @llrbtree.size)
    assert_equal(nil, @llrbtree['c'])

    assert_equal(nil, @llrbtree.delete('e'))
    assert_equal('E', @llrbtree.delete('e') { 'E' })
  end

  def test_delete_if
    result = @llrbtree.delete_if { |_key, val| val == 'A' || val == 'B' }
    assert_same(@llrbtree, result)
    assert_equal(LLRBTree['c', 'C', 'd', 'D'], @llrbtree)

    assert_raises(ArgumentError) do
      @llrbtree.delete_if { |key, _val| (key == 'c') || raise(ArgumentError) }
    end
    assert_equal(2, @llrbtree.size)

    assert_raises(TypeError) do
      @llrbtree.delete_if { @llrbtree['e'] = 'E' }
    end
    assert_equal(2, @llrbtree.size)

    @llrbtree.delete_if do
      @llrbtree.each do
        assert_equal(2, @llrbtree.size)
      end
      assert_raises(TypeError) do
        @llrbtree['e'] = 'E'
      end
      true
    end
    assert_equal(0, @llrbtree.size)

    if have_enumerator?
      llrbtree = LLRBTree['b', 'B', 'd', 'D', 'a', 'A', 'c', 'C']
      llrbtree.delete_if.with_index { |(_key, _val), i| i < 2 }
      assert_equal(LLRBTree['c', 'C', 'd', 'D'], llrbtree)
    end
  end

  def test_keep_if
    result = @llrbtree.keep_if { |_key, val| val == 'A' || val == 'B' }
    assert_same(@llrbtree, result)
    assert_equal(LLRBTree['a', 'A', 'b', 'B'], @llrbtree)

    if have_enumerator?
      llrbtree = LLRBTree['b', 'B', 'd', 'D', 'a', 'A', 'c', 'C']
      llrbtree.keep_if.with_index { |(_key, _val), i| i < 2 }
      assert_equal(LLRBTree['a', 'A', 'b', 'B'], llrbtree)
    end
  end

  def test_reject_bang
    result = @llrbtree.reject! { false }
    assert_equal(nil, result)
    assert_equal(4, @llrbtree.size)

    result = @llrbtree.reject! { |_key, val| val == 'A' || val == 'B' }
    assert_same(@llrbtree, result)
    assert_equal(LLRBTree['c', 'C', 'd', 'D'], result)

    if have_enumerator?
      llrbtree = LLRBTree['b', 'B', 'd', 'D', 'a', 'A', 'c', 'C']
      llrbtree.reject!.with_index { |(_key, _val), i| i < 2 }
      assert_equal(LLRBTree['c', 'C', 'd', 'D'], llrbtree)
    end
  end

  def test_reject
    result = @llrbtree.reject { false }
    assert_equal(LLRBTree['a', 'A', 'b', 'B', 'c', 'C', 'd', 'D'], result)
    assert_equal(4, @llrbtree.size)

    result = @llrbtree.reject { |_key, val| val == 'A' || val == 'B' }
    assert_equal(LLRBTree['c', 'C', 'd', 'D'], result)
    assert_equal(4, @llrbtree.size)

    if have_enumerator?
      result = @llrbtree.reject.with_index { |(_key, _val), i| i < 2 }
      assert_equal(LLRBTree['c', 'C', 'd', 'D'], result)
    end
  end

  def test_select_bang
    result = @llrbtree.select! { true }
    assert_equal(nil, result)
    assert_equal(4, @llrbtree.size)

    result = @llrbtree.select! { |_key, val| val == 'A' || val == 'B' }
    assert_same(@llrbtree, result)
    assert_equal(LLRBTree['a', 'A', 'b', 'B'], result)

    if have_enumerator?
      llrbtree = LLRBTree['b', 'B', 'd', 'D', 'a', 'A', 'c', 'C']
      llrbtree.select!.with_index { |(_key, _val), i| i < 2 }
      assert_equal(LLRBTree['a', 'A', 'b', 'B'], llrbtree)
    end
  end

  def test_select
    result = @llrbtree.select { true }
    assert_equal(LLRBTree['a', 'A', 'b', 'B', 'c', 'C', 'd', 'D'], result)
    assert_equal(4, @llrbtree.size)

    result = @llrbtree.select { |_key, val| val == 'A' || val == 'B' }
    assert_equal(LLRBTree['a', 'A', 'b', 'B'], result)
    assert_raises(ArgumentError) { @llrbtree.select('c') }

    if have_enumerator?
      result = @llrbtree.select.with_index { |(_key, _val), i| i < 2 }
      assert_equal(LLRBTree['a', 'A', 'b', 'B'], result)
    end
  end

  def test_values_at
    result = @llrbtree.values_at('d', 'a', 'e')
    assert_equal(['D', 'A', nil], result)
  end

  def test_invert
    assert_equal(LLRBTree['A', 'a', 'B', 'b', 'C', 'c', 'D', 'd'], @llrbtree.invert)
  end

  def test_update
    llrbtree = LLRBTree.new
    llrbtree['e'] = 'E'
    @llrbtree.update(llrbtree)
    assert_equal(LLRBTree['a', 'A', 'b', 'B', 'c', 'C', 'd', 'D', 'e', 'E'], @llrbtree)

    @llrbtree.clear
    @llrbtree['d'] = 'A'
    llrbtree.clear
    llrbtree['d'] = 'B'

    @llrbtree.update(llrbtree) do |key, val1, val2|
      val1 + val2 if key == 'd'
    end
    assert_equal(LLRBTree['d', 'AB'], @llrbtree)

    assert_raises(TypeError) { @llrbtree.update('e') }
  end

  def test_merge
    llrbtree = LLRBTree.new
    llrbtree['e'] = 'E'

    result = @llrbtree.merge(llrbtree)
    assert_equal(LLRBTree['a', 'A', 'b', 'B', 'c', 'C', 'd', 'D', 'e', 'E'], result)

    assert_equal(4, @llrbtree.size)
  end

  def test_flatten
    llrbtree = LLRBTree.new
    llrbtree.readjust { |a, b| a.flatten <=> b.flatten }
    llrbtree[['a']] = ['A']
    llrbtree[[['b']]] = [['B']]
    assert_equal([['a'], ['A'], [['b']], [['B']]], llrbtree.flatten)
    assert_equal([['a'], ['A'], [['b']], [['B']]], llrbtree.flatten(0))
    assert_equal([['a'], ['A'], [['b']], [['B']]], llrbtree.flatten(1))
    assert_equal(['a', 'A', ['b'], ['B']], llrbtree.flatten(2))
    assert_equal(%w[a A b B], llrbtree.flatten(3))

    assert_raises(TypeError) { @llrbtree.flatten('e') }
    assert_raises(ArgumentError) { @llrbtree.flatten(1, 2) }
  end

  def test_has_key
    assert_equal(true,  @llrbtree.key?('a'))
    assert_equal(true,  @llrbtree.key?('b'))
    assert_equal(true,  @llrbtree.key?('c'))
    assert_equal(true,  @llrbtree.key?('d'))
    assert_equal(false, @llrbtree.key?('e'))
  end

  def test_has_value
    assert_equal(true,  @llrbtree.value?('A'))
    assert_equal(true,  @llrbtree.value?('B'))
    assert_equal(true,  @llrbtree.value?('C'))
    assert_equal(true,  @llrbtree.value?('D'))
    assert_equal(false, @llrbtree.value?('E'))
  end

  def test_keys
    assert_equal(%w[a b c d], @llrbtree.keys)
  end

  def test_values
    assert_equal(%w[A B C D], @llrbtree.values)
  end

  def test_to_a
    assert_equal([%w[a A], %w[b B], %w[c C], %w[d D]], @llrbtree.to_a)
  end

  def test_to_hash
    @llrbtree.default = 'e'
    hash = @llrbtree.to_hash
    assert_equal(@llrbtree.to_a.flatten, hash.sort_by { |key, _val| key }.flatten)
    assert_equal('e', hash.default)

    llrbtree = LLRBTree.new { 'e' }
    hash = llrbtree.to_hash
    if hash.respond_to?(:default_proc)
      assert_equal(llrbtree.default_proc, hash.default_proc)
    else
      assert_equal(llrbtree.default_proc, hash.default)
    end
  end

  def test_to_llrbtree
    assert_same(@llrbtree, @llrbtree.to_llrbtree)
  end

  def test_inspect
    %i[to_s inspect].each do |method|
      @llrbtree.default = 'e'
      @llrbtree.readjust { |a, b| a <=> b }
      re = /#<LLRBTree: (\{.*\}), default=(.*), cmp_proc=(.*)>/

      assert_match(re, @llrbtree.send(method))
      match = re.match(@llrbtree.send(method))
      tree, default, cmp_proc = match.to_a[1..-1]
      assert_equal(%({"a"=>"A", "b"=>"B", "c"=>"C", "d"=>"D"}), tree)
      assert_equal(%("e"), default)
      assert_match(/#<Proc:\w+(@#{__FILE__}:\d+)?>/o, cmp_proc)

      llrbtree = LLRBTree.new
      assert_match(re, llrbtree.send(method))
      match = re.match(llrbtree.send(method))
      tree, default, cmp_proc = match.to_a[1..-1]
      assert_equal('{}', tree)
      assert_equal('nil', default)
      assert_equal('nil', cmp_proc)

      next if (method == :to_s) && (RUBY_VERSION < '1.9')

      llrbtree = LLRBTree.new
      llrbtree[llrbtree] = llrbtree
      llrbtree.default = llrbtree
      match = re.match(llrbtree.send(method))
      tree, default, cmp_proc =  match.to_a[1..-1]
      assert_equal('{#<LLRBTree: ...>=>#<LLRBTree: ...>}', tree)
      assert_equal('#<LLRBTree: ...>', default)
      assert_equal('nil', cmp_proc)
    end
  end

  def test_lower_bound
    llrbtree = LLRBTree['a', 'A', 'c', 'C', 'e', 'E']
    assert_equal(%w[c C], llrbtree.lower_bound('c'))
    assert_equal(%w[c C], llrbtree.lower_bound('b'))
    assert_equal(nil, llrbtree.lower_bound('f'))
  end

  def test_upper_bound
    llrbtree = LLRBTree['a', 'A', 'c', 'C', 'e', 'E']
    assert_equal(%w[c C], llrbtree.upper_bound('c'))
    assert_equal(%w[c C], llrbtree.upper_bound('d'))
    assert_equal(nil, llrbtree.upper_bound('Z'))
  end

  def test_bound
    llrbtree = LLRBTree['a', 'A', 'c', 'C', 'e', 'E']
    assert_equal(%w[a A c C], llrbtree.bound('a', 'c').to_a.flatten)
    assert_equal(%w[a A],     llrbtree.bound('a').to_a.flatten)
    assert_equal(%w[c C e E], llrbtree.bound('b', 'f').to_a.flatten)

    assert_equal([], llrbtree.bound('b', 'b').to_a)
    assert_equal([], llrbtree.bound('Y', 'Z').to_a)
    assert_equal([], llrbtree.bound('f', 'g').to_a)
    assert_equal([], llrbtree.bound('f', 'Z').to_a)

    if defined?(Enumerator) && Enumerator.method_defined?(:size)
      assert_equal(2, llrbtree.bound('a', 'c').size)
      assert_equal(1, llrbtree.bound('a').size)
      assert_equal(2, llrbtree.bound('b', 'f').size)

      assert_equal(0, llrbtree.bound('b', 'b').size)
      assert_equal(0, llrbtree.bound('Y', 'Z').size)
      assert_equal(0, llrbtree.bound('f', 'g').size)
      assert_equal(0, llrbtree.bound('f', 'Z').size)
    end
  end

  def test_bound_block
    result = []
    @llrbtree.bound('b', 'c') do |key, _val|
      result.push(key)
    end
    assert_equal(%w[b c], result)

    assert_raises(TypeError) do
      @llrbtree.bound('a', 'd') do
        @llrbtree['e'] = 'E'
      end
    end
    assert_equal(4, @llrbtree.size)

    @llrbtree.bound('b', 'c') do
      @llrbtree.bound('b', 'c') {}
      assert_raises(TypeError) do
        @llrbtree['e'] = 'E'
      end
      break
    end
    assert_equal(4, @llrbtree.size)
  end

  def test_first
    assert_equal(%w[a A], @llrbtree.first)

    llrbtree = LLRBTree.new('e')
    assert_equal('e', llrbtree.first)

    llrbtree = LLRBTree.new { 'e' }
    assert_equal('e', llrbtree.first)
  end

  def test_last
    assert_equal(%w[d D], @llrbtree.last)

    llrbtree = LLRBTree.new('e')
    assert_equal('e', llrbtree.last)

    llrbtree = LLRBTree.new { 'e' }
    assert_equal('e', llrbtree.last)
  end

  def test_readjust
    assert_equal(nil, @llrbtree.cmp_proc)

    @llrbtree.readjust { |a, b| b <=> a }
    assert_equal(%w[d c b a], @llrbtree.keys)
    assert_not_equal(nil, @llrbtree.cmp_proc)

    proc = proc { |a, b| a.to_s <=> b.to_s }
    @llrbtree.readjust(proc)
    assert_equal(%w[a b c d], @llrbtree.keys)
    assert_equal(proc, @llrbtree.cmp_proc)

    @llrbtree[0] = nil
    assert_raises(ArgumentError) { @llrbtree.readjust(nil) }
    assert_equal(5, @llrbtree.size)
    assert_equal(proc, @llrbtree.cmp_proc)

    @llrbtree.delete(0)
    @llrbtree.readjust(nil)
    assert_raises(ArgumentError) { @llrbtree[0] = nil }

    if Symbol.method_defined?(:to_proc)
      llrbtree = LLRBTree['a', 'A', 'B', 'b']
      assert_equal(%w[B b a A], llrbtree.to_a.flatten)
      llrbtree.readjust(:casecmp)
      assert_equal(%w[a A B b], llrbtree.to_a.flatten)
    end

    if RUBY_VERSION >= '1.9.2'
      assert_nothing_raised do
        @llrbtree.readjust(->(a, b) { a <=> b })
        @llrbtree.readjust(->(*a) { a[0] <=> a[1] })
        @llrbtree.readjust(->(a, *b) { a <=> b[0] })
        @llrbtree.readjust(->(a, b, *_c) { a <=> b })
        @llrbtree.readjust(&->(a, b) { a <=> b })
        @llrbtree.readjust(&->(*a) { a[0] <=> a[1] })
        @llrbtree.readjust(&->(a, *b) { a <=> b[0] })
        @llrbtree.readjust(&->(a, b, *_c) { a <=> b })
      end
      assert_raises(TypeError) { @llrbtree.readjust(->(_a) { 1 }) }
      assert_raises(TypeError) { @llrbtree.readjust(->(_a, _b, _c) { 1 }) }
      assert_raises(TypeError) { @llrbtree.readjust(->(_a, _b, _c, *_d) { 1 }) }
      assert_raises(TypeError) { @llrbtree.readjust(&->(_a) { 1 }) }
      assert_raises(TypeError) { @llrbtree.readjust(&->(_a, _b, _c) { 1 }) }
      assert_raises(TypeError) { @llrbtree.readjust(&->(_a, _b, _c, *_d) { 1 }) }
    end

    llrbtree = LLRBTree.new
    key = ['a']
    llrbtree[key] = nil
    llrbtree[['e']] = nil
    key[0] = 'f'

    assert_equal([['f'], ['e']], llrbtree.keys)
    llrbtree.readjust
    assert_equal([['e'], ['f']], llrbtree.keys)

    assert_raises(TypeError) { @llrbtree.readjust('e') }
    assert_raises(ArgumentError) do
      @llrbtree.readjust(proc) { |a, b| a <=> b }
    end
    assert_raises(ArgumentError) { @llrbtree.readjust(proc, proc) }

    llrbtree = LLRBTree[('a'..'z').to_a.zip(('A'..'Z').to_a)]
    assert_nothing_raised do
      llrbtree.readjust do |a, b|
        ObjectSpace.each_object(LLRBTree) do |temp|
          temp.clear if temp.size == llrbtree.size - 1
        end
        a <=> b
      end
    end
  end

  def test_replace
    llrbtree = LLRBTree.new { 'e' }
    llrbtree.readjust { |a, b| a <=> b }
    llrbtree['a'] = 'A'
    llrbtree['e'] = 'E'

    @llrbtree.replace(llrbtree)
    assert_equal(%w[a A e E], @llrbtree.to_a.flatten)
    assert_equal(llrbtree.default, @llrbtree.default)
    assert_equal(llrbtree.cmp_proc, @llrbtree.cmp_proc)

    assert_raises(TypeError) { @llrbtree.replace('e') }
  end

  def test_reverse_each
    result = []
    @llrbtree.reverse_each { |key, val| result.push([key, val]) }
    assert_equal(%w[d D c C b B a A], result.flatten)

    if have_enumerator?
      enumerator = @llrbtree.reverse_each
      assert_equal(%w[d D c C b B a A], enumerator.to_a.flatten)
    end
  end

  def test_marshal
    assert_equal(@llrbtree, Marshal.load(Marshal.dump(@llrbtree)))

    @llrbtree.default = 'e'
    assert_equal(@llrbtree, Marshal.load(Marshal.dump(@llrbtree)))

    assert_raises(TypeError) do
      Marshal.dump(LLRBTree.new { 'e' })
    end

    assert_raises(TypeError) do
      @llrbtree.readjust { |a, b| a <=> b }
      Marshal.dump(@llrbtree)
    end
  end

  def test_modify_in_cmp_proc
    can_clear = false
    @llrbtree.readjust do |a, b|
      @llrbtree.clear if can_clear
      a <=> b
    end
    can_clear = true
    assert_raises(TypeError) { @llrbtree['e'] }
  end

  begin
    require 'pp'

    def test_pp
      assert_equal(%(#<LLRBTree: {}, default=nil, cmp_proc=nil>\n),
                   PP.pp(LLRBTree.new, ''))
      assert_equal(%(#<LLRBTree: {"a"=>"A", "b"=>"B"}, default=nil, cmp_proc=nil>\n),
                   PP.pp(LLRBTree['a', 'A', 'b', 'B'], ''))

      llrbtree = LLRBTree[*('a'..'z').to_a]
      llrbtree.default = 'a'
      llrbtree.readjust { |a, b| a <=> b }
      expected = <<EOS
#<LLRBTree: {"a"=>"b",
  "c"=>"d",
  "e"=>"f",
  "g"=>"h",
  "i"=>"j",
  "k"=>"l",
  "m"=>"n",
  "o"=>"p",
  "q"=>"r",
  "s"=>"t",
  "u"=>"v",
  "w"=>"x",
  "y"=>"z"},
 default="a",
 cmp_proc=#{llrbtree.cmp_proc}>
EOS
      assert_equal(expected, PP.pp(llrbtree, ''))

      llrbtree = LLRBTree.new
      llrbtree[llrbtree] = llrbtree
      llrbtree.default = llrbtree
      expected = <<EOS
#<LLRBTree: {"#<LLRBTree: ...>"=>"#<LLRBTree: ...>"},
 default="#<LLRBTree: ...>",
 cmp_proc=nil>
EOS
      assert_equal(expected, PP.pp(llrbtree, ''))
    end
  rescue LoadError
  end

  def test_insert_and_delete
    tree = LLRBTree.new
    max_num = 100_000
    rand_nums = Hash[*(1..max_num).to_a.shuffle]
    rand_nums.each { |k, v| tree[k] = v }
    assert_equal(rand_nums.keys.size, tree.size)

    rand_nums.values.each do |v|
      assert_equal(nil, tree.delete(v))
    end

    assert_equal(rand_nums.keys.size, tree.size)
    assert_equal(rand_nums.keys.size, tree.count)

    rand_nums.each do |k, v|
      assert_equal(v, tree.delete(k))
    end
    assert_equal(0, tree.size)
    assert_equal(0, tree.count)
  end
end
