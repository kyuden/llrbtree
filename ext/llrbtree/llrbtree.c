/*
 * MIT License
 * Copyright (c) 2002-2013 OZAWA Takuma
 */
#include <ruby.h>
#include <ruby/st.h>
#include "tree.h"

#define LLRBTREE_PROC_DEFAULT FL_USER2
#define HASH_PROC_DEFAULT   FL_USER2

#ifdef RETURN_SIZED_ENUMERATOR
#define HAVE_SIZED_ENUMERATOR
#else
#ifdef RETURN_ENUMERATOR
#define RETURN_SIZED_ENUMERATOR(obj, argc, argv, size_fn) RETURN_ENUMERATOR(obj, argc, argv)
#else
#define RETURN_SIZED_ENUMERATOR(obj, argc, argv, size_fn) ((void)0)
#endif
#endif

#ifndef RARRAY_AREF
#define RARRAY_AREF(a, i) (RARRAY_PTR(a)[i])
#endif

#ifndef RHASH_SET_IFNONE
#define RHASH_SET_IFNONE(h, v) (RHASH(h)->ifnone = (v))
#endif

VALUE LLRBTree;

static ID id_cmp;
static ID id_call;
static ID id_default;
static ID id_flatten_bang;
static ID id_marshal;
static ID id_dump;

typedef struct {
    tree_t* tree;
    VALUE ifnone;
    VALUE cmp_proc;
    int iter_lev;
} llrbtree_t;

#define LLRBTREE(llrbtree) DATA_PTR(llrbtree)
#define TREE(llrbtree) ((llrbtree_t*)LLRBTREE(llrbtree))->tree
#define IFNONE(llrbtree) ((llrbtree_t*)LLRBTREE(llrbtree))->ifnone
#define CMP_PROC(llrbtree) ((llrbtree_t*)LLRBTREE(llrbtree))->cmp_proc
#define ITER_LEV(llrbtree) ((llrbtree_t*)LLRBTREE(llrbtree))->iter_lev

#define TO_KEY(arg) ((const void*)arg)
#define TO_VAL(arg) ((void*)arg)
#define GET_KEY(node) ((VALUE)node->key)
#define GET_VAL(node) ((VALUE)node->data)
#define ASSOC(node) rb_assoc_new(GET_KEY(node), GET_VAL(node))

/*********************************************************************/

static void
llrbtree_free(llrbtree_t* llrbtree)
{
    tree_free(llrbtree->tree);
    xfree(llrbtree->tree);
    xfree(llrbtree);
}

static void
llrbtree_mark(llrbtree_t* llrbtree)
{
    if (llrbtree == NULL) return;

    if (llrbtree->tree != NULL) {
        tree_t* tree = llrbtree->tree;
        node_t* node;
        for (node = tree_first(tree);
             node != NULL;
             node = tree_next(tree, node)) {

            rb_gc_mark(GET_KEY(node));
            rb_gc_mark(GET_VAL(node));
        }
    }
    rb_gc_mark(llrbtree->ifnone);
    rb_gc_mark(llrbtree->cmp_proc);
}

static node_t*
llrbtree_alloc_node(void* context)
{
    return ALLOC(node_t);
}

static void
llrbtree_free_node(node_t* node, void* context)
{
    xfree(node);
}

static void
llrbtree_check_argument_count(const int argc, const int min, const int max)
{
    if (argc < min || argc > max) {
        static const char* const  message = "wrong number of arguments";
        if (min == max) {
            rb_raise(rb_eArgError, "%s (%d for %d)", message, argc, min);
        } else if (max == INT_MAX) {
            rb_raise(rb_eArgError, "%s (%d for %d+)", message, argc, -min - 1);
        } else {
            rb_raise(rb_eArgError, "%s (%d for %d..%d)", message, argc, min, max);
        }
    }
}

static void
llrbtree_check_proc_arity(VALUE proc, const int expected)
{
    if (rb_proc_lambda_p(proc)) {
        const int arity = rb_proc_arity(proc);
        const int min = arity < 0 ? -arity - 1 : arity;
        const int max = arity < 0 ? INT_MAX : arity;
        if (expected < min || expected > max) {
            rb_raise(rb_eTypeError, "proc takes %d arguments", expected);
        }
    }
}

static int
llrbtree_cmp(const void* key1, const void* key2, void* context)
{
    VALUE result;
    if (TYPE(key1) == T_STRING && TYPE(key2) == T_STRING)
        return rb_str_cmp((VALUE)key1, (VALUE)key2);
    result = rb_funcall2((VALUE)key1, id_cmp, 1, (VALUE*)&key2);
    return rb_cmpint(result, (VALUE)key1, (VALUE)key2);
}

static VALUE
llrbtree_user_cmp_ensure(llrbtree_t* llrbtree)
{
    llrbtree->iter_lev--;
    return Qnil;
}

static VALUE
llrbtree_user_cmp_body(VALUE* args)
{
    llrbtree_t* llrbtree = (llrbtree_t*)args[2];
    llrbtree->iter_lev++;
    return rb_funcall2(llrbtree->cmp_proc, id_call, 2, args);
}

static int
llrbtree_user_cmp(const void* key1, const void* key2, void* context)
{
    llrbtree_t* llrbtree = (llrbtree_t*)context;
    VALUE args[3];
    VALUE result;

    args[0] = (VALUE)key1;
    args[1] = (VALUE)key2;
    args[2] = (VALUE)llrbtree;
    result = rb_ensure(llrbtree_user_cmp_body, (VALUE)&args,
                       llrbtree_user_cmp_ensure, (VALUE)llrbtree);
    return rb_cmpint(result, (VALUE)key1, (VALUE)key2);
}

static void
llrbtree_modify(VALUE self)
{
    if (ITER_LEV(self) > 0)
        rb_raise(rb_eTypeError, "can't modify llrbtree during iteration");
    rb_check_frozen(self);
    if (!OBJ_TAINTED(self) && rb_safe_level() >= 4)
        rb_raise(rb_eSecurityError, "Insecure: can't modify llrbtree");
}

static VALUE
llrbtree_alloc(VALUE klass)
{
    tree_t* tree;
    VALUE llrbtree = Data_Wrap_Struct(klass, llrbtree_mark, llrbtree_free, NULL);
    LLRBTREE(llrbtree) = ALLOC(llrbtree_t);
    MEMZERO(LLRBTREE(llrbtree), llrbtree_t, 1);

    tree = ALLOC(tree_t);
    tree_init(tree, llrbtree_cmp, llrbtree_alloc_node, llrbtree_free_node, LLRBTREE(llrbtree));

    TREE(llrbtree) = tree;
    IFNONE(llrbtree) = Qnil;
    CMP_PROC(llrbtree) = Qnil;
    return llrbtree;
}

VALUE llrbtree_aset(VALUE, VALUE, VALUE);
VALUE llrbtree_has_key(VALUE, VALUE);
VALUE llrbtree_update(VALUE, VALUE);
VALUE llrbtree_to_a(VALUE);

/*********************************************************************/

static int
hash_to_llrbtree_i(VALUE key, VALUE value, VALUE llrbtree)
{
    if (key != Qundef)
        llrbtree_aset(llrbtree, key, value);
    return ST_CONTINUE;
}

/*
 *
 */
VALUE
llrbtree_s_create(int argc, VALUE* argv, VALUE klass)
{
    long i;
    VALUE llrbtree;

    if (argc == 1) {
        VALUE temp;

        if (rb_obj_is_kind_of(argv[0], klass)) {
            llrbtree = llrbtree_alloc(klass);
            llrbtree_update(llrbtree, argv[0]);
            return llrbtree;
        }

        temp = rb_check_convert_type(argv[0], T_HASH, "Hash", "to_hash");
        if (!NIL_P(temp)) {
            llrbtree = llrbtree_alloc(klass);
            rb_hash_foreach(temp, hash_to_llrbtree_i, llrbtree);
            return llrbtree;
        }

        temp = rb_check_array_type(argv[0]);
        if (!NIL_P(temp)) {
            llrbtree = llrbtree_alloc(klass);
            for (i = 0; i < RARRAY_LEN(temp); i++) {
                VALUE v = rb_check_array_type(RARRAY_AREF(temp, i));
                if (NIL_P(v)) {
                    rb_warn("wrong element type %s at %ld (expected Array)",
                            rb_obj_classname(RARRAY_AREF(temp, i)), i);
                    continue;
                }
                switch(RARRAY_LEN(v)) {
                case 1:
                    llrbtree_aset(llrbtree, RARRAY_AREF(v, 0), Qnil);
                    break;
                case 2:
                    llrbtree_aset(llrbtree, RARRAY_AREF(v, 0), RARRAY_AREF(v, 1));
                    break;
                default:
                    rb_warn("invalid number of elements (%ld for 1..2)",
                            RARRAY_LEN(v));
                }
            }
            return llrbtree;
        }
    }

    if (argc % 2 != 0)
        rb_raise(rb_eArgError, "odd number of arguments for %s", rb_class2name(klass));

    llrbtree = llrbtree_alloc(klass);
    for (i = 0; i < argc; i += 2)
        llrbtree_aset(llrbtree, argv[i], argv[i + 1]);
    return llrbtree;
}

/*
 *
 */
VALUE
llrbtree_initialize(int argc, VALUE* argv, VALUE self)
{
    llrbtree_modify(self);

    if (rb_block_given_p()) {
        VALUE proc;
        llrbtree_check_argument_count(argc, 0, 0);
        proc = rb_block_proc();
        llrbtree_check_proc_arity(proc, 2);
        IFNONE(self) = proc;
        FL_SET(self, LLRBTREE_PROC_DEFAULT);
    } else {
        llrbtree_check_argument_count(argc, 0, 1);
        if (argc == 1) {
            IFNONE(self) = argv[0];
        }
    }
    return self;
}

/*********************************************************************/

typedef enum {
    NoNodeInserted,
    KeyAllocationFailed,
    InsertionSucceeded
} insert_result_t;

typedef struct {
    tree_t* tree;
    node_t* node;
    insert_result_t result;
} llrbtree_insert_arg_t;

static VALUE
insert_node_body(llrbtree_insert_arg_t* arg)
{
    tree_t* tree = arg->tree;
    node_t* node = arg->node;

    if (tree_insert(tree, node, node->key)) {
        if (TYPE(GET_KEY(node)) == T_STRING) {
            arg->result = KeyAllocationFailed;
            node->key = TO_KEY(rb_str_new4(GET_KEY(node)));
        }
    } else {
        tree->free_node(node, tree->context);
    }
    arg->result = InsertionSucceeded;
    return Qnil;
}

static VALUE
insert_node_ensure(llrbtree_insert_arg_t* arg)
{
    tree_t* tree = arg->tree;
    node_t* node = arg->node;

    switch (arg->result) {
    case InsertionSucceeded:
        break;
    case NoNodeInserted:
        tree->free_node(node, tree->context);
        break;
    case KeyAllocationFailed:
        tree_delete(tree, node->key);
        break;
    }
    return Qnil;
}

static void
llrbtree_insert(VALUE self, VALUE key, VALUE value)
{
    llrbtree_insert_arg_t arg;
    tree_t* tree = TREE(self);
    node_t* node = tree->alloc_node(tree->context);

    node_init(node, TO_VAL(value));
    node->key = TO_KEY(key);

    arg.tree = tree;
    arg.node = node;
    arg.result = NoNodeInserted;

    rb_ensure(insert_node_body, (VALUE)&arg,
              insert_node_ensure, (VALUE)&arg);
}

/*********************************************************************/

/*
 *
 */
VALUE
llrbtree_aset(VALUE self, VALUE key, VALUE value)
{
    llrbtree_modify(self);

    if (tree_isfull(TREE(self))) {
        node_t* node = tree_lookup(TREE(self), TO_KEY(key));
        if (node == NULL)
            rb_raise(rb_eIndexError, "llrbtree full");
        else
            node->data = TO_VAL(value);
        return value;
    }
    llrbtree_insert(self, key, value);
    return value;
}

/*
 *
 */
VALUE
llrbtree_aref(VALUE self, VALUE key)
{
    node_t* node = tree_lookup(TREE(self), TO_KEY(key));
    if (node == NULL)
        return rb_funcall2(self, id_default, 1, &key);
    else
        return GET_VAL(node);
}

/*
 *
 */
VALUE
llrbtree_fetch(int argc, VALUE* argv, VALUE self)
{
    node_t* node;

    llrbtree_check_argument_count(argc, 1, 2);
    if (argc == 2 && rb_block_given_p()) {
        rb_warn("block supersedes default value argument");
    }

    node = tree_lookup(TREE(self), TO_KEY(argv[0]));
    if (node != NULL) {
        return GET_VAL(node);
    }

    if (rb_block_given_p()) {
        return rb_yield(argv[0]);
    }
    if (argc == 1) {
        rb_raise(rb_eIndexError, "key not found");
    }
    return argv[1];
}

/*
 *
 */
VALUE
llrbtree_size(VALUE self)
{
    return ULONG2NUM(TREE(self)->node_count);
}

/*
 *
 */
VALUE
llrbtree_empty_p(VALUE self)
{
    return tree_isempty(TREE(self)) ? Qtrue : Qfalse;
}

/*
 *
 */
VALUE
llrbtree_default(int argc, VALUE* argv, VALUE self)
{
    llrbtree_check_argument_count(argc, 0, 1);
    if (FL_TEST(self, LLRBTREE_PROC_DEFAULT)) {
        if (argc == 0) {
            return Qnil;
        }
        return rb_funcall(IFNONE(self), id_call, 2, self, argv[0]);
    }
    return IFNONE(self);
}

/*
 *
 */
VALUE
llrbtree_set_default(VALUE self, VALUE ifnone)
{
    llrbtree_modify(self);
    IFNONE(self) = ifnone;
    FL_UNSET(self, LLRBTREE_PROC_DEFAULT);
    return ifnone;
}

/*
 *
 */
VALUE
llrbtree_default_proc(VALUE self)
{
    if (FL_TEST(self, LLRBTREE_PROC_DEFAULT))
        return IFNONE(self);
    return Qnil;
}

/*
 *
 */
VALUE
llrbtree_set_default_proc(VALUE self, VALUE proc)
{
    VALUE temp;

    llrbtree_modify(self);
    if (NIL_P(proc)) {
        IFNONE(self) = Qnil;
        FL_UNSET(self, LLRBTREE_PROC_DEFAULT);
        return Qnil;
    }

    temp = rb_check_convert_type(proc, T_DATA, "Proc", "to_proc");
    if (NIL_P(temp)) {
        rb_raise(rb_eTypeError,
                 "wrong default_proc type %s (expected Proc)",
                 rb_obj_classname(proc));
    }
    llrbtree_check_proc_arity(temp, 2);
    IFNONE(self) = temp;
    FL_SET(self, LLRBTREE_PROC_DEFAULT);
    return proc;
}

static VALUE
llrbtree_recursive_equal(VALUE self, VALUE other, int recursive)
{
    tree_t* tree1 = TREE(self);
    tree_t* tree2 = TREE(other);
    node_t* node1;
    node_t* node2;

    if (recursive)
        return Qtrue;
    for (node1 = tree_first(tree1), node2 = tree_first(tree2);
         node1 != NULL && node2 != NULL;
         node1 = tree_next(tree1, node1), node2 = tree_next(tree2, node2)) {

        if (!rb_equal(GET_KEY(node1), GET_KEY(node2)) ||
            !rb_equal(GET_VAL(node1), GET_VAL(node2))) {

            return Qfalse;
        }
    }
    return Qtrue;
}

/*
 *
 */
VALUE
llrbtree_equal(VALUE self, VALUE other)
{
    if (self == other)
        return Qtrue;
    if (!rb_obj_is_kind_of(other, LLRBTree))
        return Qfalse;
    if (TREE(self)->node_count != TREE(other)->node_count ||
        TREE(self)->compare != TREE(other)->compare ||
        CMP_PROC(self) != CMP_PROC(other)) {

        return Qfalse;
    }
    return rb_exec_recursive_paired(llrbtree_recursive_equal, self, other, other);
}

/*********************************************************************/

typedef enum {
    EACH_NEXT, EACH_STOP
} each_return_t;

typedef each_return_t (*each_callback_func)(node_t*, void*);

typedef struct {
    VALUE self;
    each_callback_func func;
    void* arg;
    int reverse;
} llrbtree_each_arg_t;

static VALUE
llrbtree_each_ensure(VALUE self)
{
    ITER_LEV(self)--;
    return Qnil;
}

static VALUE
llrbtree_each_body(llrbtree_each_arg_t* arg)
{
    VALUE self = arg->self;
    tree_t* tree = TREE(self);
    node_t* node;
    node_t* first_node;
    node_t* (*next_func)(tree_t*, node_t*);

    if (arg->reverse) {
        first_node = tree_last(tree);
        next_func = tree_prev;
    } else {
        first_node = tree_first(tree);
        next_func = tree_next;
    }

    ITER_LEV(self)++;
    for (node = first_node;
         node != NULL;
         node = next_func(tree, node)) {

        if (arg->func(node, arg->arg) == EACH_STOP)
            break;
    }
    return self;
}

static VALUE
llrbtree_for_each(VALUE self, each_callback_func func, void* arg)
{
    llrbtree_each_arg_t each_arg;
    each_arg.self = self;
    each_arg.func = func;
    each_arg.arg = arg;
    each_arg.reverse = 0;
    return rb_ensure(llrbtree_each_body, (VALUE)&each_arg,
                     llrbtree_each_ensure, self);
}

static VALUE
llrbtree_reverse_for_each(VALUE self, each_callback_func func, void* arg)
{
    llrbtree_each_arg_t each_arg;
    each_arg.self = self;
    each_arg.func = func;
    each_arg.arg = arg;
    each_arg.reverse = 1;
    return rb_ensure(llrbtree_each_body, (VALUE)&each_arg,
                     llrbtree_each_ensure, self);
}

/*********************************************************************/

static each_return_t
each_pair_i(node_t* node, void* arg)
{
    rb_yield(ASSOC(node));
    return EACH_NEXT;
}

/*
 * call-seq:
 *   llrbtree.each      {|key, value| block} => llrbtree
 *   llrbtree.each_pair {|key, value| block} => llrbtree
 *   llrbtree.each                           => enumerator
 *   llrbtree.each_pair                      => enumerator
 *
 * Calls block once for each key in order, passing the key-value pair
 * as parameters.
 *
 * Returns an enumerator if no block is given.
 */
VALUE
llrbtree_each_pair(VALUE self)
{
    RETURN_SIZED_ENUMERATOR(self, 0, NULL, llrbtree_size);
    return llrbtree_for_each(self, each_pair_i, NULL);
}

static each_return_t
each_key_i(node_t* node, void* arg)
{
    rb_yield(GET_KEY(node));
    return EACH_NEXT;
}

/*
 * call-seq:
 *   llrbtree.each_key {|key| block} => llrbtree
 *   llrbtree.each_key               => enumerator
 *
 * Calls block once for each key in order, passing the key as a
 * parameter.
 *
 * Returns an enumerator if no block is given.
 */
VALUE
llrbtree_each_key(VALUE self)
{
    RETURN_SIZED_ENUMERATOR(self, 0, NULL, llrbtree_size);
    return llrbtree_for_each(self, each_key_i, NULL);
}

static each_return_t
each_value_i(node_t* node, void* arg)
{
    rb_yield(GET_VAL(node));
    return EACH_NEXT;
}

/*
 * call-seq:
 *   llrbtree.each_value {|value| block} => llrbtree
 *   llrbtree.each_value                 => enumerator
 *
 * Calls block once for each key in order, passing the value as a
 * parameter.
 *
 * Returns an enumerator if no block is given.
 */
VALUE
llrbtree_each_value(VALUE self)
{
    RETURN_SIZED_ENUMERATOR(self, 0, NULL, llrbtree_size);
    return llrbtree_for_each(self, each_value_i, NULL);
}

/*
 * call-seq:
 *   llrbtree.reverse_each {|key, value| block} => llrbtree
 *   llrbtree.reverse_each                      => enumerator
 *
 * Calls block once for each key in reverse order, passing the
 * key-value pair as parameters.
 *
 * Returns an enumerator if no block is given.
 */
VALUE
llrbtree_reverse_each(VALUE self)
{
    RETURN_SIZED_ENUMERATOR(self, 0, NULL, llrbtree_size);
    return llrbtree_reverse_for_each(self, each_pair_i, NULL);
}

static each_return_t
aset_i(node_t* node, void* self)
{
    llrbtree_aset((VALUE)self, GET_KEY(node), GET_VAL(node));
    return EACH_NEXT;
}

static void
copy_tree(VALUE src, VALUE dest, compare_t cmp_func, VALUE cmp_proc)
{
    VALUE temp = llrbtree_alloc(CLASS_OF(dest));
    rb_obj_hide(temp);
    TREE(temp)->compare = cmp_func;
    CMP_PROC(temp) = cmp_proc;

    llrbtree_for_each(src, aset_i, (void*)temp);
    {
        tree_t* t = TREE(temp);
        TREE(temp) = TREE(dest);
        TREE(dest) = t;
    }
    llrbtree_free(LLRBTREE(temp));
    LLRBTREE(temp) = NULL;
    rb_gc_force_recycle(temp);

    TREE(dest)->context = LLRBTREE(dest);
    CMP_PROC(dest) = cmp_proc;
}

/*
 *
 */
VALUE
llrbtree_initialize_copy(VALUE self, VALUE other)
{
    llrbtree_modify(self);

    if (self == other)
        return self;
    if (!rb_obj_is_kind_of(other, CLASS_OF(self))) {
        rb_raise(rb_eTypeError, "wrong argument type %s (expected %s)",
                 rb_obj_classname(other),
                 rb_obj_classname(self));
    }

    copy_tree(other, self, TREE(other)->compare, CMP_PROC(other));

    IFNONE(self) = IFNONE(other);
    if (FL_TEST(other, LLRBTREE_PROC_DEFAULT))
        FL_SET(self, LLRBTREE_PROC_DEFAULT);
    else
        FL_UNSET(self, LLRBTREE_PROC_DEFAULT);
    return self;
}

/*
 *
 */
VALUE
llrbtree_values_at(int argc, VALUE* argv, VALUE self)
{
    long i;
    VALUE ary = rb_ary_new2(argc);

    for (i = 0; i < argc; i++)
        rb_ary_push(ary, llrbtree_aref(self, argv[i]));
    return ary;
}

static each_return_t
key_i(node_t* node, void* args_)
{
    VALUE* args = (VALUE*)args_;
    if (rb_equal(GET_VAL(node), args[1])) {
        args[0] = GET_KEY(node);
        return EACH_STOP;
    }
    return EACH_NEXT;
}

/*
 *
 */
VALUE
llrbtree_key(VALUE self, VALUE value)
{
    VALUE args[2];
    args[0] = Qnil;
    args[1] = value;
    llrbtree_for_each(self, key_i, &args);
    return args[0];
}

/*
 *
 */
VALUE
llrbtree_index(VALUE self, VALUE value)
{
    const char* classname = rb_class2name(LLRBTree);
    rb_warn("%s#index is deprecated; use %s#key", classname, classname);
    return llrbtree_key(self, value);
}

/*
 *
 */
VALUE
llrbtree_clear(VALUE self)
{
    llrbtree_modify(self);
    tree_free(TREE(self));
    return self;
}

/*
 *
 */
VALUE
llrbtree_delete(VALUE self, VALUE key)
{
    tree_t* tree = TREE(self);
    void* deleted_data = NULL;

    llrbtree_modify(self);
    deleted_data = tree_delete(tree, TO_KEY(key));
    if (deleted_data == NULL)
        return rb_block_given_p() ? rb_yield(key) : Qnil;

    return (VALUE)deleted_data;
}

/*********************************************************************/

typedef struct node_list_t_ {
    struct node_list_t_* prev;
    node_t* node;
} node_list_t;

typedef struct {
    VALUE self;
    node_list_t* list;
    int raised;
    int if_true;
} llrbtree_remove_if_arg_t;

static VALUE
llrbtree_remove_if_ensure(llrbtree_remove_if_arg_t* arg)
{
    tree_t* tree = TREE(arg->self);
    node_list_t* list = arg->list;

    if (--ITER_LEV(arg->self) == 0) {
        while (list != NULL) {
            node_list_t* l = list;
            if (!arg->raised)
                tree_delete(tree, l->node->key);
            list = l->prev;
            xfree(l);
        }
    }
    return Qnil;
}

static VALUE
llrbtree_remove_if_body(llrbtree_remove_if_arg_t* arg)
{
    VALUE self = arg->self;
    tree_t* tree = TREE(self);
    node_t* node;

    arg->raised = 1;
    ITER_LEV(self)++;
    for (node = tree_first(tree);
         node != NULL;
         node = tree_next(tree, node)) {

        VALUE key = GET_KEY(node);
        VALUE value = GET_VAL(node);
        if (RTEST(rb_yield_values(2, key, value)) == arg->if_true) {
            node_list_t* l = ALLOC(node_list_t);
            l->node = node;
            l->prev = arg->list;
            arg->list = l;
        }
    }
    arg->raised = 0;
    return self;
}

static VALUE
llrbtree_remove_if(VALUE self, const int if_true)
{
    llrbtree_remove_if_arg_t arg;

    RETURN_SIZED_ENUMERATOR(self, 0, NULL, llrbtree_size);
    llrbtree_modify(self);
    arg.self = self;
    arg.list = NULL;
    arg.if_true = if_true;
    return rb_ensure(llrbtree_remove_if_body, (VALUE)&arg,
                     llrbtree_remove_if_ensure, (VALUE)&arg);
}

/*********************************************************************/

/*
 *
 */
VALUE
llrbtree_delete_if(VALUE self)
{
    return llrbtree_remove_if(self, 1);
}

/*
 *
 */
VALUE
llrbtree_keep_if(VALUE self)
{
    return llrbtree_remove_if(self, 0);
}

/*
 *
 */
VALUE
llrbtree_reject_bang(VALUE self)
{
    node_count_t count;

    RETURN_SIZED_ENUMERATOR(self, 0, NULL, llrbtree_size);
    count = TREE(self)->node_count;
    llrbtree_delete_if(self);
    if (count == TREE(self)->node_count)
        return Qnil;
    return self;
}

/*
 *
 */
VALUE
llrbtree_select_bang(VALUE self)
{
    node_count_t count;

    RETURN_SIZED_ENUMERATOR(self, 0, NULL, llrbtree_size);
    count = TREE(self)->node_count;
    llrbtree_keep_if(self);
    if (count == TREE(self)->node_count)
        return Qnil;
    return self;

}

/*********************************************************************/

typedef struct {
    VALUE result;
    int if_true;
} llrbtree_select_if_arg_t;

static each_return_t
select_i(node_t* node, void* arg_)
{
    VALUE key = GET_KEY(node);
    VALUE value = GET_VAL(node);
    llrbtree_select_if_arg_t* arg = arg_;

    if (RTEST(rb_yield_values(2, key, value)) == arg->if_true) {
        llrbtree_aset(arg->result, key, value);
    }
    return EACH_NEXT;
}

static VALUE
llrbtree_select_if(VALUE self, const int if_true)
{
    llrbtree_select_if_arg_t arg;

    RETURN_SIZED_ENUMERATOR(self, 0, NULL, llrbtree_size);
    arg.result = llrbtree_alloc(CLASS_OF(self));
    arg.if_true = if_true;
    llrbtree_for_each(self, select_i, &arg);
    return arg.result;
}

/*********************************************************************/

/*
 *
 */
VALUE
llrbtree_reject(VALUE self)
{
    return llrbtree_select_if(self, 0);
}

/*
 *
 */
VALUE
llrbtree_select(VALUE self)
{
    return llrbtree_select_if(self, 1);
}

static VALUE
llrbtree_shift_pop(VALUE self, const int shift)
{
    tree_t* tree = TREE(self);
    node_t* node;
    VALUE assoc;

    llrbtree_modify(self);

    if (tree_isempty(tree))
        return rb_funcall(self, id_default, 1, Qnil);

    if (shift)
        node = tree_last(tree);
    else
        node = tree_first(tree);
    assoc = ASSOC(node);
    tree_delete(tree, node->key);
    return assoc;
}

/*
 * call-seq:
 *   llrbtree.shift => array or object or nil
 *
 * Removes the first (that is, the smallest) key-value pair and
 * returns it.
 */
VALUE
llrbtree_shift(VALUE self)
{
    return llrbtree_shift_pop(self, 0);
}

/*
 * call-seq:
 *   llrbtree.pop => array or object or nil
 *
 * Removes the last (that is, the greatest) key-value pair and returns
 * it.
 */
VALUE
llrbtree_pop(VALUE self)
{
    return llrbtree_shift_pop(self, 1);
}

static each_return_t
invert_i(node_t* node, void* llrbtree)
{
    llrbtree_aset((VALUE)llrbtree, GET_VAL(node), GET_KEY(node));
    return EACH_NEXT;
}

/*
 *
 */
VALUE
llrbtree_invert(VALUE self)
{
    VALUE llrbtree = llrbtree_alloc(CLASS_OF(self));
    llrbtree_for_each(self, invert_i, (void*)llrbtree);
    return llrbtree;
}

static each_return_t
update_block_i(node_t* node, void* self_)
{
    VALUE self = (VALUE)self_;
    VALUE key = GET_KEY(node);
    VALUE value = GET_VAL(node);

    if (llrbtree_has_key(self, key))
        value = rb_yield_values(3, key, llrbtree_aref(self, key), value);
    llrbtree_aset(self, key, value);
    return EACH_NEXT;
}

/*
 *
 */
VALUE
llrbtree_update(VALUE self, VALUE other)
{
    llrbtree_modify(self);

    if (self == other)
        return self;
    if (!rb_obj_is_kind_of(other, CLASS_OF(self))) {
        rb_raise(rb_eTypeError, "wrong argument type %s (expected %s)",
                 rb_obj_classname(other),
                 rb_obj_classname(self));
    }

    if (rb_block_given_p())
        llrbtree_for_each(other, update_block_i, (void*)self);
    else
        llrbtree_for_each(other, aset_i, (void*)self);
    return self;
}

/*
 *
 */
VALUE
llrbtree_merge(VALUE self, VALUE other)
{
    return llrbtree_update(rb_obj_dup(self), other);
}

static each_return_t
to_flat_ary_i(node_t* node, void* ary)
{
    rb_ary_push((VALUE)ary, GET_KEY(node));
    rb_ary_push((VALUE)ary, GET_VAL(node));
    return EACH_NEXT;
}

/*
 *
 */
VALUE
llrbtree_flatten(int argc, VALUE* argv, VALUE self)
{
    VALUE ary;

    llrbtree_check_argument_count(argc, 0, 1);
    ary = rb_ary_new2(TREE(self)->node_count * 2);
    llrbtree_for_each(self, to_flat_ary_i, (void*)ary);
    if (argc == 1) {
        const int level = NUM2INT(argv[0]) - 1;
        if (level > 0) {
            argv[0] = INT2FIX(level);
            rb_funcall2(ary, id_flatten_bang, argc, argv);
        }
    }
    return ary;
}

/*
 *
 */
VALUE
llrbtree_has_key(VALUE self, VALUE key)
{
    return tree_lookup(TREE(self), TO_KEY(key)) == NULL ? Qfalse : Qtrue;
}

static each_return_t
has_value_i(node_t* node, void* args_)
{
    VALUE* args = (VALUE*)args_;
    if (rb_equal(GET_VAL(node), args[1])) {
        args[0] = Qtrue;
        return EACH_STOP;
    }
    return EACH_NEXT;
}

/*
 *
 */
VALUE
llrbtree_has_value(VALUE self, VALUE value)
{
    VALUE args[2];
    args[0] = Qfalse;
    args[1] = value;
    llrbtree_for_each(self, has_value_i, &args);
    return args[0];
}

static each_return_t
keys_i(node_t* node, void* ary)
{
    rb_ary_push((VALUE)ary, GET_KEY(node));
    return EACH_NEXT;
}

/*
 *
 */
VALUE
llrbtree_keys(VALUE self)
{
    VALUE ary = rb_ary_new2(TREE(self)->node_count);
    llrbtree_for_each(self, keys_i, (void*)ary);
    return ary;
}

static each_return_t
values_i(node_t* node, void* ary)
{
    rb_ary_push((VALUE)ary, GET_VAL(node));
    return EACH_NEXT;
}

/*
 *
 */
VALUE
llrbtree_values(VALUE self)
{
    VALUE ary = rb_ary_new2(TREE(self)->node_count);
    llrbtree_for_each(self, values_i, (void*)ary);
    return ary;
}

static each_return_t
to_a_i(node_t* node, void* ary)
{
    rb_ary_push((VALUE)ary, ASSOC(node));
    return EACH_NEXT;
}

/*
 *
 */
VALUE
llrbtree_to_a(VALUE self)
{
    VALUE ary = rb_ary_new2(TREE(self)->node_count);
    llrbtree_for_each(self, to_a_i, (void*)ary);
    OBJ_INFECT(ary, self);
    return ary;
}

static each_return_t
to_hash_i(node_t* node, void* hash)
{
    rb_hash_aset((VALUE)hash, GET_KEY(node), GET_VAL(node));
    return EACH_NEXT;
}

/*
 *
 */
VALUE
llrbtree_to_hash(VALUE self)
{
    VALUE hash;
    hash = rb_hash_new();
    llrbtree_for_each(self, to_hash_i, (void*)hash);
    RHASH_SET_IFNONE(hash, IFNONE(self));
    if (FL_TEST(self, LLRBTREE_PROC_DEFAULT))
        FL_SET(hash, HASH_PROC_DEFAULT);
    OBJ_INFECT(hash, self);
    return hash;
}

/*
 *
 */
VALUE
llrbtree_to_llrbtree(VALUE self)
{
    return self;
}

static VALUE
llrbtree_begin_inspect(VALUE self)
{
    VALUE result = rb_str_new2("#<");
    rb_str_cat2(result, rb_obj_classname(self));
    rb_str_cat2(result, ": ");
    return result;
}

static each_return_t
inspect_i(node_t* node, void* result_)
{
    VALUE result = (VALUE)result_;
    VALUE str;

    if (RSTRING_PTR(result)[0] == '-')
        RSTRING_PTR(result)[0] = '#';
    else
        rb_str_cat2(result, ", ");

    str = rb_inspect(GET_KEY(node));
    rb_str_append(result, str);
    OBJ_INFECT(result, str);

    rb_str_cat2(result, "=>");

    str = rb_inspect(GET_VAL(node));
    rb_str_append(result, str);
    OBJ_INFECT(result, str);

    return EACH_NEXT;
}

static VALUE
inspect_llrbtree(VALUE self, VALUE result)
{
    VALUE str;

    rb_str_cat2(result, "{");
    RSTRING_PTR(result)[0] = '-';
    llrbtree_for_each(self, inspect_i, (void*)result);
    RSTRING_PTR(result)[0] = '#';
    rb_str_cat2(result, "}");

    str = rb_inspect(IFNONE(self));
    rb_str_cat2(result, ", default=");
    rb_str_append(result, str);
    OBJ_INFECT(result, str);

    str = rb_inspect(CMP_PROC(self));
    rb_str_cat2(result, ", cmp_proc=");
    rb_str_append(result, str);
    OBJ_INFECT(result, str);

    rb_str_cat2(result, ">");
    OBJ_INFECT(result, self);
    return result;
}

static VALUE
llrbtree_inspect_recursive(VALUE self, VALUE arg, int recursive)
{
    VALUE str = llrbtree_begin_inspect(self);
    if (recursive)
        return rb_str_cat2(str, "...>");
    return inspect_llrbtree(self, str);
}

/*
 *
 */
VALUE
llrbtree_inspect(VALUE self)
{
    return rb_exec_recursive(llrbtree_inspect_recursive, self, Qnil);
}

/*
 * call-seq:
 *   llrbtree.lower_bound(key) => array or nil
 *
 * Retruns the key-value pair corresponding to the lowest key that is
 * equal to or greater than the given key (inside the lower
 * boundary). If there is no such key, returns nil.
 *
 *   llrbtree = LLRBTree["az", 10, "ba", 20]
 *   llrbtree.lower_bound("ba") # => ["ba", 20]
 *
 *   # "ba" is the lowest key that is greater than "b"
 *   llrbtree.lower_bound("b") # => ["ba", 20]
 *
 *   # no key that is equal to or greater than "c"
 *   llrbtree.lower_bound("c") # => nil
 */
VALUE
llrbtree_lower_bound(VALUE self, VALUE key)
{
    node_t* node = tree_lower_bound(TREE(self), TO_KEY(key));
    if (node == NULL)
        return Qnil;
    return ASSOC(node);
}

/*
 * call-seq:
 *   llrbtree.upper_bound(key) => array or nil
 *
 * Retruns the key-value pair corresponding to the greatest key that
 * is equal to or lower than the given key (inside the upper
 * boundary). If there is no such key, returns nil.
 *
 *   llrbtree = LLRBTree["az", 10, "ba", 20]
 *   llrbtree.upper_bound("ba") # => ["ba", 20]
 *
 *   # "az" is the greatest key that is lower than "b"
 *   llrbtree.upper_bound("b") # => ["az", 10]
 *
 *   # no key that is equal to or lower than "a"
 *   llrbtree.upper_bound("a") # => nil
 */
VALUE
llrbtree_upper_bound(VALUE self, VALUE key)
{
    node_t* node = tree_upper_bound(TREE(self), TO_KEY(key));
    if (node == NULL)
        return Qnil;
    return ASSOC(node);
}

/*********************************************************************/

typedef struct {
    VALUE self;
    node_t* lower_node;
    node_t* upper_node;
    VALUE result;
} llrbtree_bound_arg_t;

static VALUE
llrbtree_bound_body(llrbtree_bound_arg_t* arg)
{
    VALUE self = arg->self;
    tree_t* tree = TREE(self);
    node_t* lower_node = arg->lower_node;
    node_t* upper_node = arg->upper_node;
    const int block_given = rb_block_given_p();
    VALUE result = arg->result;
    node_t* node;

    ITER_LEV(self)++;
    for (node = lower_node;
         node != NULL;
         node = tree_next(tree, node)) {

        if (block_given)
            rb_yield_values(2, GET_KEY(node), GET_VAL(node));
        else
            rb_ary_push(result, ASSOC(node));
        if (node == upper_node)
            break;
    }
    return result;
}

#ifdef HAVE_SIZED_ENUMERATOR
static VALUE
llrbtree_bound_size(VALUE self, VALUE args)
{
    VALUE key1 = RARRAY_AREF(args, 0);
    VALUE key2 = RARRAY_AREF(args, RARRAY_LEN(args) - 1);
    node_t* lower_node = tree_lower_bound(TREE(self), TO_KEY(key1));
    node_t* upper_node = tree_upper_bound(TREE(self), TO_KEY(key2));
    node_count_t count = 0;
    node_t* node;

    if (lower_node == NULL || upper_node == NULL ||
        TREE(self)->compare(lower_node->key,
                                 upper_node->key,
                                 TREE(self)->context) > 0) {
        return INT2FIX(0);
    }

    for (node = lower_node;
         node != NULL;
         node = tree_next(TREE(self), node)) {

        count++;
        if (node == upper_node) {
            break;
        }
    }
    return ULONG2NUM(count);
}
#endif

/*********************************************************************/

/*
 * call-seq:
 *   llrbtree.bound(key1, key2 = key1) {|key, value| block} => llrbtree
 *   llrbtree.bound(key1, key2 = key1)                      => enumerator
 *
 * Calls block once for each key between the result of
 * llrbtree.lower_bound(key1) and llrbtree.upper_bound(key2) in order,
 * passing the key-value pair as parameters. If the lower bound
 * exceeds the upper bound, block is not called.
 *
 * Returns an enumerator if no block is given.
 *
 *   llrbtree = LLRBTree["a", "A", "b", "B", "c", "C"]
 *   llrbtree.bound("a").to_a # => [["a", "A"]]
 *   llrbtree.bound("b", "d").to_a # => [["b", "B"], ["c", "C"]]
 *
 */
VALUE
llrbtree_bound(int argc, VALUE* argv, VALUE self)
{
    tree_t* tree = TREE(self);
    node_t* lower_node;
    node_t* upper_node;
    VALUE result;

    llrbtree_check_argument_count(argc, 1, 2);

    RETURN_SIZED_ENUMERATOR(self, argc, argv, llrbtree_bound_size);

    lower_node = tree_lower_bound(tree, TO_KEY(argv[0]));
    upper_node = tree_upper_bound(tree, TO_KEY(argv[argc - 1]));
    result = rb_block_given_p() ? self : rb_ary_new();

    if (lower_node == NULL || upper_node == NULL ||
        TREE(self)->compare(lower_node->key,
                                 upper_node->key,
                                 TREE(self)->context) > 0) {
        return result;
    } else {
        llrbtree_bound_arg_t arg;
        arg.self = self;
        arg.lower_node = lower_node;
        arg.upper_node = upper_node;
        arg.result = result;
        return rb_ensure(llrbtree_bound_body, (VALUE)&arg,
                         llrbtree_each_ensure, self);
    }
}

static VALUE
llrbtree_first_last(VALUE self, const int first)
{
    tree_t* tree = TREE(self);
    node_t* node;

    if (tree_isempty(tree))
        return rb_funcall(self, id_default, 1, Qnil);

    if (first)
        node = tree_first(tree);
    else
        node = tree_last(tree);
    return ASSOC(node);
}

/*
 * call-seq:
 *   llrbtree.first => array or object or nil
 *
 * Returns the first (that is, the smallest) key-value pair.
 */
VALUE
llrbtree_first(VALUE self)
{
    return llrbtree_first_last(self, 1);
}

/*
 * call-seq:
 *   llrbtree.last => array or object or nil
 *
 * Returns the last (that is, the greatest) key-value pair.
 */
VALUE
llrbtree_last(VALUE self)
{
    return llrbtree_first_last(self, 0);
}

/*
 * call-seq:
 *   llrbtree.readjust                      => llrbtree
 *   llrbtree.readjust(nil)                 => llrbtree
 *   llrbtree.readjust(proc)                => llrbtree
 *   llrbtree.readjust {|key1, key2| block} => llrbtree
 *
 * Sets a proc to compare keys and readjusts elements using the given
 * block or a Proc object given as an argument. The block takes two
 * keys and returns a negative integer, 0, or a positive integer as
 * the first argument is less than, equal to, or greater than the
 * second one. If no block is given, just readjusts elements using the
 * current comparison block. If nil is given as an argument the
 * default comparison block that uses the <=> method is set.
 *
 *   llrbtree = LLRBTree["a", 10, "b", 20]
 *   llrbtree.readjust {|a, b| b <=> a }
 *   llrbtree.first # => ["b", 20]
 *
 *   llrbtree.readjust(nil)
 *   llrbtree.first # => ["a", 10]
 */
VALUE
llrbtree_readjust(int argc, VALUE* argv, VALUE self)
{
    compare_t cmp_func = NULL;
    VALUE cmp_proc = Qnil;

    llrbtree_modify(self);

    if (rb_block_given_p()) {
        llrbtree_check_argument_count(argc, 0, 0);
        cmp_func = llrbtree_user_cmp;
        cmp_proc = rb_block_proc();
        llrbtree_check_proc_arity(cmp_proc, 2);
    } else {
        llrbtree_check_argument_count(argc, 0, 1);
        if (argc == 0) {
            cmp_func = TREE(self)->compare;
            cmp_proc = CMP_PROC(self);
        } else {
            if (NIL_P(argv[0])) {
                cmp_func = llrbtree_cmp;
                cmp_proc = Qnil;
            } else {
                VALUE proc = rb_check_convert_type(argv[0], T_DATA, "Proc", "to_proc");
                if (NIL_P(proc)) {
                    rb_raise(rb_eTypeError,
                             "wrong cmp_proc type %s (expected Proc)",
                             rb_obj_classname(argv[0]));
                }
                cmp_func = llrbtree_user_cmp;
                cmp_proc = proc;
                llrbtree_check_proc_arity(cmp_proc, 2);
            }
        }
    }

    if (tree_isempty(TREE(self))) {
        TREE(self)->compare = cmp_func;
        CMP_PROC(self) = cmp_proc;
        return self;
    }
    copy_tree(self, self, cmp_func, cmp_proc);
    return self;
}

/*
 * call-seq:
 *   llrbtree.cmp_proc => proc or nil
 *
 * Returns the comparison block that is set by
 * LLRBTree#readjust. If the default comparison block is set,
 * returns nil.
 */
VALUE
llrbtree_cmp_proc(VALUE self)
{
    return CMP_PROC(self);
}

/*********************************************************************/

static ID id_breakable;
static ID id_comma_breakable;
static ID id_group;
static ID id_object_group;
static ID id_pp;
static ID id_text;

static VALUE
pp_group(VALUE args_)
{
    VALUE* args = (VALUE*)args_;
    return rb_funcall(args[0], id_group, 3, args[1], args[2], args[3]);
}

typedef struct {
    VALUE pp;
    node_t* node;
} pp_pair_arg_t;

static VALUE
pp_value(VALUE nil, pp_pair_arg_t* pair_arg)
{
    VALUE pp = pair_arg->pp;
    rb_funcall(pp, id_breakable, 1, rb_str_new(NULL, 0));
    return rb_funcall(pp, id_pp, 1, GET_VAL(pair_arg->node));
}

static VALUE
pp_pair(VALUE nil, pp_pair_arg_t* pair_arg)
{
    VALUE pp = pair_arg->pp;
    VALUE group_args[4];
    group_args[0] = pp;
    group_args[1] = INT2FIX(1);
    group_args[2] = rb_str_new(NULL, 0);
    group_args[3] = rb_str_new(NULL, 0);

    rb_funcall(pp, id_pp, 1, GET_KEY(pair_arg->node));
    rb_funcall(pp, id_text, 1, rb_str_new2("=>"));
    return rb_iterate(pp_group, (VALUE)&group_args, pp_value, (VALUE)pair_arg);
}

typedef struct {
    VALUE pp;
    int first;
} pp_each_pair_arg_t;

static each_return_t
pp_each_pair_i(node_t* node, void* each_pair_arg_)
{
    pp_each_pair_arg_t* each_pair_arg = (pp_each_pair_arg_t*)each_pair_arg_;
    VALUE group_args[4];
    pp_pair_arg_t pair_arg;

    if (each_pair_arg->first) {
        each_pair_arg->first = 0;
    } else {
        rb_funcall(each_pair_arg->pp, id_comma_breakable, 0);
    }

    group_args[0] = each_pair_arg->pp;
    group_args[1] = INT2FIX(0);
    group_args[2] = rb_str_new(NULL, 0);
    group_args[3] = rb_str_new(NULL, 0);

    pair_arg.pp = each_pair_arg->pp;
    pair_arg.node = node;

    rb_iterate(pp_group, (VALUE)&group_args, pp_pair, (VALUE)&pair_arg);
    return EACH_NEXT;
}

typedef struct {
    VALUE pp;
    VALUE llrbtree;
} pp_llrbtree_arg_t;

static VALUE
pp_each_pair(VALUE nil, pp_llrbtree_arg_t* llrbtree_arg)
{
    pp_each_pair_arg_t each_pair_arg;
    each_pair_arg.pp = llrbtree_arg->pp;
    each_pair_arg.first = 1;
    return llrbtree_for_each(llrbtree_arg->llrbtree, pp_each_pair_i, &each_pair_arg);
}

static VALUE
pp_llrbtree(VALUE nil, pp_llrbtree_arg_t* llrbtree_arg)
{
    VALUE pp = llrbtree_arg->pp;
    VALUE llrbtree = llrbtree_arg->llrbtree;

    VALUE group_args[4];
    group_args[0] = pp;
    group_args[1] = INT2FIX(1);
    group_args[2] = rb_str_new2("{");
    group_args[3] = rb_str_new2("}");

    rb_funcall(pp, id_text, 1, rb_str_new2(": "));
    rb_iterate(pp_group, (VALUE)&group_args, pp_each_pair, (VALUE)llrbtree_arg);
    rb_funcall(pp, id_comma_breakable, 0);
    rb_funcall(pp, id_text, 1, rb_str_new2("default="));
    rb_funcall(pp, id_pp, 1, IFNONE(llrbtree));
    rb_funcall(pp, id_comma_breakable, 0);
    rb_funcall(pp, id_text, 1, rb_str_new2("cmp_proc="));
    return rb_funcall(pp, id_pp, 1, CMP_PROC(llrbtree));
}

static VALUE
pp_llrbtree_group(VALUE arg_)
{
    pp_llrbtree_arg_t* arg = (pp_llrbtree_arg_t*)arg_;
    return rb_funcall(arg->pp, id_object_group, 1, arg->llrbtree);
}

/*********************************************************************/

/* :nodoc:
 *
 */
VALUE
llrbtree_pretty_print(VALUE self, VALUE pp)
{
    pp_llrbtree_arg_t arg;
    arg.llrbtree = self;
    arg.pp = pp;
    return rb_iterate(pp_llrbtree_group, (VALUE)&arg, pp_llrbtree, (VALUE)&arg);
}

/* :nodoc:
 *
 */
VALUE
llrbtree_pretty_print_cycle(VALUE self, VALUE pp)
{
    return rb_funcall(pp, id_pp, 1, llrbtree_inspect_recursive(self, Qnil, 1));
}

/*********************************************************************/

/* :nodoc:
 *
 */
VALUE
llrbtree_dump(VALUE self, VALUE limit)
{
    VALUE ary;
    VALUE result;

    if (FL_TEST(self, LLRBTREE_PROC_DEFAULT))
        rb_raise(rb_eTypeError, "can't dump llrbtree with default proc");
    if (CMP_PROC(self) != Qnil)
        rb_raise(rb_eTypeError, "can't dump llrbtree with comparison proc");

    ary = rb_ary_new2(TREE(self)->node_count * 2 + 1);
    llrbtree_for_each(self, to_flat_ary_i, (void*)ary);
    rb_ary_push(ary, IFNONE(self));

    result = rb_funcall(rb_const_get(rb_cObject, id_marshal), id_dump, 2, ary, limit);
    rb_ary_resize(ary, 0);
    return result;
}

/* :nodoc:
 *
 */
VALUE
llrbtree_s_load(VALUE klass, VALUE str)
{
    VALUE llrbtree = llrbtree_alloc(klass);
    VALUE ary = rb_marshal_load(str);
    long len = RARRAY_LEN(ary) - 1;
    long i;

    for (i = 0; i < len; i += 2)
        llrbtree_aset(llrbtree, RARRAY_AREF(ary, i), RARRAY_AREF(ary, i + 1));
    IFNONE(llrbtree) = RARRAY_AREF(ary, len);

    rb_ary_resize(ary, 0);
    return llrbtree;
}

/*********************************************************************/
/*
 * Document-class: LLRBTree
 *
 * A sorted associative collection that cannot contain duplicate keys.
 */
void Init_llrbtree()
{
    LLRBTree = rb_define_class("LLRBTree", rb_cData);

    rb_include_module(LLRBTree, rb_mEnumerable);

    rb_define_alloc_func(LLRBTree, llrbtree_alloc);

    rb_define_singleton_method(LLRBTree, "[]", llrbtree_s_create, -1);

    rb_define_method(LLRBTree, "initialize", llrbtree_initialize, -1);
    rb_define_method(LLRBTree, "initialize_copy", llrbtree_initialize_copy, 1);

    rb_define_method(LLRBTree, "to_a", llrbtree_to_a, 0);
    rb_define_method(LLRBTree, "to_h", llrbtree_to_hash, 0);
    rb_define_method(LLRBTree, "to_hash", llrbtree_to_hash, 0);
    rb_define_method(LLRBTree, "to_llrbtree", llrbtree_to_llrbtree, 0);
    rb_define_method(LLRBTree, "inspect", llrbtree_inspect, 0);
    rb_define_alias(LLRBTree, "to_s", "inspect");

    rb_define_method(LLRBTree, "==", llrbtree_equal, 1);
    rb_define_method(LLRBTree, "[]", llrbtree_aref, 1);
    rb_define_method(LLRBTree, "fetch", llrbtree_fetch, -1);
    rb_define_method(LLRBTree, "lower_bound", llrbtree_lower_bound, 1);
    rb_define_method(LLRBTree, "upper_bound", llrbtree_upper_bound, 1);
    rb_define_method(LLRBTree, "bound", llrbtree_bound, -1);
    rb_define_method(LLRBTree, "first", llrbtree_first, 0);
    rb_define_method(LLRBTree, "last", llrbtree_last, 0);
    rb_define_method(LLRBTree, "[]=", llrbtree_aset, 2);
    rb_define_method(LLRBTree, "store", llrbtree_aset, 2);
    rb_define_method(LLRBTree, "default", llrbtree_default, -1);
    rb_define_method(LLRBTree, "default=", llrbtree_set_default, 1);
    rb_define_method(LLRBTree, "default_proc", llrbtree_default_proc, 0);
    rb_define_method(LLRBTree, "default_proc=", llrbtree_set_default_proc, 1);
    rb_define_method(LLRBTree, "key", llrbtree_key, 1);
    rb_define_method(LLRBTree, "index", llrbtree_index, 1);
    rb_define_method(LLRBTree, "empty?", llrbtree_empty_p, 0);
    rb_define_method(LLRBTree, "size", llrbtree_size, 0);
    rb_define_method(LLRBTree, "length", llrbtree_size, 0);

    rb_define_method(LLRBTree, "each", llrbtree_each_pair, 0);
    rb_define_method(LLRBTree, "each_value", llrbtree_each_value, 0);
    rb_define_method(LLRBTree, "each_key", llrbtree_each_key, 0);
    rb_define_method(LLRBTree, "each_pair", llrbtree_each_pair, 0);
    rb_define_method(LLRBTree, "reverse_each", llrbtree_reverse_each, 0);

    rb_define_method(LLRBTree, "keys", llrbtree_keys, 0);
    rb_define_method(LLRBTree, "values", llrbtree_values, 0);
    rb_define_method(LLRBTree, "values_at", llrbtree_values_at, -1);

    rb_define_method(LLRBTree, "shift", llrbtree_shift, 0);
    rb_define_method(LLRBTree, "pop", llrbtree_pop, 0);
    rb_define_method(LLRBTree, "delete", llrbtree_delete, 1);
    rb_define_method(LLRBTree, "delete_if", llrbtree_delete_if, 0);
    rb_define_method(LLRBTree, "keep_if", llrbtree_keep_if, 0);
    rb_define_method(LLRBTree, "reject", llrbtree_reject, 0);
    rb_define_method(LLRBTree, "reject!", llrbtree_reject_bang, 0);
    rb_define_method(LLRBTree, "select", llrbtree_select, 0);
    rb_define_method(LLRBTree, "select!", llrbtree_select_bang, 0);
    rb_define_method(LLRBTree, "clear", llrbtree_clear, 0);
    rb_define_method(LLRBTree, "invert", llrbtree_invert, 0);
    rb_define_method(LLRBTree, "update", llrbtree_update, 1);
    rb_define_method(LLRBTree, "merge!", llrbtree_update, 1);
    rb_define_method(LLRBTree, "merge", llrbtree_merge, 1);
    rb_define_method(LLRBTree, "replace", llrbtree_initialize_copy, 1);
    rb_define_method(LLRBTree, "flatten", llrbtree_flatten, -1);

    rb_define_method(LLRBTree, "include?", llrbtree_has_key, 1);
    rb_define_method(LLRBTree, "member?", llrbtree_has_key, 1);
    rb_define_method(LLRBTree, "has_key?", llrbtree_has_key, 1);
    rb_define_method(LLRBTree, "has_value?", llrbtree_has_value, 1);
    rb_define_method(LLRBTree, "key?", llrbtree_has_key, 1);
    rb_define_method(LLRBTree, "value?", llrbtree_has_value, 1);

    rb_define_method(LLRBTree, "readjust", llrbtree_readjust, -1);
    rb_define_method(LLRBTree, "cmp_proc", llrbtree_cmp_proc, 0);

    rb_define_method(LLRBTree, "_dump", llrbtree_dump, 1);
    rb_define_singleton_method(LLRBTree, "_load", llrbtree_s_load, 1);

    id_cmp = rb_intern("<=>");
    id_call = rb_intern("call");
    id_default = rb_intern("default");
    id_flatten_bang = rb_intern("flatten!");
    id_marshal = rb_intern("Marshal");
    id_dump = rb_intern("dump");

    rb_define_method(LLRBTree, "pretty_print", llrbtree_pretty_print, 1);
    rb_define_method(LLRBTree,
                     "pretty_print_cycle", llrbtree_pretty_print_cycle, 1);

    id_breakable = rb_intern("breakable");
    id_comma_breakable = rb_intern("comma_breakable");
    id_group = rb_intern("group");
    id_object_group = rb_intern("object_group");
    id_pp = rb_intern("pp");
    id_text = rb_intern("text");
}
