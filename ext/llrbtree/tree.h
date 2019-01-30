/*
 * MIT License
 * Copyright (c) 2019 Kyuden Masahiro
 */

#ifndef LLRB_TREE_H
#define LLRB_TREE_H

#include <limits.h>

#ifdef __cplusplus
extern "C" {
#endif

#define NODE_COUNT_T_MAX ULONG_MAX

typedef enum { node_red, node_black } node_color_t;

typedef struct node_t {
    struct node_t *left;
    struct node_t *right;
    struct node_t *parent;
    node_color_t color;
    const void *key;
    void *data;
} node_t;

typedef unsigned long node_count_t;
typedef int (*compare_t)(const void *, const void *, void *);
typedef node_t *(*alloc_node_t)(void *);
typedef void (*free_node_t)(node_t *, void *);

typedef struct tree_t {
    node_t *root_node;
    node_count_t node_count;
    compare_t compare;
    alloc_node_t alloc_node;
    free_node_t free_node;
    void *context;
} tree_t;

extern void tree_free(tree_t *);
extern tree_t *tree_init(tree_t *, compare_t, alloc_node_t, free_node_t, void *);
extern int tree_verify(tree_t *);
extern int tree_similar(const tree_t *, const tree_t *);
extern node_t *tree_lookup(tree_t *, const void *);
extern node_t *tree_lower_bound(tree_t *, const void *);
extern node_t *tree_upper_bound(tree_t *, const void *);
extern int tree_insert(tree_t *, node_t *, const void *);
extern void tree_delete(tree_t *, const void *);
extern node_t *tree_first(tree_t *);
extern node_t *tree_last(tree_t *);
extern node_t *tree_next(tree_t *, node_t *);
extern node_t *tree_prev(tree_t *, node_t *);
extern int tree_isempty(tree_t *);
extern int tree_isfull(tree_t *);
extern node_t *node_init(node_t *, void *);

#ifdef __cplusplus
}
#endif

#endif
