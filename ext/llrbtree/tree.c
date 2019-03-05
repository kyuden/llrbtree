/*
 * MIT License
 * Copyright (c) 2019 Kyuden Masahiro
 */

#include <stdlib.h>
#include "tree.h"

static int
is_red(node_t *node)
{
    return node == NULL ? 0 : node->color == node_red;
}

static int
is_black(node_t *node)
{
    return node == NULL ? 0 : node->color == node_black;
}

static node_t*
rotate_left(node_t *upper)
{
    node_t *lower;

    lower = upper->right;
    upper->right = lower->left;
    if (upper->right != NULL) {
        upper->right->parent = upper;
    }

    lower->left = upper;
    lower->parent = upper->parent;
    lower->left->parent = lower;

    lower->color = upper->color;
    upper->color = node_red;

    return lower;
}

static node_t*
rotate_right(node_t *upper)
{
    node_t *lower;

    lower = upper->left;
    upper->left = lower->right;
    if (upper->left != NULL) {
        upper->left->parent = upper;
    }

    lower->right = upper;
    lower->parent = upper->parent;
    lower->right->parent = lower;

    lower->color = upper->color;
    upper->color = node_red;

    return lower;
}

static void
flip_color(node_t *node)
{
    is_red(node) ? (node->color = node_black) : (node->color = node_red);

    if (node->left != NULL) {
        is_red(node->left) ? (node->left->color = node_black) : (node->left->color = node_red);
    }
    if (node->right != NULL) {
        is_red(node->right) ? (node->right->color = node_black) : (node->right->color = node_red);
    }
}

static node_t*
fix_up(node_t *node)
{
    if (is_red(node->right)) {
        node = rotate_left(node);
    }
    if (is_red(node->left) && is_red(node->left->left)) {
        node = rotate_right(node);
    }
    if (is_red(node->left) && is_red(node->right)) {
        flip_color(node);
    }

    return node;
}

static node_t*
move_red_left(node_t *node)
{
    flip_color(node);
    if (node->right != NULL && is_red(node->right->left)) {
        node->right = rotate_right(node->right);
        node = rotate_left(node);
        flip_color(node);
    }

    return node;
}

static node_t*
move_red_right(node_t *node)
{
    flip_color(node);
    if (node->left != NULL && is_red(node->left->left)) {
        node = rotate_right(node);
        flip_color(node);
    }

    return node;
}

static node_t*
delete_min(tree_t *tree, node_t *node)
{
    if (node->left == NULL) {
        tree->free_node(node, tree->context);
        return NULL;
    }
    if (is_black(node->left) && (node->left->left == NULL || is_black(node->left->left))) {
        node = move_red_left(node);
    }

    node->left = delete_min(tree, node->left);
    return fix_up(node);
}

static node_t*
min(node_t *node)
{
    while (node->left != NULL) {
        node = node->left;
    }
    return node;
}

static void
free_nodes(tree_t *tree, node_t *node)
{
    if (node == NULL) return;

    free_nodes(tree, node->left);
    free_nodes(tree, node->right);
    tree->free_node(node, tree->context);
}

static int
verify_bintree(tree_t *tree)
{
    node_t *first, *next;

    first = tree_first(tree);

    while (first != NULL && (next = tree_next(tree, first))) {
        if (tree->compare(first->key, next->key, tree->context) >= 0) {
            return 0;
        }
        first = next;
    }
    return 1;
}

static unsigned int
verify_redblack(node_t *root)
{
    unsigned height_left, height_right;

    if (root != NULL) {
        height_left = verify_redblack(root->left);
        height_right = verify_redblack(root->right);
        if (height_left == 0 || height_right == 0) return 0;
        if (height_left != height_right) return 0;
        if (is_red(root)) {
            if (is_red(root->left)) return 0;
            if (is_red(root->right)) return 0;
            return height_left;
        }
        if (is_red(root)) return 0;
        return height_left + 1;
    }
    return 1;
}

static node_count_t
verify_node_count(node_t *root)
{
    if (root == NULL) return 0;

    return 1 + verify_node_count(root->left) + verify_node_count(root->right);
}

void
tree_free(tree_t *tree)
{
    free_nodes(tree, tree->root_node);
    tree->root_node = NULL;
    tree->node_count = 0;
}

tree_t*
tree_init(tree_t *tree, compare_t comp, alloc_node_t al, free_node_t fr, void *context)
{
    tree->compare = comp;
    tree->alloc_node = al;
    tree->free_node = fr;
    tree->context = NULL;
    tree->node_count = 0;
    tree->root_node = NULL;
    tree->context = context;
    return tree;
}

int
tree_verify(tree_t *tree)
{
    if (is_red(tree->root_node)) return 0;
    if (tree->root_node->parent != NULL) return 0;
    if (!verify_bintree(tree)) return 0;
    if (!verify_redblack(tree->root_node)) return 0;
    if (verify_node_count(tree->root_node) != tree->node_count) return 0;
    return 1;
}

int
tree_similar(const tree_t *left, const tree_t *right)
{
    if (left->compare != right->compare) return 0;
    if (left->alloc_node != right->alloc_node) return 0;
    if (left->free_node != right->free_node) return 0;
    if (left->context != right->context) return 0;
    return 1;
}

node_t*
tree_lookup(tree_t *tree, const void *key)
{
    node_t *root = tree->root_node;
    int result;

    while (root != NULL) {
        result = tree->compare(key, root->key, tree->context);
        if (result < 0) {
            root = root->left;
        } else if (result > 0) {
            root = root->right;
        } else {
            return root;
        }
    }

    return NULL;
}

node_t*
tree_lower_bound(tree_t *tree, const void *key)
{
    node_t *root = tree->root_node;
    node_t *tentative = 0;

    while (root != NULL) {
        int result = tree->compare(key, root->key, tree->context);

        if (result > 0) {
            root = root->right;
        } else if (result < 0) {
            tentative = root;
            root = root->left;
        } else {
            return root;
        }
    }

    return tentative;
}

node_t*
tree_upper_bound(tree_t *tree, const void *key)
{
    node_t *root = tree->root_node;
    node_t *tentative = 0;

    while (root != NULL) {
        int result = tree->compare(key, root->key, tree->context);

        if (result < 0) {
            root = root->left;
        } else if (result > 0) {
            tentative = root;
            root = root->right;
        } else {
            return root;
        }
    }

    return tentative;
}

static node_t*
recursive_insert(tree_t *tree, node_t *node, node_t *new_node, const void *key)
{
    int result = -1;
    if (node == NULL) {
        tree->node_count++;
        return new_node;
    }

    result = tree->compare(key, node->key, tree->context);

    if (result == 0) {
        node->data = new_node->data;
    } else if (result < 0) {
        node->left = recursive_insert(tree, node->left, new_node, key);
        node->left->parent = node;
    } else {
        node->right = recursive_insert(tree, node->right, new_node, key);
        node->right->parent = node;
    }

    return fix_up(node);
}

int
tree_insert(tree_t *tree, node_t *node, const void *key)
{
    tree->root_node = recursive_insert(tree, tree->root_node, node, key);
    tree->root_node->color = node_black;
    return 1;
}

static node_t*
recursive_delete(tree_t *tree, node_t *node, const void *key)
{
    node_t *min_node;

    if (node == NULL) return NULL;

    if (tree->compare(key, node->key, tree->context) < 0) {
        if (is_black(node->left) && (node->left->left == NULL || is_black(node->left->left))) {
            node = move_red_left(node);
        }
        node->left = recursive_delete(tree, node->left, key);
    } else {
        if (is_red(node->left)) {
            node = rotate_right(node);
        }

        if (tree->compare(key, node->key, tree->context) == 0 && node->right == NULL) {
            tree->free_node(node, tree->context);
            return NULL;
        }

        if (is_black(node->right) && (node->right->left == NULL || is_black(node->right->left))) {
            node = move_red_right(node);
        }

        if (tree->compare(key, node->key, tree->context) == 0) {
            min_node = min(node->right);
            node->key = min_node->key;
            node->data = min_node->data;
            node->right = delete_min(tree, node->right);
        } else {
            node->right = recursive_delete(tree, node->right, key);
        }
    }

    return fix_up(node);
}

void
tree_delete(tree_t *tree, const void *key)
{
  tree->root_node = recursive_delete(tree, tree->root_node, key);
  if (tree->root_node != NULL) {
    tree->root_node->color = node_black;
  }
  tree->node_count--;
}

node_t*
tree_first(tree_t *tree)
{
    node_t *root = tree->root_node, *left;

    if (root != NULL) {
        while ((left = root->left) != NULL) {
            root = left;
        }
    }

    return (root == NULL) ? NULL : root;
}

node_t*
tree_last(tree_t *tree)
{
    node_t *root = tree->root_node, *right;

    if (root != NULL) {
        while ((right = root->right) != NULL) {
            root = right;
        }
    }

    return (root == NULL) ? NULL : root;
}

node_t*
tree_next(tree_t *tree, node_t *curr)
{
    node_t *parent, *left;

    if (curr->right != NULL) {
        curr = curr->right;
        while ((left = curr->left) != NULL) {
            curr = left;
        }
        return curr;
    }

    parent = curr->parent;

    while (parent != NULL && curr == parent->right) {
        curr = parent;
        parent = curr->parent;
    }

    return (parent == NULL) ? NULL : parent;
}

node_t*
tree_prev(tree_t *tree, node_t *curr)
{
    node_t *parent, *right;

    if (curr->left != NULL) {
        curr = curr->left;
        while ((right = curr->right) != NULL) {
            curr = right;
        }
        return curr;
    }

    parent = curr->parent;

    while (parent != NULL && curr == parent->left) {
        curr = parent;
        parent = curr->parent;
    }

    return (parent == NULL) ? NULL : parent;
}

int
tree_isempty(tree_t *tree)
{
    return tree->node_count == 0;
}

int
tree_isfull(tree_t *tree)
{
    return tree->node_count == NODE_COUNT_T_MAX;
}

node_t*
node_init(node_t *node, void *data)
{
    node->data = data;
    node->parent = NULL;
    node->left = NULL;
    node->right = NULL;
    node->color = node_red;
    return node;
}
