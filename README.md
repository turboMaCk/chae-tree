# Chae-Tree

[![Build Status](https://travis-ci.org/turboMaCk/chae-tree.svg?branch=master)](https://travis-ci.org/turboMaCk/chae-tree)

Create multi-level navigation in elm easily.

This package provides essential abstractions for manipulating and creating tree data structure directly from your collection data.

Imagine you have collection of some items which has defined parent <-> children relationship.
This package provides easy and universal way to transform this list to tree like data structure
and comes with easy to use functions you can use to work with it.
For example you can easily browse this tree by levels, getting ancestors of node etc.

This package makes no decision about neither `update` nor `model` nor `view`.
You can build your own application level logic as you wish and just use provided api to manipulate your data.
Every abstraction which you might need is build in so you're saved from thinking about implementation details
and rather focus on actual UI and business logic.

The name comes from [Chaenomeles](https://en.wikipedia.org/wiki/Chaenomeles) and is reference to similarities with [RoseTree](https://en.wikipedia.org/wiki/Rose_tree)

**The good place to start is by looking on [example code](https://github.com/turboMaCk/ChaeTree/tree/master/examples).**

# Installation

```
elm-package install turboMaCk/chae-tree
```

For more informations please follow [documentation](http://package.elm-lang.org/packages/turboMaCk/chae-tree/latest)
or see [examples](https://github.com/turboMaCk/ChaeTree/tree/master/examples) of usage.

# More About Data Structure

You might be familiar with [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree) data structure.
The data structure this plugin uses is quite similar but not really the same.
This paragraph(s) is to explain similarities and differences between the two and more importantly explain why they are there.
First things first **please do not use this implementation as an alternative to plain Rose Trees!**
Even they share some similarities you can find it unnecessary hard to use this in every place you might want to use Rose Tree.
Think about this as about more domain specific *tree like* structure which might be quite handy for one thing but not that good for some other.

## Chae.Tree is Forest

In fact `Tree` in the context of this plugin **is not** node containing item and children.
Tree is collection of multiple nodes (`List` of `Node a` to be more precise).
Thanks to this `Chae.Tree` allows you to have multiple nodes in root.
For example think about collection of categories where every category can have multiple sub-categories.
In that case `Tree` is actual collection and every category is one `Node` in that tree.

## Chae.Node is not Rose Tree Either

Now you might be thinking that if `Tree` is just an alias for `List (Node a)` than `Node a` is actually a Rose Tree.
You're partially right! In fact `Node` is pretty close to Rose Tree definition which might look like:

```elm
type RoseTree a = Node a (List (RoseTree a))
```

in fact node is defined as follows:

```elm
type Node a = Node Id a (List (Node a))

```

So what is that magic `Id`? `Id` is just alias for `String`. And this Id/String is used as identifier for that node.
The string is used since you can easily represent any value using string
and since it do not makes much sense to allow any arithmetics over identifier it's perfect fit for ids.

So every node in `Tree` has its id. What is this good for? Well first of all you can lookup any node by knowing its id.
Also it is super easy to build tree from plain `List` if you just know what is a parent of each item in that list.

**All these functions are implemented for you so you can simple transform your flat collection into tree and start quering it by ids.**

## Functors

Beside this both `Node` and `Tree` are functors and you can find essential functions like `map`, `reduce`, `zip` and similar
in particular module.
