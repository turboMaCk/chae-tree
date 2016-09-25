# ChaeTree

This library implements [RoseTree](https://en.wikipedia.org/wiki/Rose_tree) related datastructure
and essential functions for its manipulations. `ChaeTree` (name comes from [Chaenomeles](https://en.wikipedia.org/wiki/Chaenomeles))
can be essential structure **for building multi level navigation browser or anything with similar hierarchic structure**.
Unlike `RoseTree` `ChaeTree` do not have single root node which makes it more *tree like* data structure rather then regular tree.
`ChaeTree` itself is just list of Nodes where each node is it's own `RoseTree`.
Beside this every `Node` has its `id` which makes some useful manipulations easier.


## Instalation

```
elm-package install turbomack/chaetree
```

For more informations please follow [documentation](http://package.elm-lang.org/packages/turbomack/chaetree/latest)
or see [examples](https://github.com/turboMaCk/ChaeTree/tree/master/examples) of usage.
