module ChaeTree
    exposing
        ( Id
        , Node
        , Tree
        , createNode
        , toId
        , getId
        , unpack
        , push
        , getSubTree
        , pushChild
        , fromList
        , subTreeFor
        )

{-| This library implements [RoseTree](https://en.wikipedia.org/wiki/Rose_tree) related datastructure
and essential functions for its manipulations. `ChaeTree` (name comes from [Chaenomeles](https://en.wikipedia.org/wiki/Chaenomeles))
can be essential structure **for building multi level navigation browser or anything with similar hierarchic structure**.
Unlike `RoseTree` `ChaeTree` do not have single root node which makes it more *tree like* data structure rather then regular tree.
`ChaeTree` itself is just list of Nodes where each node is it's own `RoseTree`.
Beside this every `Node` has its `id` which makes some useful manipulations easier.

# Types
@docs Id, Node, Tree

# Common Helpers
@docs toId, createNode, getId, unpack, pushChild, getSubTree

# Common Operations
@docs push, fromList, subTreeFor

# Examples
Please see [example](https://github.com/turboMaCk/ChaeTree/tree/master/examples) implementations to learn more.

-}

import List exposing (..)
import Maybe exposing (Maybe(..))


-- Types


{-| `Id` is just alias for `String` type
-}
type alias Id =
    String


{-| Node is single `RoseTree` like type. More `Node`s together create `ChaeTree`.
This is how node is defined:
-}
type Node a
    = Node Id a (Tree a)


{-| Tree is just type alias for `List (Node a)`.
Tree is top level datastructure and is defined as:
-}
type alias Tree a =
    List (Node a)



-- Public API


{-| Convert any value to `Id` type.
This is just alias for `toString` function.

    toId "str" = "str"
    toId 1 = "1"
    toId = "{ a = \"a\" }"
-}
toId : a -> Id
toId =
    toString


{-| Create `Node` for given values.
First paramter is function which takes given value and return it's id.

    createNode toId 1 == Node "1" 1 []
    createNode toId { a = "b" } == Node "{ a = \"b\" }" { a = "b" } []
    createNode (\a -> .id a) { id = "1" } == Node "1" { id = "1" } []
    createNode (\a -> .id a |> toId ) { id = 1 } == Node "1" { id = 1 } []
-}
createNode : (a -> Id) -> a -> Node a
createNode getId item =
    Node (getId item) item []


{-| Get id of given `Node`.

    getId (createNode toId 1) == "1"
    getId (createNode (\_ -> "uid") { a = "a"} ) == "uid"
-}
getId : Node a -> Id
getId (Node id _ _) =
    id


{-| Turns `Node` back to value it was created from.

This function provide recommended way to access user space data while working with tree.

    unpack (createNode toId "Elm") == "Elm"
    unpack (createNode (\i -> .id i) { id = "1", name = "Elm" }) == { id = "1", name = "Elm" }
-}
unpack : Node a -> a
unpack (Node _ a _) =
    a


{-| Return `Node` with item add as sub `Node`.
First argument is function from item to `Id/String`.

    pushChild toId 2 (createNode toId 1) == Node "1" 1 ([Node "2" 2 []])
    pushChild toId 3 (pushChild toId 2 (createNode toId 1)) == Node "1" 1 ([Node "3" 3 [],Node "2" 2 []])
-}
pushChild : (a -> Id) -> a -> Node a -> Node a
pushChild idGet item (Node id a children) =
    let
        node =
            Node (idGet item) item []
    in
        (Node id a (node :: children))


{-| Get child tree of `Node`.
This is common way to access sub tree of given node.

    getSubTree (createNode toId 1) == []
    getSubTree (pushChild toId 2 (createNode toId 1)) == [Node "2" 2 []]
-}
getSubTree : Node a -> Tree a
getSubTree (Node _ _ children) =
    children


{-| Produce new tree with given item pushed under its parent.
First argument is function from item to `Id/String`.

Second argument is `Maybe Id` is ether:

- `Nothing` => push to root
- `Just parentId` => push to sub Tree

    push toId Nothing 1 [] == [Node "1" 1 []]
    push toId (Just (toId 1)) 2 [ createNode toId 1 ] == [Node "1" 1 ([Node "2" 2 []])]
-}
push : (a -> Id) -> Maybe Id -> a -> Tree a -> Tree a
push idGet maybeId item tree =
    case maybeId of
        Just id ->
            map (\node -> pushDeep idGet id item node) tree

        Nothing ->
            Node (idGet item) item [] :: tree


{-| Build `Tree` from given list of items.
First argument is function from item to `Id/String`.
Second argument is function from item to `List Id/List String`.

    items =
        [ { id = 1, name = "first", parentIds = [] }
        , { id = 2, name = "child", parentIds = [1] }
        , { id = 3, name = "dep categories", parentIds = [2] }
        ]

     itemId item =
         toId (.id item )

     itemParentIds item =
         .parentIds item |> List.map toId

     fromList itemId itemParentIds items == [Node "1" { id = 1, name = "first", parentIds = [] } ([Node "2" { id = 2, name = "child", parentIds = [1] } ([Node "3" { id = 3, name = "dep categories", parentIds = [2] } []])])]
-}
fromList : (a -> Id) -> (a -> List Id) -> List a -> Tree a
fromList idGet getParentId list =
    fromListLevel idGet getParentId list Nothing


{-| Returns sub `Tree` and ancestors for given `Id` and `Tree`.
First argument is `Maybe Id` is ether:

- `Nothing` => result is given tree (with empty ancestors `List`).
- `Just parentId` => result is sub tree for node with `id == parentId`.

Returns tuple containing sub tree and list of ancestors (paratenrs of root `Node`).

    items =
        [ { id = 1, name = "first", parentIds = [] }
        , { id = 2, name = "child", parentIds = [1] }
        , { id = 3, name = "dep categories", parentIds = [2] }
        ]

     itemId item =
         toId (.id item )

     itemParentIds item =
         .parentIds item |> List.map toId

     tree =
        fromList itemId itemParentIds items

     subtreeFor Nothing tree == ([Node "1" { id = 1, name = "first", parentIds = [] } ([Node "2" { id = 2, name = "child", parentIds = [1] } ([Node "3" { id = 3, name = "dep categories", parentIds = [2] } []])])],[])
     subtreeFor (Just "1") tree == ([Node "2" { id = 2, name = "child", parentIds = [1] } ([Node "3" { id = 3, name = "dep categories", parentIds = [2] } []])],[{ id = 1, name = "first", parentIds = [] }])
     subtreeFor (Just "2") tree == ([Node "3" { id = 3, name = "dep categories", parentIds = [2] } []],[{ id = 2, name = "child", parentIds = [1] },{ id = 1, name = "first", parentIds = [] }])
-}
subTreeFor : Maybe Id -> Tree a -> ( Tree a, List a )
subTreeFor maybeId tree =
    case maybeId of
        Just id ->
            subTreeFor' id ( tree, [] )

        Nothing ->
            ( tree, [] )



-- Private API


pushDeep : (a -> Id) -> Id -> a -> Node a -> Node a
pushDeep idGet id item ((Node nodeId a children) as node) =
    if nodeId == id then
        pushChild idGet item node
    else
        Node nodeId a (map (\node -> pushDeep idGet id item node) children)


fromListLevel : (a -> Id) -> (a -> List Id) -> List a -> Maybe Id -> Tree a
fromListLevel idGet getParentId list maybeId =
    let
        children =
            case maybeId of
                Nothing ->
                    filter (\item -> isEmpty (getParentId item)) list

                Just id ->
                    filter (\item -> member id (getParentId item)) list
    in
        map (\i -> Node (idGet i) i (fromListLevel idGet getParentId list (Just (idGet i)))) children


subTreeFor' : Id -> ( Tree a, List a ) -> ( Tree a, List a )
subTreeFor' id ( tree, accestors ) =
    let
        matches =
            filter (\n -> getId n == id) tree

        subMatches (Node _ item subTree) =
            subTreeFor' id ( subTree, item :: accestors )
    in
        case (head matches) of
            Just (Node _ item subTree) ->
                ( subTree, item :: accestors )

            Nothing ->
                foldr
                    (\child acc ->
                        if (length (subMatches child |> fst)) > 0 then
                            subMatches child
                        else
                            acc
                    )
                    ( [], [] )
                    tree
