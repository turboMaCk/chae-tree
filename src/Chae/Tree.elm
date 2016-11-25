module Chae.Tree
    exposing
        ( Tree
        , nil
        , map
        , map2
        , zip
        , reduce
        , filter
        , push
        , fromList
        , subTreeFor
        )

{-| Tree is list of nodes.
Tree is main data structure this package provides and this module implements the most essential functions
which really differs from general Rose Tree implementation. Along side with functor functions
trees support operation like `push`, `subTreeFor` and `fromList`. These functions make it easy to create
and manipulate trees only by knowing Ids of items.

# Definition
@docs Tree

# Constructors
@docs nil, fromList

# Query a Tree
@docs subTreeFor

# Common Operations
@docs push

# Map - Reduce
@docs map, map2, zip, reduce, filter

-}
import Tuple

import List
import Chae.Id exposing (..)
import Chae.Node as Node
import Maybe exposing (Maybe(..))


-- Types


{-| Tree
-}
type alias Tree a =
    List (Node.Node a)


{-| Construct empty tree
Alias for []
-}
nil : Tree a
nil =
    []


{-| Map function over tree
Similar to `List.map` but working with trees
-}
map : (a -> Id) -> (a -> b) -> Tree a -> Tree b
map getId fc =
    List.map (Node.map getId fc)


{-| Map function over two trees to produce new tree from both combined
Similar to `List.map2` but working with trees
-}
map2 : (a -> b -> Id) -> (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 getId fc =
    List.map2 (Node.map2 getId fc)


{-| Zip two trees to tree of tuple
Similar to `List.zip` but working with trees
-}
zip : (a -> b -> Id) -> Tree a -> Tree b -> Tree ( a, b )
zip getId =
    map2 getId (,)


{-| Reduce Tree by given function
Similar to `List.foldl` but working with trees
-}
reduce : (a -> b -> b) -> b -> Tree a -> b
reduce reducer =
    List.foldl (flip (Node.reduce reducer))


{-| Filter Tree.
Similar to `List.filter` but working on trees.
If parent node do not pass condition its children are no included in result

    tree = [Node.node "5" 5 [ Node.singleton "1" 1, Node.singleton "10" 10 ] ]

    filter ((<) 4) tree == [Node "5" 5 ([Node "10" 10 []])]
    filter ((<) 6) tree == []
    filter ((<) 0) tree == tree

-}
filter : (a -> Bool) -> Tree a -> Tree a
filter fc =
    let
        sieve node acc =
            let
                ( id, a, c ) =
                    Node.toTuple node
            in
                if fc a then
                    (Node.node id a (filter fc c)) :: acc
                else
                    acc
    in
        List.foldr sieve []


{-| Produce new tree with given item pushed under its parent.
First argument is function from item to `Id/String`.

Second argument is `Maybe Id` is ether:

- `Nothing` => push to root
- `Just parentId` => push to sub Tree


    push toId Nothing 1 [] == [Node "1" 1 []]
    push toId (Just (toId 1)) 2 [ Node.singleton "1" 1 ] == [Node "1" 1 ([Node "2" 2 []])]
-}
push : (a -> Id) -> Maybe Id -> a -> Tree a -> Tree a
push getId maybeId item tree =
    case maybeId of
        Just id ->
            List.map (Node.pushDeep id (getId item) item) tree

        Nothing ->
            Node.singleton (getId item) item :: tree


{-| Build `Tree` from given list of items.
First argument is function from item to `Id/String`.
Second argument is function from item to `List Id/List String`.

    items =
        [ { id = 1, name = "first", parentIds = [] }
        , { id = 2, name = "child", parentIds = [1] }
        , { id = 3, name = "deep child", parentIds = [2] }
        ]

     itemId item =
         toId (.id item )

     itemParentIds item =
         .parentIds item |> List.map toId

     fromList itemId itemParentIds items == [Node "1" { id = 1, name = "first", parentIds = [] } ([Node "2" { id = 2, name = "child", parentIds = [1] } ([Node "3" { id = 3, name = "deep child", parentIds = [2] } []])])]
-}
fromList : (a -> Id) -> (a -> List Id) -> List a -> Tree a
fromList getId getParentId list =
    fromList_ getId getParentId list Nothing


fromList_ : (a -> Id) -> (a -> List Id) -> List a -> Maybe Id -> Tree a
fromList_ getId getParentId list maybeId =
    let
        children =
            case maybeId of
                Nothing ->
                    List.filter (\item -> List.isEmpty (getParentId item)) list

                Just id ->
                    List.filter (\item -> List.member id (getParentId item)) list
    in
        List.map (\i -> Node.node (getId i) i (fromList_ getId getParentId list (Just (getId i)))) children


{-| Returns sub `Tree` and ancestors for given `Id` and `Tree`.
First argument is `Maybe Id` is ether:

- `Nothing` => result is given tree (with empty ancestors `List`).
- `Just parentId` => result is sub tree for node with `id == parentId`.

Returns tuple containing sub tree and list of ancestors of `Node`.

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

     subTreeFor Nothing tree == (tree, [])
     subTreeFor (Just "1") tree == ([Node "2" { id = 2, name = "child", parentIds = [1] } ([Node "3" { id = 3, name = "dep categories", parentIds = [2] } []])],[{ id = 1, name = "first", parentIds = [] }])
     subTreeFor (Just "2") tree == ([Node "3" { id = 3, name = "dep categories", parentIds = [2] } []],[{ id = 2, name = "child", parentIds = [1] },{ id = 1, name = "first", parentIds = [] }])
-}
subTreeFor : Maybe Id -> Tree a -> ( Tree a, List a )
subTreeFor maybeId tree =
    case maybeId of
        Just id ->
            subTreeFor_ id ( tree, [] )

        Nothing ->
            ( tree, [] )


subTreeFor_ : Id -> ( Tree a, List a ) -> ( Tree a, List a )
subTreeFor_ id ( tree, ancestors ) =
    let
        matches =
            List.filter (\n -> Node.id n == id) tree

        nest node =
            let
                ( _, item, children ) =
                    Node.toTuple node
            in
                ( children, item :: ancestors )
    in
        case (List.head matches) of
            Just node ->
                nest node

            Nothing ->
                List.foldr
                    (\child acc ->
                        let
                            subMatches =
                                subTreeFor_ id (nest child)
                        in
                            if List.isEmpty (subMatches |> Tuple.second) then
                                acc
                            else
                                subMatches
                    )
                    ( [], [] )
                    tree
