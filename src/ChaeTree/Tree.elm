module ChaeTree.Tree
    exposing
        ( Tree
        , map
        , map2
        , zip
        , foldr
        , sum
        , product
        , push
        , fromList
        , subTreeFor
        )

{-| Tree

@docs Tree @map2, zip, foldr, sum, product, push, fromList, subTreeFor

-}

import List
import ChaeTree.Id exposing (..)
import ChaeTree.Node as Node
import Maybe exposing (Maybe(..))


-- Types


{-| Tree
-}
type alias Tree a =
    List (Node.Node a)


{-|
-}
map : (a -> Id) -> (a -> b) -> Tree a -> Tree b
map getId fc =
    List.map (Node.map getId fc)


{-|
-}
map2 : (a -> b -> Id) -> (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 getId fc =
    List.map2 (Node.map2 getId fc)


{-|
-}
zip : (a -> b -> Id) -> Tree a -> Tree b -> Tree ( a, b )
zip getId =
    map2 getId (,)


{-|
-}
foldr : (a -> b -> b) -> b -> Tree a -> b
foldr reducer b tree =
    List.foldr (flip (Node.foldr reducer)) b tree


{-|
-}
sum : Tree number -> number
sum =
    foldr (+) 0


{-|
-}
product : Tree number -> number
product =
    foldr (*) 1


{-| Produce new tree with given item pushed under its parent.
First argument is function from item to `Id/String`.

Second argument is `Maybe Id` is ether:

- `Nothing` => push to root
- `Just parentId` => push to sub Tree

    push toId Nothing 1 [] == [Node "1" 1 []]
    push toId (Just (toId 1)) 2 [ Node.simple 1 [] ] == [Node "1" 1 ([Node "2" 2 []])]
-}
push : (a -> Id) -> Maybe Id -> a -> Tree a -> Tree a
push getId maybeId item tree =
    case maybeId of
        Just id ->
            List.map (Node.pushDeep getId id item) tree

        Nothing ->
            Node.singleton getId item :: tree


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
fromList getId getParentId list =
    fromList' getId getParentId list Nothing


fromList' : (a -> Id) -> (a -> List Id) -> List a -> Maybe Id -> Tree a
fromList' getId getParentId list maybeId =
    let
        children =
            case maybeId of
                Nothing ->
                    List.filter (\item -> List.isEmpty (getParentId item)) list

                Just id ->
                    List.filter (\item -> List.member id (getParentId item)) list
    in
        List.map (\i -> Node.node (getId i) i (fromList' getId getParentId list (Just (getId i)))) children


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


subTreeFor' : Id -> ( Tree a, List a ) -> ( Tree a, List a )
subTreeFor' id ( tree, accestors ) =
    let
        matches =
            List.filter (\n -> Node.id n == id) tree

        subMatches node =
            let
                ( _, item, children ) =
                    Node.toTuple node
            in
                subTreeFor' id ( children, item :: accestors )
    in
        case (List.head matches) of
            Just node ->
                let
                    ( _, item, children ) =
                        Node.toTuple node
                in
                    ( children, item :: accestors )

            Nothing ->
                List.foldr
                    (\child acc ->
                        if (List.length (subMatches child |> fst)) > 0 then
                            subMatches child
                        else
                            acc
                    )
                    ( [], [] )
                    tree
