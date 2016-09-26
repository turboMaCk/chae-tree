module ChaeTree.Node
    exposing
        ( Node
        , singleton
        , node
        , simple
        , id
        , root
        , addChild
        , children
        , hasChildren
        , toTuple
        , map
        , map2
        , foldr
        , sum
        , product
        , pushDeep
        )

{-| Manipulationg with `None` based data structure

@docs Node, singleton, simple, node, id, root, addChild, children, hasChildren, toTuple, map, map2, foldr, sum, product, pushDeep

-}

import List
import Maybe exposing (Maybe(..))
import ChaeTree.Id exposing (..)


-- Types


{-| Node
-}
type Node a
    = Node Id a (List (Node a))



-- Common Helpers


{-| Create empty `Node` for given values.
First paramter is function which takes given value and return it's id.

    singleton toId 1 == Node "1" 1 []
    singleton toId { a = "b" } == Node "{ a = \"b\" }" { a = "b" } []
    singleton (\a -> .id a) { id = "1" } == Node "1" { id = "1" } []
    singleton (\a -> .id a |> toId ) { id = 1 } == Node "1" { id = 1 } []
-}
singleton : (a -> Id) -> a -> Node a
singleton getId item =
    Node (getId item) item []

{-| Create node manualy
-}
node : a -> b -> List (Node b) -> Node b
node id a c =
  Node (toId id) a c

{-| Simple node creation
-}
simple : a -> List (Node a) -> Node a
simple a =
  node a a

{-| Get id of given `Node`.

    id (createNode toId 1) == "1"
    id (createNode (\_ -> "uid") { a = "a"} ) == "uid"
-}
id : Node a -> Id
id (Node id _ _) =
    id


{-| Turns `Node` back to value it was created from.

This function provide recommended way to access user space data while working with tree.

    root (createNode toId "Elm") == "Elm"
    root (createNode (\i -> .id i) { id = "1", name = "Elm" }) == { id = "1", name = "Elm" }
-}
root : Node a -> a
root (Node _ a _) =
    a


{-| Return `Node` with item add as sub `Node`.
First argument is function from item to `Id/String`.

    addChild toId 2 (singleton toId 1) == Node "1" 1 ([Node "2" 2 []])
    addChild toId 3 (addChild toId 2 (singleton toId 1)) == Node "1" 1 ([Node "3" 3 [], Node "2" 2 []])
-}
addChild : (a -> Id) -> a -> Node a -> Node a
addChild getId item (Node id a children) =
    let
        node =
            Node (getId item) item []
    in
        (Node id a (node :: children))


{-| Get child tree of `Node`.
This is common way to access sub tree of given node.

    children (singleton toId 1) == []
    children (addChild toId 2 (singleton toId 1)) == [Node "2" 2 []]
-}
children : Node a -> List (Node a)
children (Node _ _ children) =
    children


{-| Check if tree has children

    hasChildren (singleton toId 1) == False
    hasChildren (addChild toId 2 (singleton toId 1)) == True
-}
hasChildren : Node a -> Bool
hasChildren tree =
    children tree |> List.isEmpty |> not


{-| Transform node to tupple of

- id (`String`) - Id of node
- item inside node
- Children (`List (Node a)`) - child nodes
-}
toTuple : Node a -> (Id, a, List (Node a))
toTuple (Node id a c) =
  (id, a, c)


-- Common operations


{-| Map function on tree
produces new modified tree

   map toId ((+) 1) (addChild toId 2 (singleton toId 1)) == Node "1" 2 ([Node "2" 3 []])
   map (\n -> n + 1 |> toId) ((+) 1) (addChild toId 2 (singleton toId 1)) == Node "2" 2 ([Node "3" 3 []])
-}
map : (a -> Id) -> (a -> b) -> Node a -> Node b
map getId fc (Node _ a c) =
    Node (getId a) (fc a) (List.map (map getId fc) c)


{-|
-}
map2 : (a -> b -> Id) -> (a -> b -> c) -> Node a -> Node b -> Node c
map2 getId fc (Node _ a ca) (Node _ b cb) =
    Node (getId a b) (fc a b) (List.map2 (map2 getId fc) ca cb)


{-|
-}
zip : (a -> b -> Id) -> Node a -> Node b -> Node ( a, b )
zip getId =
    map2 getId (,)


{-|
    foldr (+) 0 (addChild toId 20 (singleton toId 1)) == 21
    folr (*) 1 (addChild toId 3 (singleton toId 4)) == 12
-}
foldr : (a -> b -> b) -> b -> Node a -> b
foldr reducer b (Node _ a c) =
    List.foldr (flip (foldr reducer)) (reducer a b) c


{-|
    sum (addChild toId 20 (singleton toId 1)) == 21
-}
sum : Node number -> number
sum =
    foldr (+) 0


{-|
    product (addChild toId 3 (singleton toId 4)) == 12
-}
product : Node number -> number
product =
    foldr (*) 1


{-| push deep
-}
pushDeep : (a -> Id) -> Id -> a -> Node a -> Node a
pushDeep idGet id item ((Node nodeId a children) as node) =
    if nodeId == id then
        addChild idGet item node
    else
        Node nodeId a (List.map (pushDeep idGet id item) children)
