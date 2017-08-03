module Tests.Tree exposing (all)

import Test exposing (..)
import Expect
import String


-- Library

import Chae.Tree as Tree exposing (..)
import Chae.Node as Node exposing (node)


-- Exposed


all : Test
all =
    describe "Tree"
        [ functorTest
        , filterTest
        , deepFilterTest
        , pushTest
        , fromListTest
        , subTreeForTests
        ]



-- Fixtures


items =
    [ { id = 1, name = "first", parentIds = [] }
    , { id = 2, name = "child", parentIds = [ 1 ] }
    , { id = 3, name = "deep child", parentIds = [ 2 ] }
    ]


toId id =
    toString id


itemId item =
    toId (.id item)


itemParentIds item =
    .parentIds item |> List.map toId


tree =
    fromList itemId itemParentIds items



-- Tests


functorTest : Test
functorTest =
    describe "Act like functor"
        [ test "1st law" <|
            \() ->
                Expect.equal
                    (Tree.map identity tree)
                    (identity tree)
        , test "2nd law" <|
            \() ->
                let
                    fc1 =
                        toString

                    fc2 str =
                        "Hi " ++ str
                in
                    Expect.equal
                        (Tree.map (fc2 << fc1) tree)
                        (Tree.map fc1 tree |> Tree.map fc2)
        ]


filterTest : Test
filterTest =
    let
        tree =
            [ Node.node "5" 5 [ Node.singleton "1" 1, Node.singleton "10" 10 ] ]
    in
        describe "filter"
            [ test "< 4" <|
                \() ->
                    Expect.equal
                        (Tree.filter ((<) 4) tree)
                        ([ node "5" 5 [ node "10" 10 [] ] ])
            , test "< 6" <|
                \() ->
                    Expect.equal
                        (Tree.filter ((<) 6) tree)
                        []
            , test "< 0" <|
                \() ->
                    Expect.equal
                        (Tree.filter ((<) 0) tree)
                        tree
            ]


deepFilterTest : Test
deepFilterTest =
    let
        tree =
            [ Node.node "5" 5 [ Node.node "1" 1 [ Node.singleton "9" 9 ], Node.singleton "10" 10 ] ]
    in
        describe "filter node OR children"
            [ test "< 6" <|
                \() ->
                    Expect.equal
                        (Tree.deepFilter ((<) 6) tree)
                        ([ Node.node "5" 5 ([ Node.node "1" 1 ([ Node.node "9" 9 [] ]), Node.node "10" 10 [] ]) ])
            , test "< 11" <|
                \() ->
                    Expect.equal
                        (Tree.deepFilter ((<) 11) tree)
                        []
            , test "< 0" <|
                \() ->
                    Expect.equal
                        (Tree.deepFilter ((<) 0) tree)
                        tree
            ]


pushTest : Test
pushTest =
    describe "push"
        [ test "to empty" <|
            \() ->
                Expect.equal
                    (push toString Nothing 4 [])
                    ([ node "4" 4 [] ])
        , test "to root of existing" <|
            \() ->
                let
                    item =
                        { id = 4, name = "new", parentIds = [] }
                in
                    Expect.equal
                        (Tree.filter (\i -> i.id == 4) <| push itemId Nothing item tree)
                        ([ node "4" item [] ])
        , test "to deep level" <|
            \() ->
                let
                    item =
                        { id = 4, name = "new", parentIds = [ 3 ] }
                in
                    Expect.equal
                        (push itemId (Just "3") item tree)
                        ([ node
                            "1"
                            { id = 1
                            , name = "first"
                            , parentIds = []
                            }
                            [ node
                                "2"
                                { id = 2
                                , name = "child"
                                , parentIds = [ 1 ]
                                }
                                [ node
                                    "3"
                                    { id = 3
                                    , name = "deep child"
                                    , parentIds = [ 2 ]
                                    }
                                    [ node
                                        "4"
                                        { id = 4
                                        , name = "new"
                                        , parentIds = [ 3 ]
                                        }
                                        []
                                    ]
                                ]
                            ]
                         ]
                        )
        ]


fromListTest : Test
fromListTest =
    describe "fromList"
        [ test "resolve fixtures" <|
            \() ->
                Expect.equal
                    tree
                    ([ node
                        "1"
                        { id = 1
                        , name = "first"
                        , parentIds = []
                        }
                        [ node
                            "2"
                            { id = 2
                            , name = "child"
                            , parentIds = [ 1 ]
                            }
                            [ node
                                "3"
                                { id = 3
                                , name = "deep child"
                                , parentIds = [ 2 ]
                                }
                                []
                            ]
                        ]
                     ]
                    )
        ]


subTreeForTests : Test
subTreeForTests =
    describe "subTreeFor"
        [ test "no id" <|
            \() ->
                Expect.equal (subTreeFor Nothing tree) ( tree, [] )
        , test "root id" <|
            \() ->
                Expect.equal
                    (subTreeFor (Just "1") tree)
                    ( [ node
                            "2"
                            { id = 2, name = "child", parentIds = [ 1 ] }
                            ([ node
                                "3"
                                { id = 3, name = "deep child", parentIds = [ 2 ] }
                                []
                             ]
                            )
                      ]
                    , [ { id = 1, name = "first", parentIds = [] } ]
                    )
        , test "id the middle of the tree" <|
            \() ->
                Expect.equal
                    (subTreeFor (Just "2") tree)
                    ( [ node
                            "3"
                            { id = 3, name = "deep child", parentIds = [ 2 ] }
                            []
                      ]
                    , [ { id = 2, name = "child", parentIds = [ 1 ] }
                      , { id = 1, name = "first", parentIds = [] }
                      ]
                    )
        , test "leaf id" <|
            \() ->
                Expect.equal
                    (subTreeFor (Just "3") tree)
                    ( []
                    , [ { id = 3, name = "deep child", parentIds = [ 2 ] }
                      , { id = 2, name = "child", parentIds = [ 1 ] }
                      , { id = 1, name = "first", parentIds = [] }
                      ]
                    )
        ]
