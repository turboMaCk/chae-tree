module Tests.Tree exposing (all)

import Test exposing (..)
import Expect
import String


-- Library

import Chae.Tree exposing (..)
import Chae.Node exposing (node)


-- Exposed


all : Test
all =
    describe "Tree"
        [ fromListTest
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



-- Test


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
