module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Chae.Tree exposing (..)
import Chae.Node exposing (node)


all : Test
all =
    describe "Tree"
        [ subTreeForTests ]


subTreeForTests : Test
subTreeForTests =
    let
        items =
            [ { id = 1, name = "first", parentIds = [] }
            , { id = 2, name = "child", parentIds = [ 1 ] }
            , { id = 3, name = "dep categories", parentIds = [ 2 ] }
            ]

        toId id =
            toString id

        itemId item =
            toId (.id item)

        itemParentIds item =
            .parentIds item |> List.map toId

        tree =
            fromList itemId itemParentIds items
    in
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
                                    { id = 3, name = "dep categories", parentIds = [ 2 ] }
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
                                { id = 3, name = "dep categories", parentIds = [ 2 ] }
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
                        , [ { id = 3, name = "dep categories", parentIds = [ 2 ] }
                          , { id = 2, name = "child", parentIds = [ 1 ] }
                          , { id = 1, name = "first", parentIds = [] }
                          ]
                        )
            ]
