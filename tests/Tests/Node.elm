module Tests.Node exposing (all)

import Test exposing (..)
import Expect


-- Library

import Chae.Node as Node


all : Test
all =
    describe "Node"
        [ functorTest
        , applicativeTest
        ]



-- Tests


functorTest : Test
functorTest =
    let
        node : Node.Node Int
        node =
            Node.node "1" 1 [ Node.singleton "2" 2 ]
    in
        describe "Act like functor"
            [ test "1st law" <|
                \() ->
                    Expect.equal
                        (Node.map identity node)
                        (identity node)
            , test "2nd law" <|
                \() ->
                    let
                        fc1 =
                            toString

                        fc2 str =
                            "Hi " ++ str
                    in
                        Expect.equal
                            (Node.map (fc2 << fc1) node)
                            (Node.map fc1 node |> Node.map fc2)
            ]


applicativeTest : Test
applicativeTest =
    let
        node : Node.Node Int
        node =
            Node.node "1" 1 [ Node.singleton "2" 2 ]
    in
        describe "Act like applicative"
            [ test "1st law" <|
                \() ->
                    Expect.equal (Node.pure identity |> Node.andMap node) node
            , test "2nd law" <|
                \() ->
                    Expect.equal
                        (Node.pure (<<)
                            |> Node.andMap (Node.pure ((+) 5))
                            |> Node.andMap (Node.pure ((-) 2))
                            |> Node.andMap node
                        )
                        (Node.pure ((+) 5)
                            |> Node.andMap (Node.pure ((-) 2) |> Node.andMap node)
                        )
            , test "3rd law" <|
                \() ->
                    Expect.equal
                        (Node.pure ((+) 3)
                            |> Node.andMap (Node.pure 1)
                        )
                        (Node.pure <| ((+) 3) 1)
            ]
