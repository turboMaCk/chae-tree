module Tests.Node exposing (all)

import Test exposing (..)
import Expect


-- Library

import Chae.Node as Node


all : Test
all =
    describe "Node"
        [ functorTest ]



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
