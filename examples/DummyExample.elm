module Main exposing (..)

import List exposing (..)
import Html exposing (..)
import Maybe

import Chae.Id as Id
import Chae.Node as Node
import Chae.Tree as Tree


tree1 =
    Tree.push Id.toId Nothing 1 []
        |> Tree.push Id.toId (Just "1") 2
        |> Tree.push Id.toId (Just "2") 3


list =
    [ { id = 1, pid = [] }, { id = 2, pid = [] }, { id = 3, pid = [ 1 ] } ]


tree2 =
    Tree.fromList (\a -> Id.toId (.id a)) (\a -> .pid a |> map Id.toId) list


main : Html msg
main =
    div []
        [ div []
            [ text (tree1 |> toString) ]
        , div []
            [ text (tree2 |> toString) ]
        ]
