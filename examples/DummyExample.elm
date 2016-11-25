module Main exposing (..)

import List exposing (..)
import Html exposing (..)
import Maybe
import Chae.Id as Id
import Chae.Tree as Tree


tree1 : Tree.Tree number
tree1 =
    Tree.push Id.toId Nothing 1 []
        |> Tree.push Id.toId (Just "1") 2
        |> Tree.push Id.toId (Just "2") 3


list : List { id : number, pid : List number1 }
list =
    [ { id = 1, pid = [] }, { id = 2, pid = [] }, { id = 3, pid = [ 1 ] } ]


tree2 : Tree.Tree { id : number, pid : List number1 }
tree2 =
    Tree.fromList (\a -> Id.toId (.id a)) (\a -> .pid a |> List.map Id.toId) list


main : Html msg
main =
    div []
        [ div []
            [ text (tree1 |> toString) ]
        , div []
            [ text (tree2 |> toString) ]
        ]
