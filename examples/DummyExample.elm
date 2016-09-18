module Main exposing (..)

import List exposing (..)
import Html exposing (..)
import Maybe
import ChaeTree as CT


tree1 =
    CT.push CT.toId Nothing 1 []
        |> CT.push CT.toId (Just "1") 2
        |> CT.push CT.toId (Just "2") 3


list =
    [ { id = 1, pid = [] }, { id = 2, pid = [] }, { id = 3, pid = [ 1 ] } ]


tree2 =
    CT.fromList (\a -> CT.toId (.id a)) (\a -> .pid a |> map CT.toId) list


main : Html msg
main =
    div []
        [ div []
            [ text (tree1 |> toString) ]
        , div []
            [ text (tree2 |> toString) ]
        ]
