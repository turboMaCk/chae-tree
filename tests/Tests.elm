module Tests exposing (all)

import Test exposing (..)
import Tests.Tree
import Tests.Node


all : Test
all =
    describe "All"
        [ Tests.Tree.all
        , Tests.Node.all ]
