module Tests exposing (all)

import Test exposing (..)
import List
import Tests.Tree


all : Test
all =
    describe "All"
        [ Tests.Tree.all ]
