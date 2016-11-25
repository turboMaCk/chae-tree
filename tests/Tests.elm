module Tests exposing (all)

import Test exposing (..)
import Tests.Tree


all : Test
all =
    describe "All"
        [ Tests.Tree.all ]
