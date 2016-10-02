module Chae.Id exposing (Id, toId)

{-| This module contains `Id` type implementation.

# Definition
@docs Id

# Constructor
@docs toId

-}


{-| -}
type alias Id =
    String


{-| Convert any value to `Id` type.
This is just alias for `toString` function.

    toId "str" = "str"
    toId 1 = "1"
    toId { a = "a" } = "{ a = \"a\" }"
-}
toId : a -> Id
toId =
    toString
