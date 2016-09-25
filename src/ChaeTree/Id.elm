module ChaeTree.Id exposing (Id, toId)

{-| This module implements basic `Id` type manipulations
that are used across other data types.

# Type
@docs Id

# Essential
@docs toId

-}


{-| `Id` is just alias for `String` type
-}
type alias Id =
    String


{-| Convert any value to `Id` type.
This is just alias for `toString` function.

    toId "str" = "str"
    toId 1 = "1"
    toId = "{ a = \"a\" }"
-}
toId : a -> Id
toId =
    toString
