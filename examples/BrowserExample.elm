module Main exposing (..)

import List exposing (..)
import Html exposing (..)
import Html.App
import Html.Events as Events
import Maybe


-- Lib import

import ChaeTree as CT


-- Model


type alias Item =
    { id : String, name : String, parentIds : List String }


items : List Item
items =
    [ { id = "a", name = "root", parentIds = [] }
    , { id = "b", name = "next root", parentIds = [] }
    , { id = "c", name = "nested", parentIds = [ "b" ] }
    , { id = "d", name = "deep nested", parentIds = [ "c" ] }
    ]


type alias Model =
    { items : CT.Tree Item, opened : List CT.Id }


initialModel : Model
initialModel =
    { items = CT.fromList (.id) (.parentIds) items
    , opened = map .id items
    }


init : ( Model, Cmd Msg )
init =
    initialModel ! []



-- Update


type Msg
    = NoOp
    | Toggle (Maybe CT.Id)


update : Msg -> Model -> ( Model, Cmd Msg )
update cmd model =
    case cmd of
        NoOp ->
            model ! []

        Toggle maybeId ->
            case maybeId of
                Nothing ->
                    model ! []

                Just id ->
                    case partition (\o -> o == id) (.opened model) of
                        ( [], rest ) ->
                            { model | opened = id :: rest } ! []

                        ( _, rest ) ->
                            { model | opened = rest } ! []



-- View


isOpened list node =
    member (CT.getId node) list


itemView : Model -> CT.Node Item -> Html Msg
itemView model node =
    let
        item =
            CT.unpack node

        open =
            isOpened (.opened model) node

        symbol =
            if length (CT.getSubTree node) > 0 then
                if open then
                    "[-] "
                else
                    "[+] "
            else
                "[ ] "
    in
        li []
            [ a [ Events.onClick (Toggle (Just (CT.getId node))) ]
                [ text (symbol ++ (.name item)) ]
            , if open then
                listView model (CT.getSubTree node)
              else
                text ""
            ]


listView : Model -> CT.Tree Item -> Html Msg
listView model items =
    ul []
        (map (\n -> itemView model n) items)


view : Model -> Html Msg
view model =
    listView model (.items model)


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
