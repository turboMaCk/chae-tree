module Main exposing (..)

import List exposing (..)
import Html exposing (..)
import Html.Events as Events


-- Lib import

import Chae.Id as Id exposing (Id)
import Chae.Node as Node exposing (Node)
import Chae.Tree as Tree exposing (Tree)


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
    { items : Tree Item, opened : List Id }


initialModel : Model
initialModel =
    { items = Tree.fromList (.id) (.parentIds) items
    , opened = List.map .id items
    }


init : ( Model, Cmd Msg )
init =
    initialModel ! []



-- Update


type Msg
    = NoOp
    | Toggle Id


update : Msg -> Model -> ( Model, Cmd Msg )
update cmd model =
    case cmd of
        NoOp ->
            model ! []

        Toggle id ->
            case partition (\o -> o == id) model.opened of
                ( [], rest ) ->
                    { model | opened = id :: rest } ! []

                ( _, rest ) ->
                    { model | opened = rest } ! []



-- View


isOpened : List Id -> Node a -> Bool
isOpened list node =
    member (Node.id node) list


itemView : Model -> Node Item -> Html Msg
itemView model node =
    let
        item =
            Node.root node

        open =
            isOpened (.opened model) node

        symbol =
            if length (Node.children node) > 0 then
                if open then
                    "[-] "
                else
                    "[+] "
            else
                "[ ] "
    in
        li []
            [ a [ Events.onClick (Toggle (Node.id node)) ]
                [ text (symbol ++ item.name) ]
            , if open then
                listView model (Node.children node)
              else
                text ""
            ]


listView : Model -> Tree Item -> Html Msg
listView model items =
    ul []
        (List.map (\n -> itemView model n) items)


view : Model -> Html Msg
view model =
    listView model (.items model)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
