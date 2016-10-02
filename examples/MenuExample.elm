-- Common imports


module Main exposing (..)

import List exposing (..)
import Html exposing (..)
import Html.App
import Html.Events as Events
import Maybe


-- Lib import

import Chae.Id as Id exposing (Id)
import Chae.Node as Node exposing (Node)
import Chae.Tree as Tree exposing (Tree)


type Either a b
    = Left a
    | Right b



-- Model


type alias Question =
    { id : Int, name : String, categoryIds : List Int }


type alias Category =
    { id : Int, name : String, parentId : Maybe Int }


categories : List Category
categories =
    [ { id = 1, name = "first", parentId = Nothing }
    , { id = 2, name = "child", parentId = Just 1 }
    , { id = 3, name = "deep child", parentId = Just 2 }
    ]


questions : List Question
questions =
    [ { id = 1, name = "root item", categoryIds = [] }
    , { id = 2, name = "item with parent", categoryIds = [ 1 ] }
    , { id = 3, name = "deeper nested item", categoryIds = [ 2 ] }
    , { id = 4, name = "item with two parents", categoryIds = [ 2, 3 ] }
    ]


getId : Either Category Question -> Id
getId thing =
    case thing of
        Left category ->
            Id.toId category.id

        Right question ->
            Id.toId question.id


getParentIds : Either Category Question -> List Id
getParentIds thing =
    case thing of
        Left category ->
            case category.parentId of
                Just id ->
                    [ Id.toId id ]

                Nothing ->
                    []

        Right question ->
            map (\id -> Id.toId id) question.categoryIds


list : List (Either Category Question)
list =
    (map (\c -> Left c) categories) ++ map (\q -> Right q) questions


tree : Tree (Either Category Question)
tree =
    Tree.fromList getId getParentIds list


type alias Model =
    { activeCategoryId : Maybe String
    , tree : Tree (Either Category Question)
    }


initialModel : Model
initialModel =
    { activeCategoryId = Nothing, tree = tree }



-- Upadte


type Msg
    = NoOp
    | Open (Maybe Id)


init : ( Model, Cmd Msg )
init =
    initialModel ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Open id ->
            { model | activeCategoryId = id } ! []



-- View


itemView : Node (Either Category Question) -> Html Msg
itemView node =
    case Node.root node of
        Right question ->
            li [] [ text ("item: " ++ (.name question)) ]

        Left category ->
            li []
                [ a [ Events.onClick (Open (Just (Node.id node))) ]
                    [ text ("category: " ++ category.name ++ " >") ]
                ]


menuView : Tree (Either Category Question) -> Html Msg
menuView list =
    ul []
        (map itemView list)


ancestorView : Either Category Question -> Html Msg
ancestorView thing =
    let
        openArg category =
            case (.parentId category) of
                Just id ->
                    Just (Id.toId id)

                Nothing ->
                    Nothing
    in
        case thing of
            Right _ ->
                text ""

            Left category ->
                li
                    []
                    [ a [ Events.onClick (Open (openArg category)) ]
                        [ text (.name category ++ " ^") ]
                    ]


ancestorsView : List (Either Category Question) -> Html Msg
ancestorsView things =
    ul []
        (map (\c -> ancestorView c) (reverse things))


menu : ( Tree (Either Category Question), List (Either Category Question) ) -> Html Msg
menu ( tree, categories ) =
    div []
        [ ancestorsView categories
        , menuView tree
        ]


view : Model -> Html Msg
view model =
    menu (Tree.subTreeFor model.activeCategoryId model.tree)


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
