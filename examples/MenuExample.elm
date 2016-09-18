-- Common imports


module Main exposing (..)

import List exposing (..)
import Html exposing (..)
import Html.App
import Html.Events as Events
import Maybe


-- Lib import

import ChaeTree as CT


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
    , { id = 3, name = "dep categories", parentId = Just 2 }
    ]


questions : List Question
questions =
    [ { id = 1, name = "root q", categoryIds = [] }
    , { id = 2, name = "in cat", categoryIds = [ 1 ] }
    , { id = 3, name = "almost last", categoryIds = [ 2 ] }
    , { id = 4, name = "last", categoryIds = [ 2, 3 ] }
    ]


getId : Either Category Question -> CT.Id
getId thing =
    case thing of
        Left category ->
            CT.toId (.id category)

        Right question ->
            CT.toId (.id question)


getParentIds : Either Category Question -> List CT.Id
getParentIds thing =
    case thing of
        Left category ->
            case .parentId category of
                Just id ->
                    [ CT.toId id ]

                Nothing ->
                    []

        Right question ->
            map (\id -> CT.toId id) (.categoryIds question)


list : List (Either Category Question)
list =
    (map (\c -> Left c) categories) ++ map (\q -> Right q) questions


tree : CT.Tree (Either Category Question)
tree =
    CT.fromList getId getParentIds list


type alias Model =
    { activeCategoryId : Maybe String
    , tree : CT.Tree (Either Category Question)
    }


initialModel : Model
initialModel =
    { activeCategoryId = Nothing, tree = tree }



-- Upadte


type Msg
    = NoOp
    | Open (Maybe CT.Id)


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


itemView : CT.Node (Either Category Question) -> Html Msg
itemView node =
    case CT.unpack node of
        Right question ->
            li [] [ text (">> " ++ (.name question)) ]

        Left category ->
            li []
                [ a [ Events.onClick (Open (Just (CT.getId node))) ]
                    [ text (.name category ++ " >") ]
                ]


menuView : CT.Tree (Either Category Question) -> Html Msg
menuView list =
    ul []
        (map itemView list)


ancestorView : Either Category Question -> Html Msg
ancestorView thing =
    let
        openArg category =
            case (.parentId category) of
                Just id ->
                    Just (CT.toId id)

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


menu : ( CT.Tree (Either Category Question), List (Either Category Question) ) -> Html Msg
menu ( tree, categories ) =
    div []
        [ ancestorsView categories
        , menuView tree
        ]


view : Model -> Html Msg
view model =
    menu (CT.subTreeFor (.activeCategoryId model) (.tree model))


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
