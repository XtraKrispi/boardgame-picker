module App exposing (main)

{-| This Application will allow multiple people to combine their collections or individual games together and
select one randomly given a set of parameters

@docs main
The main function which executes the application
-}

import Html exposing (Html, div, text, h1, input, label, button, form)
import Html.Attributes exposing (class, type_, name, checked, placeholder)
import Html.Events exposing (onClick, onSubmit, onInput)


type alias Model =
    { selectionType : SelectionType
    , selection : String
    }


type Msg
    = AddFromCollectionSelected
    | AddGameSelected
    | ChangeSelection String
    | Search


type SelectionType
    = AddFromCollection
    | AddGame


{-| Main execution of the application
-}
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { selectionType = AddFromCollection, selection = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        AddFromCollectionSelected ->
            { m | selectionType = AddFromCollection } ! [ Cmd.none ]

        AddGameSelected ->
            { m | selectionType = AddGame } ! [ Cmd.none ]

        ChangeSelection s ->
            { m | selection = s } ! [ Cmd.none ]

        Search ->
            m ! [ Cmd.none ]


view : Model -> Html Msg
view model =
    layout <| mainHeader model


layout : Html Msg -> Html Msg
layout body =
    div [ class "container" ] [ h1 [] [ text "Board Game Picker" ], body ]


radioButton : String -> String -> Bool -> Msg -> Html Msg
radioButton name_ label_ checked_ msg =
    div [ class "radio" ]
        [ label []
            [ input [ type_ "radio", name name_, onClick msg, checked checked_ ] []
            , (text label_)
            ]
        ]


mainHeader : Model -> Html Msg
mainHeader model =
    div []
        [ --   radioButton "selection" "Add from a collection" (model.selectionType == AddFromCollection) AddFromCollectionSelected
          -- , radioButton "selection" "Add a game" (model.selectionType == AddGame) AddGameSelected
          -- ,
          searchBar model
        ]


searchBar : Model -> Html Msg
searchBar model =
    let
        placeholderText =
            case model.selectionType of
                AddFromCollection ->
                    "Please enter a BoardGameGeek user"

                AddGame ->
                    "Please enter a game"
    in
        form [ onSubmit Search ]
            [ div [ class "row" ]
                [ div [ class "col-sm-10" ]
                    [ input [ type_ "text", class "form-control", placeholder placeholderText, onInput ChangeSelection ] []
                    ]
                , div [ class "col-sm-2" ]
                    [ button [ type_ "button", class "btn btn-primary" ] [ text "Search" ]
                    ]
                ]
            ]
