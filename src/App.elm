module App exposing (main)

{-| This Application will allow multiple people to combine their collections or individual games together and
select one randomly given a set of parameters

@docs main
The main function which executes the application
-}

import Html exposing (Html, div, text, h1, input, label, button, form, img)
import Html.Attributes exposing (class, type_, name, checked, placeholder, src)
import Html.Events exposing (onClick, onSubmit, onInput)
import Json.Decode.Pipeline exposing (decode, required, custom)
import Json.Decode exposing (Decoder, int, string, float, bool, list)
import Http


type alias Model =
    { selectionType : SelectionType
    , selection : String
    , searched : Bool
    , loading : Bool
    , lastSearchResults : List Game
    , lastError : String
    }


type Msg
    = AddFromCollectionSelected
    | AddGameSelected
    | ChangeSelection String
    | Search
    | CollectionLookupFailed Http.Error
    | CollectionLookupSucceeded (List Game)


type SelectionType
    = AddFromCollection
    | AddGame


type GameId
    = GameId Int


type alias Game =
    { id : GameId
    , name : String
    , largeImageUrl : String
    , thumbnailUrl : String
    , minPlayers : Int
    , maxPlayers : Int
    , playingTime : Int
    , year : Int
    , avgRating : Float
    , rank : Int
    }


type alias Username =
    String


type alias UserGame =
    { username : Username
    , isOwned : Bool
    , game : Game
    }


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
    ( { selectionType = AddFromCollection, selection = "", loading = False, lastSearchResults = [], lastError = "", searched = False }, Cmd.none )


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
            { m | loading = True, searched = True } ! [ getUserCollection (m.selection) ]

        CollectionLookupFailed err ->
            { m | lastError = toString err, loading = False } ! [ Cmd.none ]

        CollectionLookupSucceeded results ->
            { m | lastSearchResults = results, loading = False } ! [ Cmd.none ]


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
        , renderSearchResults model
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
                    [ input [ type_ "text", class "form-control", placeholder placeholderText, onInput ChangeSelection, Html.Attributes.required True ] []
                    ]
                , div [ class "col-sm-2" ]
                    [ button [ type_ "submit", class "btn btn-primary" ] [ text "Search" ]
                    ]
                ]
            ]


renderSearchResults : Model -> Html Msg
renderSearchResults { lastSearchResults, loading, searched } =
    let
        body =
            if loading then
                [ text "Loading..." ]
            else if searched && List.length lastSearchResults == 0 then
                [ text "No results found..." ]
            else
                List.map renderGame lastSearchResults
    in
        div [] body


renderGame : Game -> Html Msg
renderGame game =
    div [ class "game" ]
        [ div []
            [ img [ src game.thumbnailUrl ] [] ]
        , div []
            [ text game.name ]
        ]


collectionLookupUrl : Username -> String
collectionLookupUrl u =
    "https://bgg-json.azurewebsites.net/collection/" ++ u


gameDecoder : Decoder Game
gameDecoder =
    decode Game
        |> required "gameId" ((Json.Decode.map GameId) int)
        |> required "name" string
        |> required "image" string
        |> required "thumbnail" string
        |> required "minPlayers" int
        |> required "maxPlayers" int
        |> required "playingTime" int
        |> required "yearPublished" int
        |> required "averageRating" float
        |> required "rank" int


userGameDecoder : Username -> Decoder UserGame
userGameDecoder u =
    decode (UserGame u)
        |> required "owned" bool
        |> custom gameDecoder


getUserCollection : Username -> Cmd Msg
getUserCollection u =
    Http.get (collectionLookupUrl u) (list (userGameDecoder u))
        |> Http.send
            (\result ->
                case result of
                    Err err ->
                        CollectionLookupFailed err

                    Ok games ->
                        CollectionLookupSucceeded
                            (games
                                |> List.filter (\ug -> ug.isOwned)
                                |> List.map (\{ game } -> game)
                            )
            )
