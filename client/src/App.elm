module App exposing (main)

{-| This Application will allow multiple people to combine their collections or individual games together and
select one randomly given a set of parameters

@docs main
The main function which executes the application
-}

import Html exposing (Html, div, text, h1, input, label, button, form, img, h3)
import Html.Attributes exposing (class, type_, name, checked, placeholder, src, disabled, style)
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
    , selectedResults : List Game
    , allAvailableGames : List Game
    }


type Msg
    = AddFromCollectionSelected
    | AddGameSelected
    | ChangeSelection String
    | Search
    | CollectionLookupFailed Http.Error
    | CollectionLookupSucceeded (List Game)
    | SelectSearchedGame Game
    | NoOp


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
    ( { selectionType = AddFromCollection, selection = "", loading = False, lastSearchResults = [], lastError = "", searched = False, selectedResults = [], allAvailableGames = [] }, Cmd.none )


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

        SelectSearchedGame game ->
            if (isSelected m.selectedResults game) then
                { m | selectedResults = List.filter ((/=) game) m.selectedResults } ! [ Cmd.none ]
            else
                { m | selectedResults = m.selectedResults ++ [ game ] } ! [ Cmd.none ]

        NoOp ->
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
          div [ class "row" ] [ div [ class "col-sm-6" ] [ h3 [] [ text "Search" ], searchBar model, renderSearchResults model ], div [ class "col-sm-6" ] [ h3 [] [ text "Available Games" ], renderAvailableGames model ] ]
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


isSelected : List Game -> Game -> Bool
isSelected selectedResults g =
    (not << List.isEmpty) <| List.filter ((==) g) selectedResults


renderSearchResults : Model -> Html Msg
renderSearchResults { lastSearchResults, loading, searched, selectedResults } =
    let
        body =
            if loading then
                [ text "Loading..." ]
            else if searched && List.length lastSearchResults == 0 then
                [ text "No results found..." ]
            else if List.length lastSearchResults > 0 then
                [ div [ class "btn-group" ]
                    [ button [ class "btn btn-default" ] [ text "Add All Games" ]
                    , button [ class "btn btn-default", disabled (List.isEmpty selectedResults) ] [ text <| "Add " ++ (toString <| List.length selectedResults) ++ " Selected" ]
                    ]
                , div [ class "search-results" ] <| List.map (renderGame SelectSearchedGame (isSelected selectedResults)) lastSearchResults
                ]
            else
                []
    in
        div [ class "results-area" ] body


renderGame : (Game -> Msg) -> (Game -> Bool) -> Game -> Html Msg
renderGame action isSelected game =
    div
        [ class <|
            "game"
                ++ (if isSelected game then
                        " selected"
                    else
                        ""
                   )
        , onClick (action game)
        ]
        [ div [ class "thumb" ]
            [ img [ src game.thumbnailUrl ] []
            ]
        , div [ class "description" ]
            [ div [] [ text <| game.name ++ " (" ++ (toString game.year) ++ ")" ]
            , div [] [ text <| (toString game.minPlayers) ++ "-" ++ (toString game.maxPlayers) ++ " players" ]
            , div [] [ text <| (toString game.playingTime) ++ " minutes" ]
            , div [] [ text <| "BGG Rank: " ++ (toString game.rank) ]
            , div [] [ text <| "Avg Rating: " ++ (toString game.avgRating) ]
            ]
        ]


renderAvailableGames : Model -> Html Msg
renderAvailableGames { allAvailableGames } =
    if List.length allAvailableGames == 0 then
        div [] [ text "Please add at least one game..." ]
    else
        div [] <| List.map (renderGame (\_ -> NoOp) (always False)) allAvailableGames


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
