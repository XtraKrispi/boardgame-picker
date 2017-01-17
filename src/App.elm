module App exposing (main)

{-| This Application will allow multiple people to combine their collections or individual games together and
select one randomly given a set of parameters

@docs main
The main function which executes the application
-}

import Html exposing (Html, div, text, h1, input, label, button, form, img, h3, span, i, footer, p, a)
import Html.Attributes exposing (class, type_, name, checked, placeholder, src, disabled, style, href)
import Html.Events exposing (onClick, onSubmit, onInput)
import Json.Decode.Pipeline exposing (decode, required, custom)
import Json.Decode exposing (Decoder, int, string, float, bool, list)
import Http
import Random


type alias Model =
    { selectionType : SelectionType
    , selection : String
    , searched : Bool
    , loading : Bool
    , lastSearchResults : List Game
    , lastError : String
    , selectedResults : List Game
    , allAvailableGames : List Game
    , numberOfPlayers : NumberOfPlayers
    , playTime : PlayTime
    , availableGamesFromCriteria : List Game
    , chosenGame : GameRandomlyChosen
    }


type Msg
    = AddFromCollectionSelected
    | AddGameSelected
    | ChangeSelection String
    | Search
    | CollectionLookupFailed Http.Error
    | CollectionLookupSucceeded (List Game)
    | SelectSearchedGame Game
    | AddAllGames
    | AddSelectedGames
    | RemoveGame Game
    | ChooseAGame
    | GameNotChosen
    | GameChosen Game
    | UpdateNumberOfPlayers (Maybe Int)
    | UpdatePlayTime (Maybe Int)
    | RemoveAllSelectedGames
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


type NumberOfPlayers
    = NumberOfPlayers Int
    | NoPlayerRestriction


type PlayTime
    = PlayTime Int
    | NoPlayTimeRestriction


type GameRandomlyChosen
    = NotChosen
    | CouldntChoose
    | Chosen Game


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
    ( { selectionType = AddFromCollection
      , selection = ""
      , loading = False
      , lastSearchResults = []
      , lastError = ""
      , searched = False
      , selectedResults = []
      , allAvailableGames = []
      , numberOfPlayers = NoPlayerRestriction
      , playTime = NoPlayTimeRestriction
      , availableGamesFromCriteria = []
      , chosenGame = NotChosen
      }
    , Cmd.none
    )


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
            { m | loading = True, searched = True, lastError = "" } ! [ getUserCollection (m.selection) ]

        CollectionLookupFailed err ->
            { m | lastError = toString err, loading = False } ! [ Cmd.none ]

        CollectionLookupSucceeded results ->
            { m | lastSearchResults = results, loading = False, lastError = "" } ! [ Cmd.none ]

        SelectSearchedGame game ->
            if (isSelected m.selectedResults game) then
                { m | selectedResults = List.filter ((/=) game) m.selectedResults } ! [ Cmd.none ]
            else
                { m | selectedResults = m.selectedResults ++ [ game ] } ! [ Cmd.none ]

        AddAllGames ->
            { m | allAvailableGames = m.allAvailableGames ++ (except m.lastSearchResults m.allAvailableGames), selectedResults = [] } ! [ Cmd.none ]

        AddSelectedGames ->
            { m | allAvailableGames = m.allAvailableGames ++ (except m.selectedResults m.allAvailableGames), selectedResults = [] } ! [ Cmd.none ]

        RemoveGame game ->
            { m | allAvailableGames = List.filter ((/=) game) m.allAvailableGames } ! [ Cmd.none ]

        ChooseAGame ->
            let
                filteredGames =
                    filterApplicableGames m.playTime m.numberOfPlayers m.allAvailableGames
            in
                m ! [ getRandomGame filteredGames ]

        GameNotChosen ->
            { m | chosenGame = CouldntChoose } ! [ Cmd.none ]

        GameChosen game ->
            { m | chosenGame = Chosen game } ! [ Cmd.none ]

        UpdateNumberOfPlayers Nothing ->
            { m | numberOfPlayers = NoPlayerRestriction } ! [ Cmd.none ]

        UpdateNumberOfPlayers (Just t) ->
            { m | numberOfPlayers = NumberOfPlayers t } ! [ Cmd.none ]

        UpdatePlayTime Nothing ->
            { m | playTime = NoPlayTimeRestriction } ! [ Cmd.none ]

        UpdatePlayTime (Just t) ->
            { m | playTime = PlayTime t } ! [ Cmd.none ]

        RemoveAllSelectedGames ->
            { m | allAvailableGames = [] } ! [ Cmd.none ]

        NoOp ->
            m ! [ Cmd.none ]


filterApplicableGames : PlayTime -> NumberOfPlayers -> List Game -> List Game
filterApplicableGames playTime numberOfPlayers =
    let
        filterPlayTime { playingTime } =
            case playTime of
                NoPlayTimeRestriction ->
                    True

                PlayTime n ->
                    playingTime <= n

        filterNumberOfPlayers { minPlayers, maxPlayers } =
            case numberOfPlayers of
                NoPlayerRestriction ->
                    True

                NumberOfPlayers t ->
                    minPlayers <= t && maxPlayers >= t
    in
        List.filter (\game -> filterPlayTime game && filterNumberOfPlayers game)


getRandomGame : List Game -> Cmd Msg
getRandomGame games =
    let
        cmdFn gs n =
            case (List.drop n gs) of
                [] ->
                    GameNotChosen

                x :: xs ->
                    GameChosen x
    in
        Random.generate (cmdFn games) (Random.int 0 (List.length games - 1))


view : Model -> Html Msg
view model =
    layout <| mainHeader model


layout : Html Msg -> Html Msg
layout body =
    div []
        [ div [ class "container" ] [ h1 [] [ text "Board Game Picker" ], body ]
        , renderFooter
        ]


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
          div
            [ if model.lastError /= "" then
                class "panel panel-danger"
              else
                class ""
            ]
            [ div [ class "panel-body" ]
                [ text model.lastError ]
            ]
        , div [ class "row" ]
            [ div [ class "col-sm-6" ]
                [ h3 [] [ text "Search" ]
                , searchBar model
                , renderSearchResults model
                ]
            , div [ class "col-sm-6" ]
                [ h3 [] [ text "Available Games" ]
                , renderAvailableGames model
                ]
            ]
        , div [ class "option-area" ] <|
            if List.length model.allAvailableGames > 0 then
                [ renderOptionsArea ]
            else
                []
        , div [ class "results-area" ] <|
            case model.chosenGame of
                NotChosen ->
                    []

                CouldntChoose ->
                    [ text "Couldn't find a game that matches the given criteria!" ]

                Chosen game ->
                    [ renderGame (\_ -> NoOp) (always False) game ]
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
renderSearchResults { lastSearchResults, loading, searched, selectedResults, allAvailableGames } =
    let
        body =
            if loading then
                [ text "Loading..." ]
            else if searched && List.length lastSearchResults == 0 then
                [ text "No results found..." ]
            else if List.length lastSearchResults > 0 then
                [ div [ class "btn-group" ]
                    [ button [ class "btn btn-default", onClick AddAllGames ] [ text <| "Add All Games (" ++ (toString <| List.length lastSearchResults) ++ ")" ]
                    , button [ class "btn btn-default", disabled (List.isEmpty selectedResults), onClick AddSelectedGames ] [ text <| "Add " ++ (toString <| List.length selectedResults) ++ " Selected" ]
                    ]
                , div [ class "search-results" ] <| List.map (renderGame SelectSearchedGame (isSelected selectedResults)) (List.sortBy .name <| except lastSearchResults allAvailableGames)
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
            , div []
                [ text <|
                    (if game.minPlayers == game.maxPlayers then
                        (toString game.minPlayers)
                     else
                        (toString game.minPlayers) ++ "-" ++ (toString game.maxPlayers)
                    )
                        ++ " players"
                ]
            , div [] [ text <| (toString game.playingTime) ++ " minutes" ]
            , div [] [ text <| "BGG Rank: " ++ (toString game.rank) ]
            , div [] [ text <| "Avg Rating: " ++ (toString game.avgRating) ]
            ]
        ]


renderAvailableGames : Model -> Html Msg
renderAvailableGames { allAvailableGames } =
    let
        renderGameWithDelete game =
            div [] [ span [ class "remove-game", onClick (RemoveGame game) ] [ i [ class "glyphicon glyphicon-remove" ] [] ], (renderGame (\_ -> NoOp) (always False) game) ]
    in
        if List.length allAvailableGames == 0 then
            div [ class "available-games" ] [ text "Please add at least one game..." ]
        else
            div [ class "available-games" ]
                [ div [] [ button [ class "btn btn-default", onClick RemoveAllSelectedGames ] [ text "Remove all" ] ]
                , div [] <| List.map renderGameWithDelete (List.sortBy .name allAvailableGames)
                ]


renderOptionsArea : Html Msg
renderOptionsArea =
    form [ onSubmit ChooseAGame ]
        [ div [ class "form-group" ]
            [ label [] [ text "How many players?" ]
            , input [ type_ "number", class "form-control", onInput (UpdateNumberOfPlayers << Result.toMaybe << String.toInt) ] []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "How long (in minutes) do you have to play?" ]
            , input [ type_ "number", class "form-control", onInput (UpdatePlayTime << Result.toMaybe << String.toInt) ] []
            ]
        , button [ type_ "submit", class "btn btn-primary" ] [ text "Find me a game!" ]
        ]


renderFooter : Html Msg
renderFooter =
    footer [ class "footer" ]
        [ div [ class "container text-center" ]
            [ p [ class "text-muted" ]
                [ text "All APIs provided by"
                , a [ href "https://bgg-json.azurewebsites.net/" ] [ text "BoardGameGeek JSON APIs" ]
                ]
            ]
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


except : List a -> List a -> List a
except xs ys =
    let
        filterFn x =
            List.all ((/=) x) ys
    in
        List.filter filterFn xs
