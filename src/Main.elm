module Main exposing (main)

import Html exposing (Html, program, text, div, ul, li, h2, input)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Html.Attributes exposing (style, value)
import List.Extra


type alias Model =
    { library : List Track
    , playlist : List Track
    , libraryFilter : String
    , selected : Maybe Selected
    }


type alias Selected =
    { cd : Int
    , number : Int
    }


type alias Track =
    { cd : Int
    , number : Int
    , artist : String
    , title : String
    , mix : String
    }


init : ( Model, Cmd Msg )
init =
    let
        library =
            [ Track 1 1 "Michiel" "Hoi" "Original Mix"
            , Track 1 2 "Michiel" "Doei" "Original Mix"
            , Track 1 3 "Michiel" "Bla" "Original Mix"
            ]
    in
        ( { library = library
          , playlist = []
          , libraryFilter = ""
          , selected = Nothing
          }
        , Cmd.none
        )


type Msg
    = NoOp
    | AddTrack Int Int
    | RemoveTrack Int
    | ToggleSelection Int Int
    | LibraryFilter String


iContains : String -> String -> Bool
iContains a b =
    String.contains (String.toLower a) (String.toLower b)


filter : String -> List Track -> List Track
filter query tracks =
    List.filter
        (\track ->
            String.trim query
                == ""
                || iContains query (toString track.cd)
                || iContains query (toString track.number)
                || iContains query track.artist
                || iContains query track.title
                || iContains query track.mix
        )
        tracks


view : Model -> Html Msg
view state =
    div []
        [ h2 [] [ text "Library" ]
        , libraryFilter state.libraryFilter
        , ul [] <|
            List.map (libraryLi state.selected) (filter state.libraryFilter state.library)
        , h2 [] [ text "Playlist" ]
        , ul [] <|
            List.map (playlistLi state.selected) (List.Extra.zip (List.range 0 (List.length state.playlist)) state.playlist)
        , h2 [] [ text "Matches" ]
        , ul [] <|
            List.map (matchLi state.selected) (matches state.library state.playlist state.selected)
        ]


libraryFilter : String -> Html Msg
libraryFilter query =
    input [ value query, onInput LibraryFilter ] []


libraryLi : Maybe Selected -> Track -> Html Msg
libraryLi maybeSelected track =
    li
        [ onClick <| ToggleSelection track.cd track.number
        , onDoubleClick <| AddTrack track.cd track.number
        , style [ selectedStyle track maybeSelected ]
        ]
        [ text <| track.artist ++ " - " ++ track.title ++ " (" ++ track.mix ++ ")" ]


matchLi : Maybe Selected -> Track -> Html Msg
matchLi maybeSelected track =
    li
        [ onClick <| ToggleSelection track.cd track.number
        , onDoubleClick <| AddTrack track.cd track.number
        , style [ selectedStyle track maybeSelected ]
        ]
        [ text <| track.artist ++ " - " ++ track.title ++ " (" ++ track.mix ++ ")" ]


matches : List Track -> List Track -> Maybe Selected -> List Track
matches library playlist maybeSelected =
    case maybeSelected of
        Just selected ->
            List.filter
                (\track ->
                    List.Extra.notMember track playlist
                        && case maybeSelected of
                            Just selected ->
                                track.cd /= selected.cd || track.number /= selected.number

                            Nothing ->
                                True
                        && True
                )
                library
        Nothing ->
            []


selectedStyle : Track -> Maybe Selected -> ( String, String )
selectedStyle track maybeSelected =
    let
        isSelected =
            case maybeSelected of
                Just selected ->
                    selected.cd
                        == track.cd
                        && selected.number
                        == track.number

                Nothing ->
                    False
    in
        case isSelected of
            True ->
                ( "border", "1px solid red" )

            False ->
                ( "border", "1px solid black" )


playlistLi : Maybe Selected -> ( Int, Track ) -> Html Msg
playlistLi maybeSelected index_track =
    let
        index =
            Tuple.first index_track

        track =
            Tuple.second index_track
    in
        li
            [ onClick <| ToggleSelection track.cd track.number
            , onDoubleClick <| RemoveTrack index
            , style [ ( "list-style-type", "decimal" ), selectedStyle track maybeSelected ]
            ]
            [ text <| track.artist ++ " - " ++ track.title ++ " (" ++ track.mix ++ ")" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case msg of
        AddTrack cd number ->
            let
                playlist =
                    case
                        state.library
                            |> List.Extra.find (\t -> t.cd == cd && t.number == t.number)
                    of
                        Just track ->
                            List.append state.playlist [ track ]

                        Nothing ->
                            state.playlist
            in
                ( { state | playlist = playlist }, Cmd.none )

        RemoveTrack index ->
            let
                playlist =
                    List.Extra.removeAt index state.playlist
            in
                ( { state | playlist = playlist }, Cmd.none )

        ToggleSelection cd number ->
            let
                justSelected =
                    Just
                        { cd = cd
                        , number = number
                        }

                selected =
                    case state.selected of
                        Just selected ->
                            if selected.cd == cd && selected.number == number then
                                Nothing
                            else
                                justSelected

                        Nothing ->
                            justSelected
            in
                ( { state | selected = selected }, Cmd.none )

        LibraryFilter query ->
            ( { state | libraryFilter = query }, Cmd.none )

        NoOp ->
            ( state, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions state =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
