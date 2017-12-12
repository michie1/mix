module Main exposing (main)

import Html exposing (Html, program, text, div, ul, li, h2, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)
import List.Extra


type alias Model =
    { library : List Track
    , playlist : List Track
    , libraryFilter : String
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
        track =
            Track 1 1 "Michiel" "Hoi" "Original Mix"
    in
        ( { library = [ track ]
          , playlist = []
          , libraryFilter = ""
          }
        , Cmd.none
        )


type Msg
    = NoOp
    | SelectTrack Int Int
    | DeselectTrack Int
    | LibraryFilter String

iContains : String -> String -> Bool
iContains a b =
    String.contains (String.toLower a) (String.toLower b)

filter : String -> List Track -> List Track
filter query tracks =
    List.filter
        (\track -> String.trim query == "" ||
                   iContains query (toString track.cd) ||
                   iContains query (toString track.number) ||
                   iContains query track.artist ||
                   iContains query track.title ||
                   iContains query track.mix
        )
        tracks


view : Model -> Html Msg
view state =
    div []
        [ h2 [] [ text "Library" ]
        , libraryFilter state.libraryFilter
        , ul [] <|
            List.map libraryLi (filter state.libraryFilter state.library)
        , h2 [] [ text "Playlist" ]
        , ul [] <|
            List.map playlistLi (List.Extra.zip (List.range 0 (List.length state.playlist)) state.playlist)
        ]


libraryFilter : String -> Html Msg
libraryFilter query =
    input [ value query, onInput LibraryFilter ] []


libraryLi : Track -> Html Msg
libraryLi track =
    li [ onClick <| SelectTrack track.cd track.number ] [ text <| track.artist ++ " - " ++ track.title ++ " (" ++ track.mix ++ ")" ]


playlistLi : ( Int, Track ) -> Html Msg
playlistLi index_track =
    let
        index =
            Tuple.first index_track

        track =
            Tuple.second index_track
    in
        li [ onClick <| DeselectTrack index, style [ ( "list-style-type", "decimal" ) ] ] [ text <| track.artist ++ " - " ++ track.title ++ " (" ++ track.mix ++ ")" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case msg of
        SelectTrack cd number ->
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

        DeselectTrack index ->
            let
                playlist =
                    List.Extra.removeAt index state.playlist

                -- List.filter (\t -> t.cd /= cd || t.number /= t.number) state.library
            in
                ( { state | playlist = playlist }, Cmd.none )

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
