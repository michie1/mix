module Main exposing (main)

import Html exposing (Html, program, text, div, ul, li, h2)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)


type alias Model =
    { library : List Track
    , playlist : List Track
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
          }
        , Cmd.none
        )


type Msg
    = NoOp
    | SelectTrack Int Int
    | DeselectTrack Int Int


view : Model -> Html Msg
view state =
    div []
        [ h2 [] [ text "Library" ]
        , ul [] <|
            List.map libraryLi state.library
        , h2 [] [ text "Library" ]
        , ul [] <|
            List.map playlistLi state.playlist
        ]


libraryLi : Track -> Html Msg
libraryLi track =
    li [ onClick <| SelectTrack track.cd track.number ] [ text <| track.artist ++ " - " ++ track.title ++ " (" ++ track.mix ++ ")" ]


playlistLi : Track -> Html Msg
playlistLi track =
    li [ onClick <| DeselectTrack track.cd track.number, style [ ( "list-style-type", "decimal" ) ] ] [ text <| track.artist ++ " - " ++ track.title ++ " (" ++ track.mix ++ ")" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case msg of
        SelectTrack cd number ->
            let
                playlist =
                    case
                        state.library
                            |> List.filter (\t -> t.cd == cd && t.number == t.number)
                            |> List.head
                    of
                        Just track ->
                            List.append state.playlist [ track ]

                        Nothing ->
                            state.playlist
            in
                ( { state | playlist = playlist }, Cmd.none )

        DeselectTrack cd number ->
            let
                playlist =
                    List.filter (\t -> t.cd /= cd || t.number /= t.number) state.library
            in
                ( { state | playlist = playlist }, Cmd.none )

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
