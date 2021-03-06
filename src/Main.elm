module Main exposing (main)

import Html exposing (Html, Attribute, program, text, div, span, ul, li, h2, input, button)
import Html.Events exposing (onWithOptions, onClick, onInput)
import Html.Attributes exposing (style, value)
import List.Extra
import Json.Decode
import Http


type alias Model =
    { library : List Track
    , playlist : List Track
    , libraryFilter : String
    , selected :
        Maybe Track
        -- TODO: use index?
    }


type KeyType
    = A
    | B


type alias Track =
    { cd : Int
    , number : Int
    , artist : String
    , title : String
    , mix : String
    , bpm : Float
    , keyNumber : Int
    , keyType : KeyType
    }


type alias PlaylistTrack =
    { index : Int
    , track : Track
    , endPitch : Float
    , beginPitch : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { library = []
      , playlist = []
      , libraryFilter = ""
      , selected = Nothing
      }
    , loadTracks
    )


type Msg
    = NoOp
    | AddTrack Int Int
    | RemoveTrack Int
    | ToggleSelection Track
    | LibraryFilter String
    | LoadTracks
    | LoadedTracks (Result Http.Error (List Track))


iContains : String -> String -> Bool
iContains a b =
    String.contains (String.toLower a) (String.toLower b)


filter : String -> List Track -> List Track
filter query tracks =
    List.filter
        (\track ->
            String.trim query
                == ""
                || (iContains query <|
                        String.join ""
                            [ (toString track.cd)
                            , (toString track.number)
                            , track.artist
                            , track.title
                            , track.mix
                            , (toString track.keyNumber)
                            , (toString track.keyType)
                            , (toString track.bpm)
                            ]
                   )
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
            List.map (playlistLi state.selected) (playlist state.playlist)
        , h2 [] [ text "Matches" ]
        , ul [] <|
            List.map (matchLi state.selected) (matches state.library state.playlist state.selected)
        ]


playlist : List Track -> List PlaylistTrack
playlist tracks =
    List.indexedMap (,) tracks
        |> List.map
            (\indexTrack ->
                PlaylistTrack (Tuple.first indexTrack) (Tuple.second indexTrack) 0.0 0.0
            )
        |> setEndPitch
        |> setBeginPitch


setEndPitch : List PlaylistTrack -> List PlaylistTrack
setEndPitch playlistTracks =
    List.indexedMap
        (\index playlistTrack ->
            case List.Extra.getAt (index + 1) playlistTracks of
                Just next ->
                    let
                        track =
                            playlistTrack.track

                        endPitch =
                            (targetAdjustment track.bpm (targetBpm track.bpm next.track.bpm))
                    in
                        { playlistTrack | endPitch = endPitch }

                Nothing ->
                    playlistTrack
        )
        playlistTracks


setBeginPitch : List PlaylistTrack -> List PlaylistTrack
setBeginPitch playlistTracks =
    List.indexedMap
        (\index playlistTrack ->
            case List.Extra.getAt (index - 1) playlistTracks of
                Just next ->
                    let
                        track =
                            playlistTrack.track

                        beginPitch =
                            (targetAdjustment track.bpm (targetBpm track.bpm next.track.bpm))
                    in
                        { playlistTrack | beginPitch = beginPitch }

                Nothing ->
                    playlistTrack
        )
        playlistTracks


targetBpm : Float -> Float -> Float
targetBpm a b =
    let
        avg =
            (a - b) / 2.0 + b
    in
        if (avg |> round |> toFloat) == avg then
            avg - 0.5
        else
            avg


targetAdjustment : Float -> Float -> Float
targetAdjustment begin end =
    (end / begin) - 1


libraryFilter : String -> Html Msg
libraryFilter query =
    input [ value query, onInput LibraryFilter ] []


libraryLi : Maybe Track -> Track -> Html Msg
libraryLi maybeSelected track =
    li
        [ --[ onClick <| ToggleSelection track
          --, onDoubleClickWithPreventDefault <| AddTrack track.cd track.number
          style [ selectedStyle track maybeSelected ]
        ]
        [ span [ onClick <| ToggleSelection track ]
            [ text <|
                toString track.cd
                    ++ "#"
                    ++ toString track.number
                    ++ " "
                    ++ track.artist
                    ++ " - "
                    ++ track.title
                    ++ " ("
                    ++ track.mix
                    ++ ")"
                    ++ " "
                    ++ toString track.keyNumber
                    ++ toString track.keyType
                    ++ toString track.bpm
            ]
        , span [ onClick <| AddTrack track.cd track.number ] [ text "+" ]
        ]


matchLi : Maybe Track -> Track -> Html Msg
matchLi maybeSelected track =
    li
        [ style [ selectedStyle track maybeSelected ] ]
        [ span [ onClick <| ToggleSelection track ]
            [ text <|
                toString track.cd
                    ++ "#"
                    ++ toString track.number
                    ++ " "
                    ++ track.artist
                    ++ " - "
                    ++ track.title
                    ++ " ("
                    ++ track.mix
                    ++ ")"
                    ++ " "
                    ++ toString track.keyNumber
                    ++ toString track.keyType
                    ++ toString track.bpm
            ]
        , span [ onClick <| AddTrack track.cd track.number ] [ text "+" ]
        ]


matches : List Track -> List Track -> Maybe Track -> List Track
matches library playlist maybeSelected =
    case maybeSelected of
        Just selected ->
            List.filter
                (\track ->
                    let
                        similar =
                            ((track.keyNumber == selected.keyNumber && track.keyType == selected.keyType && (diffBpm track.bpm selected.bpm) <= 2.0)
                                || (track.bpm == selected.bpm && track.keyNumber == selected.keyNumber)
                                || (track.keyType == selected.keyType && track.bpm == selected.bpm && (diffKeyNumber track.keyNumber selected.keyNumber) <= 2)
                            )
                    in
                        List.Extra.notMember track playlist
                            && case maybeSelected of
                                Just selected ->
                                    (track.cd /= selected.cd || track.number /= selected.number)
                                        && similar

                                Nothing ->
                                    similar
                )
                library

        Nothing ->
            []


diffKeyNumber : Int -> Int -> Int
diffKeyNumber a b =
    let
        d =
            case ( a, b ) of
                ( 1, 12 ) ->
                    1

                ( 12, 1 ) ->
                    1

                ( 1, 11 ) ->
                    2

                ( 11, 1 ) ->
                    2

                ( 2, 12 ) ->
                    2

                ( 12, 2 ) ->
                    2

                _ ->
                    a - b
    in
        if d > 0 then
            d
        else
            d * -1


diffBpm : Float -> Float -> Float
diffBpm a b =
    let
        d =
            a - b
    in
        if d > 0 then
            d
        else
            d * -1


selectedStyle : Track -> Maybe Track -> ( String, String )
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
                ( "border", "1px solid white" )


playlistLi : Maybe Track -> PlaylistTrack -> Html Msg
playlistLi maybeSelected playlistTrack =
    let
        index =
            playlistTrack.index

        track =
            playlistTrack.track

        beginPitch =
            playlistTrack.beginPitch

        endPitch =
            playlistTrack.endPitch
    in
        li
            [ style [ ( "list-style-type", "decimal" ), selectedStyle track maybeSelected ] ]
            [ span [ onClick <| ToggleSelection track ]
                [ text <|
                    toString track.cd
                        ++ "#"
                        ++ toString track.number
                        ++ " "
                        ++ track.artist
                        ++ " - "
                        ++ track.title
                        ++ " ("
                        ++ track.mix
                        ++ ")"
                        ++ " "
                        ++ toString track.keyNumber
                        ++ toString track.keyType
                        ++ toString track.bpm
                        ++ " "
                        ++ formatPitch beginPitch
                        ++ "%"
                        ++ " -> "
                        ++ formatPitch endPitch
                        ++ "%"
                ]
            , span [ onClick <| RemoveTrack index ] [ text "-" ]
            ]


formatPitch : Float -> String
formatPitch pitch =
    case pitch of
        0.0 ->
            "0.00"

        _ ->
            pitch
                |> (*) 100
                -- To percentage
                |>
                    (*) 100
                -- Begin round to even decimals
                |>
                    flip (/) 2.0
                |> round
                |> (*) 2
                -- End round to even decimals
                -- Begin rounding 2 decimals
                |>
                    toFloat
                |> flip (/) 100.0
                -- End rounding
                |>
                    toString


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case msg of
        AddTrack cd number ->
            let
                playlist =
                    case
                        state.library
                            |> List.Extra.find (\t -> t.cd == cd && t.number == number)
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

        ToggleSelection track ->
            let
                justSelected =
                    Just track

                selected =
                    case state.selected of
                        Just selected ->
                            if selected.cd == track.cd && selected.number == track.number then
                                Nothing
                            else
                                justSelected

                        Nothing ->
                            justSelected
            in
                ( { state | selected = selected }, Cmd.none )

        LibraryFilter query ->
            ( { state | libraryFilter = query, selected = Nothing }, Cmd.none )

        LoadTracks ->
            ( state, loadTracks )

        LoadedTracks resultTracks ->
            case resultTracks of
                Ok tracks ->
                    ( { state | library = tracks }, Cmd.none )

                Err e ->
                    let
                        _ =
                            Debug.log "err e" e
                    in
                        ( state, Cmd.none )

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


loadTracks : Cmd Msg
loadTracks =
    Http.send LoadedTracks (Http.get "static/tracks.json" tracksDecoder)


keyType : String -> KeyType
keyType value =
    case value of
        "A" ->
            A

        _ ->
            B


keyTypeDecoder : String -> Json.Decode.Decoder KeyType
keyTypeDecoder value =
    Json.Decode.succeed (keyType value)


trackDecoder : Json.Decode.Decoder Track
trackDecoder =
    Json.Decode.map8
        Track
        (Json.Decode.field "cd" Json.Decode.int)
        (Json.Decode.field "number" Json.Decode.int)
        (Json.Decode.field "artist" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "remix" Json.Decode.string)
        (Json.Decode.field "bpm" Json.Decode.float)
        (Json.Decode.field "keyNumber" Json.Decode.int)
        (Json.Decode.field "keyType"
            (Json.Decode.andThen keyTypeDecoder Json.Decode.string)
        )


tracksDecoder : Json.Decode.Decoder (List Track)
tracksDecoder =
    Json.Decode.at [ "tracks" ] (Json.Decode.list trackDecoder)
