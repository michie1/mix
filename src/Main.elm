module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, h1, h2, input, li, p, span, text, ul)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, class, classList, placeholder, attribute, type_)
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
    , activePanel : Panel
    }


type Panel
    = LibraryPanel
    | PlaylistPanel
    | MatchesPanel


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { library = []
      , playlist = []
      , libraryFilter = ""
      , selected = Nothing
      , activePanel = LibraryPanel
      }
    , loadTracks
    )


type Msg
    = NoOp
    | AddTrack Int Int
    | RemoveTrack Int
    | ToggleSelection Track
    | ShowPanel Panel
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
                            [ String.fromInt track.cd
                            , String.fromInt track.number
                            , track.artist
                            , track.title
                            , track.mix
                            , String.fromInt track.keyNumber
                            , keyTypeToString track.keyType
                            , String.fromFloat track.bpm
                            ]
                   )
        )
        tracks


view : Model -> Html Msg
view state =
    let
        visibleLibrary =
            filter state.libraryFilter state.library

        matchingTracks =
            matches state.library state.playlist state.selected
    in
    div [ class "app-shell" ]
        [ div [ class "app-header" ]
            [ div []
                [ h1 [] [ text "Mix" ]
                , p [] [ text "Build a smooth, compatible playlist." ]
                ]
            ]
        , div [ class "panel-tabs", attribute "aria-label" "Choose a panel" ]
            [ panelTab state.activePanel LibraryPanel "Library"
            , panelTab state.activePanel PlaylistPanel "Playlist"
            , panelTab state.activePanel MatchesPanel "Matches"
            ]
        , div [ class "workspace" ]
            [ div [ panelClass state.activePanel LibraryPanel ]
                [ div [ class "panel-header" ]
                    [ div []
                        [ h2 [] [ text "Library" ]
                        , span [ class "count" ] [ text <| String.fromInt (List.length visibleLibrary) ]
                        ]
                    , libraryFilter state.libraryFilter
                    ]
                , ul [ class "track-list" ] <|
                    List.map (libraryLi state.selected) visibleLibrary
                ]
            , div [ panelClass state.activePanel PlaylistPanel ]
                [ div [ class "panel-header" ]
                    [ div []
                        [ h2 [] [ text "Playlist" ]
                        , span [ class "count" ] [ text <| String.fromInt (List.length state.playlist) ]
                        ]
                    ]
                , if List.isEmpty state.playlist then
                    emptyState "Your playlist is empty" "Add tracks from the library or matches."
                  else
                    ul [ class "track-list playlist-list" ] <|
                        List.map (playlistLi state.selected) (playlist state.playlist)
                ]
            , div [ panelClass state.activePanel MatchesPanel ]
                [ div [ class "panel-header" ]
                    [ div []
                        [ h2 [] [ text "Matches" ]
                        , span [ class "count" ] [ text <| String.fromInt (List.length matchingTracks) ]
                        ]
                    , selectedTrackLabel state.selected
                    ]
                , case state.selected of
                    Nothing ->
                        emptyState "Select a track" "We will show compatible tracks here."

                    Just selected ->
                        if List.isEmpty matchingTracks then
                            emptyState "No matches found" "Try another track from your library or playlist."
                        else
                            ul [ class "track-list" ] <|
                                List.map (matchLi state.selected selected) matchingTracks
                ]
            ]
        ]


panelTab : Panel -> Panel -> String -> Html Msg
panelTab activePanel panel label =
    button
        [ type_ "button"
        , classList [ ( "panel-tab", True ), ( "is-active", activePanel == panel ) ]
        , onClick <| ShowPanel panel
        ]
        [ text label ]


panelClass : Panel -> Panel -> Attribute Msg
panelClass activePanel panel =
    classList [ ( "panel", True ), ( "is-active", activePanel == panel ) ]


emptyState : String -> String -> Html Msg
emptyState heading description =
    div [ class "empty-state" ]
        [ p [ class "empty-heading" ] [ text heading ]
        , p [] [ text description ]
        ]


selectedTrackLabel : Maybe Track -> Html Msg
selectedTrackLabel maybeTrack =
    case maybeTrack of
        Just track ->
            p [ class "panel-context" ]
                [ text <| "For " ++ track.artist ++ " — " ++ track.title ]

        Nothing ->
            p [ class "panel-context" ] [ text "Choose a track to start" ]


playlist : List Track -> List PlaylistTrack
playlist tracks =
    List.indexedMap
        (\index track ->
            { index = index
            , track = track
            , endPitch = 0.0
            , beginPitch = 0.0
            }
        )
        tracks
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
    input
        [ class "search-input"
        , value query
        , placeholder "Search tracks"
        , attribute "aria-label" "Search the library"
        , onInput LibraryFilter
        ]
        []


libraryLi : Maybe Track -> Track -> Html Msg
libraryLi maybeSelected track =
    trackRow maybeSelected track [] "Add" (AddTrack track.cd track.number)


matchLi : Maybe Track -> Track -> MatchInfo -> Html Msg
matchLi maybeSelected selected matchResult =
    let
        track =
            matchResult.track

        matchDetails =
            [ span [ class "match-detail" ]
                [ text <|
                    if matchResult.bpmDelta == 0 then
                        "Exact BPM"
                    else
                        "Δ " ++ String.fromFloat matchResult.bpmDelta ++ " BPM"
                ]
            , span [ class "match-detail" ]
                [ text <|
                    if matchResult.keyDelta == 0 && track.keyType == selected.keyType then
                        "Exact key"
                    else
                        "Δ " ++ String.fromInt matchResult.keyDelta ++ " key"
                ]
            ]
    in
    trackRow maybeSelected track matchDetails "Add" (AddTrack track.cd track.number)


trackRow : Maybe Track -> Track -> List (Html Msg) -> String -> Msg -> Html Msg
trackRow maybeSelected track details actionLabel action =
    li [ classList [ ( "track-row", True ), ( "is-selected", isSelected track maybeSelected ) ] ]
        [ button
            [ type_ "button"
            , class "track-main"
            , onClick <| ToggleSelection track
            , attribute "aria-label" <| "Select " ++ track.artist ++ " — " ++ track.title
            ]
            [ span [ class "track-title" ] [ text <| track.artist ++ " — " ++ track.title ]
            , span [ class "track-meta" ]
                [ text <|
                    String.fromInt track.cd
                        ++ "#"
                        ++ String.fromInt track.number
                        ++ "  ·  "
                        ++ track.mix
                        ++ "  ·  Key "
                        ++ String.fromInt track.keyNumber
                        ++ keyTypeToString track.keyType
                        ++ "  ·  "
                        ++ String.fromFloat track.bpm
                        ++ " BPM"
                ]
            , span [ class "match-details" ] details
            ]
        , button
            [ type_ "button"
            , class <|
                if actionLabel == "Remove" then
                    "track-action remove-action"
                else
                    "track-action"
            , onClick action
            , attribute "aria-label" <| actionLabel ++ " " ++ track.artist ++ " — " ++ track.title
            ]
            [ text actionLabel ]
        ]


type alias MatchInfo =
    { track : Track
    , bpmDelta : Float
    , keyDelta : Int
    , rank : Int
    }


matches : List Track -> List Track -> Maybe Track -> List MatchInfo
matches library playlistTracks maybeSelected =
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
                        List.Extra.notMember track playlistTracks
                            && (track.cd /= selected.cd || track.number /= selected.number)
                            && similar
                )
                library
                |> List.map (matchInfo selected)
                |> List.sortWith compareMatches

        Nothing ->
            []


matchInfo : Track -> Track -> MatchInfo
matchInfo selected track =
    let
        bpmDelta =
            diffBpm track.bpm selected.bpm

        keyDelta =
            diffKeyNumber track.keyNumber selected.keyNumber

        sameKey =
            track.keyNumber == selected.keyNumber && track.keyType == selected.keyType

        rank =
            if sameKey && bpmDelta == 0 then
                0
            else if sameKey then
                1
            else if track.bpm == selected.bpm && track.keyNumber == selected.keyNumber then
                2
            else
                3
    in
    { track = track
    , bpmDelta = bpmDelta
    , keyDelta = keyDelta
    , rank = rank
    }


compareMatches : MatchInfo -> MatchInfo -> Order
compareMatches a b =
    case compare a.rank b.rank of
        EQ ->
            case compare a.bpmDelta b.bpmDelta of
                EQ ->
                    case compare a.keyDelta b.keyDelta of
                        EQ ->
                            case compare (String.toLower a.track.artist) (String.toLower b.track.artist) of
                                EQ ->
                                    compare (String.toLower a.track.title) (String.toLower b.track.title)

                                order ->
                                    order

                        order ->
                            order

                order ->
                    order

        order ->
            order


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


isSelected : Track -> Maybe Track -> Bool
isSelected track maybeSelected =
    case maybeSelected of
        Just selected ->
            selected.cd == track.cd && selected.number == track.number

        Nothing ->
            False


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

        pitchDetails =
            [ span [ class "match-detail" ]
                [ text <| formatPitch beginPitch ++ "% → " ++ formatPitch endPitch ++ "%" ]
            ]
    in
        trackRow maybeSelected track pitchDetails "Remove" (RemoveTrack index)


formatPitch : Float -> String
formatPitch pitch =
    if abs pitch < 0.000001 then
        "0.00"

    else
        String.fromFloat
            (toFloat (round (pitch * 5000)) * 2 / 100)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case msg of
        AddTrack cd number ->
            let
                updatedPlaylist =
                    case
                        state.library
                            |> List.Extra.find (\t -> t.cd == cd && t.number == number)
                    of
                        Just track ->
                            List.append state.playlist [ track ]

                        Nothing ->
                            state.playlist
            in
                ( { state | playlist = updatedPlaylist }, Cmd.none )

        RemoveTrack index ->
            let
                updatedPlaylist =
                    List.Extra.removeAt index state.playlist
            in
                ( { state | playlist = updatedPlaylist }, Cmd.none )

        ToggleSelection track ->
            let
                justSelected =
                    Just track

                nextSelected =
                    case state.selected of
                        Just currentSelected ->
                            if currentSelected.cd == track.cd && currentSelected.number == track.number then
                                Nothing
                            else
                                justSelected

                        Nothing ->
                            justSelected

            in
                ( { state | selected = nextSelected, activePanel = MatchesPanel }, Cmd.none )

        ShowPanel panel ->
            ( { state | activePanel = panel }, Cmd.none )

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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


loadTracks : Cmd Msg
loadTracks =
    Http.get
        { url = "static/tracks.json"
        , expect = Http.expectJson LoadedTracks tracksDecoder
        }


keyType : String -> KeyType
keyType value =
    case value of
        "A" ->
            A

        _ ->
            B


keyTypeToString : KeyType -> String
keyTypeToString value =
    case value of
        A ->
            "A"

        B ->
            "B"


keyTypeDecoder : String -> Json.Decode.Decoder KeyType
keyTypeDecoder value =
    Json.Decode.succeed (keyType value)


trackDecoder : Json.Decode.Decoder Track
trackDecoder =
    Json.Decode.map8
        (\cd number artist title mix bpm keyNumber decodedKeyType ->
            { cd = cd
            , number = number
            , artist = artist
            , title = title
            , mix = mix
            , bpm = bpm
            , keyNumber = keyNumber
            , keyType = decodedKeyType
            }
        )
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
