port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Attribute, Html, button, div, h1, h2, input, li, node, option, p, select, span, text, ul)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, type_, value)
import List.Extra
import Json.Decode
import Json.Encode
import Http
import Task


type alias Model =
    { library : List Track
    , playlist : List Track
    , libraryFilter : String
    , selected :
        Maybe Track
        -- TODO: use index?
    , activePanel : Panel
    , cursor : Int
    , shortcutHelpOpen : Bool
    , playlistName : String
    , savedPlaylists : List SavedPlaylist
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


type alias SavedPlaylist =
    { name : String
    , tracks : List Track
    }


type alias StoredState =
    { current : List Track
    , saved : List SavedPlaylist
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init savedPlaylist =
    let
        storedState =
            Json.Decode.decodeValue storedStateDecoder savedPlaylist
                |> Result.withDefault { current = [], saved = [] }
    in
    ( { library = []
      , playlist = storedState.current
      , libraryFilter = ""
      , selected = Nothing
      , activePanel = LibraryPanel
      , cursor = 0
      , shortcutHelpOpen = False
      , playlistName = ""
      , savedPlaylists = storedState.saved
      }
    , loadTracks
    )


type Msg
    = NoOp
    | AddTrack Int Int
    | RemoveTrack Int
    | ToggleSelection Track
    | ShowPanel Panel
    | KeyPressed KeyEvent
    | ToggleShortcutHelp
    | FocusResult (Result Browser.Dom.Error ())
    | LibraryFilter String
    | ExportPlaylist
    | PlaylistName String
    | SavePlaylist
    | LoadPlaylist String
    | LoadTracks
    | LoadedTracks (Result Http.Error (List Track))


type alias KeyEvent =
    { key : String
    , targetTag : String
    , ctrl : Bool
    , alt : Bool
    , meta : Bool
    }


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
            , button
                [ type_ "button"
                , id "shortcut-trigger"
                , class "shortcuts-button"
                , attribute "aria-keyshortcuts" "?"
                , attribute "aria-expanded" <|
                    if state.shortcutHelpOpen then
                        "true"
                    else
                        "false"
                , onClick ToggleShortcutHelp
                ]
                [ text "Shortcuts  ?" ]
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
                    List.indexedMap (libraryLi state.activePanel state.cursor state.selected) visibleLibrary
                ]
            , div [ panelClass state.activePanel PlaylistPanel ]
                [ div [ class "panel-header" ]
                    [ div [ class "playlist-header" ]
                        [ div []
                        [ h2 [] [ text "Playlist" ]
                        , span [ class "count" ] [ text <| String.fromInt (List.length state.playlist) ]
                        ]
                        , button
                            [ type_ "button"
                            , class "export-button"
                            , disabled (List.isEmpty state.playlist)
                            , onClick ExportPlaylist
                            ]
                            [ text "Export Markdown" ]
                        ]
                    , div [ class "playlist-manager" ]
                        [ input
                            [ class "playlist-name-input"
                            , value state.playlistName
                            , placeholder "Playlist name"
                            , attribute "aria-label" "Playlist name"
                            , onInput PlaylistName
                            ]
                            []
                        , button
                            [ type_ "button"
                            , class "save-button"
                            , disabled (List.isEmpty state.playlist || String.isEmpty (String.trim state.playlistName))
                            , onClick SavePlaylist
                            ]
                            [ text "Save" ]
                        , select
                            [ class "playlist-select"
                            , value ""
                            , attribute "aria-label" "Load a saved playlist"
                            , disabled (List.isEmpty state.savedPlaylists)
                            , onInput LoadPlaylist
                            ]
                            (option [ value "" ] [ text "Load playlist…" ]
                                :: List.map
                                    (\saved -> option [ value saved.name ] [ text saved.name ])
                                    state.savedPlaylists
                            )
                        ]
                    ]
                , if List.isEmpty state.playlist then
                    emptyState "Your playlist is empty" "Add tracks from the library or matches."
                  else
                    ul [ class "track-list playlist-list" ] <|
                        List.indexedMap (playlistLi state.activePanel state.cursor state.selected) (playlist state.playlist)
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
                                List.indexedMap (matchLi state.activePanel state.cursor state.selected selected) matchingTracks
                ]
            ]
        , if state.shortcutHelpOpen then
            shortcutHelp
          else
            text ""
        ]


shortcutHelp : Html Msg
shortcutHelp =
    div [ class "shortcut-backdrop", attribute "role" "presentation" ]
        [ div
            [ class "shortcut-dialog"
            , attribute "role" "dialog"
            , attribute "aria-modal" "true"
            , attribute "aria-labelledby" "shortcut-title"
            ]
            [ div [ class "shortcut-dialog-header" ]
                [ h2 [ id "shortcut-title" ] [ text "Keyboard shortcuts" ]
                , button
                    [ type_ "button"
                    , id "shortcut-close"
                    , class "shortcut-close"
                    , attribute "aria-label" "Close keyboard shortcuts"
                    , onClick ToggleShortcutHelp
                    ]
                    [ text "Close" ]
                ]
            , ul [ class "shortcut-list" ]
                [ shortcutItem "1  2  3" "Open a panel"
                , shortcutItem "/" "Search the library"
                , shortcutItem "↑  ↓  J  K" "Move between tracks"
                , shortcutItem "Enter" "Select a track"
                , shortcutItem "A" "Add the current track"
                , shortcutItem "Delete" "Remove a playlist track"
                , shortcutItem "?" "Open or close this guide"
                , shortcutItem "Escape" "Close or clear"
                ]
            ]
        ]


shortcutItem : String -> String -> Html Msg
shortcutItem keys description =
    li []
        [ node "kbd" [] [ text keys ]
        , span [] [ text description ]
        ]


panelTab : Panel -> Panel -> String -> Html Msg
panelTab activePanel panel label =
    button
        [ type_ "button"
        , classList [ ( "panel-tab", True ), ( "is-active", activePanel == panel ) ]
        , attribute "aria-keyshortcuts" <| panelShortcut panel
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
        [ id "library-search"
        , class "search-input"
        , value query
        , placeholder "Search tracks"
        , attribute "aria-label" "Search the library"
        , onInput LibraryFilter
        ]
        []


libraryLi : Panel -> Int -> Maybe Track -> Int -> Track -> Html Msg
libraryLi activePanel cursor maybeSelected index track =
    trackRow LibraryPanel (activePanel == LibraryPanel && cursor == index) index maybeSelected track [] "Add" (AddTrack track.cd track.number)


matchLi : Panel -> Int -> Maybe Track -> Track -> Int -> MatchInfo -> Html Msg
matchLi activePanel cursor maybeSelected selected index matchResult =
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
    trackRow MatchesPanel (activePanel == MatchesPanel && cursor == index) index maybeSelected track matchDetails "Add" (AddTrack track.cd track.number)


trackRow : Panel -> Bool -> Int -> Maybe Track -> Track -> List (Html Msg) -> String -> Msg -> Html Msg
trackRow panel isCursor index maybeSelected track details actionLabel action =
    li
        [ classList
            [ ( "track-row", True )
            , ( "is-selected", isSelected track maybeSelected )
            , ( "is-cursor", isCursor )
            ]
        ]
        [ button
            [ type_ "button"
            , class "track-main"
            , id <| rowId panel index
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


playlistLi : Panel -> Int -> Maybe Track -> Int -> PlaylistTrack -> Html Msg
playlistLi activePanel cursor maybeSelected _ playlistTrack =
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
        trackRow PlaylistPanel (activePanel == PlaylistPanel && cursor == index) index maybeSelected track pitchDetails "Remove" (RemoveTrack index)


formatPitch : Float -> String
formatPitch pitch =
    if abs pitch < 0.000001 then
        "0.00"

    else
        String.fromFloat
            (toFloat (round (pitch * 5000)) * 2 / 100)


playlistMarkdown : List Track -> String
playlistMarkdown tracks =
    let
        trackLine playlistTrack =
            let
                track =
                    playlistTrack.track

                mixLabel =
                    if String.isEmpty (String.trim track.mix) then
                        ""
                    else
                        " (" ++ track.mix ++ ")"
            in
                String.fromInt (playlistTrack.index + 1)
                    ++ ". **"
                    ++ track.artist
                    ++ " — "
                    ++ track.title
                    ++ "**"
                    ++ mixLabel
                    ++ " — "
                    ++ String.fromFloat track.bpm
                    ++ " BPM — Key "
                    ++ String.fromInt track.keyNumber
                    ++ keyTypeToString track.keyType
                    ++ " — CD "
                    ++ String.fromInt track.cd
                    ++ " #"
                    ++ String.fromInt track.number
                    ++ " — Pitch "
                    ++ formatPitch playlistTrack.beginPitch
                    ++ "% → "
                    ++ formatPitch playlistTrack.endPitch
                    ++ "%"
    in
        "# Mix playlist\n\n"
            ++ (playlist tracks |> List.map trackLine |> String.join "\n")
            ++ "\n"


trackEncoder : Track -> Json.Encode.Value
trackEncoder track =
    Json.Encode.object
        [ ( "cd", Json.Encode.int track.cd )
        , ( "number", Json.Encode.int track.number )
        , ( "artist", Json.Encode.string track.artist )
        , ( "title", Json.Encode.string track.title )
        , ( "remix", Json.Encode.string track.mix )
        , ( "bpm", Json.Encode.float track.bpm )
        , ( "keyNumber", Json.Encode.int track.keyNumber )
        , ( "keyType", Json.Encode.string (keyTypeToString track.keyType) )
        ]


persistState : Model -> Cmd Msg
persistState state =
    savePlaylist <|
        Json.Encode.object
            [ ( "current", Json.Encode.list trackEncoder state.playlist )
            , ( "saved", Json.Encode.list savedPlaylistEncoder state.savedPlaylists )
            ]


savedPlaylistEncoder : SavedPlaylist -> Json.Encode.Value
savedPlaylistEncoder saved =
    Json.Encode.object
        [ ( "name", Json.Encode.string saved.name )
        , ( "tracks", Json.Encode.list trackEncoder saved.tracks )
        ]


panelShortcut : Panel -> String
panelShortcut panel =
    case panel of
        LibraryPanel ->
            "1"

        PlaylistPanel ->
            "2"

        MatchesPanel ->
            "3"


panelId : Panel -> String
panelId panel =
    case panel of
        LibraryPanel ->
            "library"

        PlaylistPanel ->
            "playlist"

        MatchesPanel ->
            "matches"


rowId : Panel -> Int -> String
rowId panel index =
    panelId panel ++ "-track-" ++ String.fromInt index


sameTrack : Track -> Track -> Bool
sameTrack a b =
    a.cd == b.cd && a.number == b.number


tracksForPanel : Panel -> Model -> List Track
tracksForPanel panel state =
    case panel of
        LibraryPanel ->
            filter state.libraryFilter state.library

        PlaylistPanel ->
            state.playlist

        MatchesPanel ->
            List.map .track (matches state.library state.playlist state.selected)


cursorForPanel : Panel -> Model -> Int
cursorForPanel panel state =
    case state.selected of
        Just selected ->
            tracksForPanel panel state
                |> List.Extra.findIndex (sameTrack selected)
                |> Maybe.withDefault 0

        Nothing ->
            0


clampCursor : List Track -> Int -> Int
clampCursor tracks cursor =
    if List.isEmpty tracks then
        0
    else
        clamp 0 (List.length tracks - 1) cursor


focusRow : Panel -> Int -> Cmd Msg
focusRow panel cursor =
    Browser.Dom.focus (rowId panel cursor)
        |> Task.attempt FocusResult


focusCurrentRow : Model -> Cmd Msg
focusCurrentRow state =
    if List.isEmpty (tracksForPanel state.activePanel state) then
        Cmd.none
    else
        focusRow state.activePanel state.cursor


switchPanel : Panel -> Model -> ( Model, Cmd Msg )
switchPanel panel state =
    let
        cursor =
            cursorForPanel panel state

        nextState =
            { state | activePanel = panel, cursor = cursor }
    in
        ( nextState, focusCurrentRow nextState )


moveCursor : Int -> Model -> ( Model, Cmd Msg )
moveCursor offset state =
    let
        nextCursor =
            clampCursor (tracksForPanel state.activePanel state) (state.cursor + offset)

        nextState =
            { state | cursor = nextCursor }
    in
        ( nextState, focusCurrentRow nextState )


currentTrack : Model -> Maybe Track
currentTrack state =
    List.Extra.getAt state.cursor (tracksForPanel state.activePanel state)


toggleTrackSelection : Track -> Model -> Model
toggleTrackSelection track state =
    let
        nextSelected =
            case state.selected of
                Just currentSelected ->
                    if sameTrack currentSelected track then
                        Nothing
                    else
                        Just track

                Nothing ->
                    Just track
    in
        { state | selected = nextSelected, activePanel = MatchesPanel, cursor = 0 }


handleKey : KeyEvent -> Model -> ( Model, Cmd Msg )
handleKey event state =
    if event.ctrl || event.alt || event.meta then
        ( state, Cmd.none )

    else if state.shortcutHelpOpen then
        if event.key == "Escape" || event.key == "?" then
            update ToggleShortcutHelp state
        else
            ( state, Cmd.none )

    else if event.targetTag == "INPUT" then
        if event.key == "Escape" then
            if String.isEmpty state.libraryFilter then
                switchPanel LibraryPanel state
            else
                ( { state | libraryFilter = "", selected = Nothing, cursor = 0 }, Cmd.none )
        else
            ( state, Cmd.none )

    else
        case event.key of
            "1" ->
                switchPanel LibraryPanel state

            "2" ->
                switchPanel PlaylistPanel state

            "3" ->
                switchPanel MatchesPanel state

            "/" ->
                let
                    nextState =
                        { state | activePanel = LibraryPanel, cursor = cursorForPanel LibraryPanel state }
                in
                    ( nextState, Browser.Dom.focus "library-search" |> Task.attempt FocusResult )

            "ArrowUp" ->
                moveCursor -1 state

            "ArrowDown" ->
                moveCursor 1 state

            "Enter" ->
                ( state, Cmd.none )

            "Delete" ->
                if state.activePanel == PlaylistPanel then
                    update (RemoveTrack state.cursor) state
                else
                    ( state, Cmd.none )

            "Backspace" ->
                if state.activePanel == PlaylistPanel then
                    update (RemoveTrack state.cursor) state
                else
                    ( state, Cmd.none )

            "?" ->
                update ToggleShortcutHelp state

            "Escape" ->
                if state.shortcutHelpOpen then
                    update ToggleShortcutHelp state
                else if not (String.isEmpty state.libraryFilter) then
                    ( { state | libraryFilter = "", selected = Nothing, cursor = 0 }, Cmd.none )
                else
                    ( { state | selected = Nothing }, Cmd.none )

            key ->
                case String.toLower key of
                    "j" ->
                        moveCursor 1 state

                    "k" ->
                        moveCursor -1 state

                    "a" ->
                        if state.activePanel == PlaylistPanel then
                            ( state, Cmd.none )
                        else
                            case currentTrack state of
                                Just track ->
                                    update (AddTrack track.cd track.number) state

                                Nothing ->
                                    ( state, Cmd.none )

                    _ ->
                        ( state, Cmd.none )


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

                stateWithPlaylist =
                    { state | playlist = updatedPlaylist }

                nextCursor =
                    clampCursor (tracksForPanel state.activePanel stateWithPlaylist) state.cursor
            in
                let
                    nextState =
                        { stateWithPlaylist | cursor = nextCursor }
                in
                    ( nextState, persistState nextState )

        RemoveTrack index ->
            let
                updatedPlaylist =
                    List.Extra.removeAt index state.playlist

                stateWithPlaylist =
                    { state | playlist = updatedPlaylist }

                nextCursor =
                    clampCursor (tracksForPanel state.activePanel stateWithPlaylist) state.cursor
            in
                let
                    nextState =
                        { stateWithPlaylist | cursor = nextCursor }
                in
                    ( nextState, persistState nextState )

        ExportPlaylist ->
            if List.isEmpty state.playlist then
                ( state, Cmd.none )
            else
                ( state, exportMarkdown (playlistMarkdown state.playlist) )

        PlaylistName name ->
            ( { state | playlistName = name }, Cmd.none )

        SavePlaylist ->
            let
                name =
                    String.trim state.playlistName

                withoutExisting =
                    List.filter (\saved -> String.toLower saved.name /= String.toLower name) state.savedPlaylists

                nextState =
                    { state
                        | playlistName = name
                        , savedPlaylists = withoutExisting ++ [ { name = name, tracks = state.playlist } ]
                    }
            in
                if String.isEmpty name || List.isEmpty state.playlist then
                    ( state, Cmd.none )
                else
                    ( nextState, persistState nextState )

        LoadPlaylist name ->
            case List.Extra.find (\saved -> saved.name == name) state.savedPlaylists of
                Just saved ->
                    let
                        nextState =
                            { state
                                | playlist = saved.tracks
                                , playlistName = saved.name
                                , selected = Nothing
                                , cursor = 0
                            }
                    in
                        ( nextState, persistState nextState )

                Nothing ->
                    ( state, Cmd.none )

        ToggleSelection track ->
            ( toggleTrackSelection track state, Cmd.none )

        ShowPanel panel ->
            switchPanel panel state

        KeyPressed event ->
            handleKey event state

        ToggleShortcutHelp ->
            let
                opening =
                    not state.shortcutHelpOpen

                focusTarget =
                    if opening then
                        "shortcut-close"
                    else
                        "shortcut-trigger"
            in
                ( { state | shortcutHelpOpen = opening }
                , Browser.Dom.focus focusTarget |> Task.attempt FocusResult
                )

        FocusResult _ ->
            ( state, Cmd.none )

        LibraryFilter query ->
            ( { state | libraryFilter = query, selected = Nothing, cursor = 0 }, Cmd.none )

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
    Browser.Events.onKeyDown keyEventDecoder


keyEventDecoder : Json.Decode.Decoder Msg
keyEventDecoder =
    Json.Decode.map5
        (\key targetTag ctrl alt meta ->
            KeyPressed
                { key = key
                , targetTag = targetTag
                , ctrl = ctrl
                , alt = alt
                , meta = meta
                }
        )
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.at [ "target", "tagName" ] Json.Decode.string)
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "altKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


port savePlaylist : Json.Encode.Value -> Cmd msg


port exportMarkdown : String -> Cmd msg


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


savedPlaylistDecoder : Json.Decode.Decoder SavedPlaylist
savedPlaylistDecoder =
    Json.Decode.map2 SavedPlaylist
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "tracks" (Json.Decode.list trackDecoder))


storedStateDecoder : Json.Decode.Decoder StoredState
storedStateDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map2 StoredState
            (Json.Decode.field "current" (Json.Decode.list trackDecoder))
            (Json.Decode.field "saved" (Json.Decode.list savedPlaylistDecoder))
        , Json.Decode.map (\tracks -> { current = tracks, saved = [] }) (Json.Decode.list trackDecoder)
        ]
