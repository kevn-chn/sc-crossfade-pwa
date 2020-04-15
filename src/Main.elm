module Main exposing (..)

import Api exposing (Flags)
import Array exposing (Array)
import Audio
import Browser
import Dict exposing (Dict)
import FeatherIcons as Icons
import Html exposing (..)
import Html.Attributes as Attr exposing (alt, class, disabled, href, rel, src, target, title)
import Html.Events exposing (on, onClick, onInput, stopPropagationOn, targetValue)
import Http
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Svg.Attributes
import Task exposing (Task)
import Time exposing (Posix, every, millisToPosix, utc)


type alias User =
    { avatar_url : String
    , id : Int
    , kind : String
    , last_modified : String
    , permalink : String
    , permalink_url : String
    , username : String
    }


type alias TrackInfo =
    { title : String
    , id : Int
    , duration : Int
    , permalink_url : String
    , user : User
    , artwork_url : String
    , stream_url : String
    , waveform_url : String
    }


type alias PlaylistInfo =
    { title : String
    , id : Int
    , duration : Int
    , permalink_url : String
    , user : User
    , tracks : List TrackInfo
    , waveform_url : String
    }


type alias Favorites =
    { collection : List TrackInfo
    , next_href : String
    }


type alias PlaylistsCollection =
    List PlaylistInfo


type alias Accordion =
    { isOpen : Bool }


type alias Player =
    { currentTrack : TrackInfo
    , currentIndex : Int
    , elapsedTime : Int
    , displayTime : Int
    , tracks : Array TrackInfo
    , isPlaying : Bool
    , isSeeking : Bool
    }


type alias UserSearch =
    { isOpen : Bool
    , input : String
    , results : List User
    }



---- MODEL ----


type alias Model =
    { flags : Flags
    , player : Player
    , playlists : List PlaylistInfo
    , accordions : Dict Int Accordion
    , user : User
    , userSearch : UserSearch
    }


initialModel : Flags -> Model
initialModel flags =
    { flags = flags
    , player = defaultPlayer
    , playlists = []
    , accordions = Dict.empty
    , user = defaultUser
    , userSearch = defaultUserSearch
    }


defaultAccordion : Accordion
defaultAccordion =
    { isOpen = False }


defaultPlayer : Player
defaultPlayer =
    { currentTrack = defaultTrackInfo
    , currentIndex = 0
    , elapsedTime = 0
    , displayTime = 0
    , tracks = Array.empty
    , isPlaying = False
    , isSeeking = False
    }


defaultTrackInfo : TrackInfo
defaultTrackInfo =
    { title = ""
    , id = 0
    , duration = 0
    , permalink_url = ""
    , user = defaultUser
    , artwork_url = ""
    , stream_url = ""
    , waveform_url = ""
    }


defaultUser : User
defaultUser =
    { avatar_url = ""
    , id = 0
    , kind = ""
    , last_modified = ""
    , permalink = ""
    , permalink_url = ""
    , username = ""
    }


defaultUserSearch : UserSearch
defaultUserSearch =
    { isOpen = False
    , input = ""
    , results = []
    }


favoritesDecoder : Decoder Favorites
favoritesDecoder =
    succeed Favorites
        |> required "collection" (Decode.list trackDecoder)
        |> optional "next_href" Decode.string ""


playlistDecoder : Decoder PlaylistInfo
playlistDecoder =
    succeed PlaylistInfo
        |> required "title" Decode.string
        |> required "id" Decode.int
        |> required "duration" Decode.int
        |> required "permalink_url" Decode.string
        |> required "user" userDecoder
        |> required "tracks" (Decode.list trackDecoder)
        |> optional "artwork_url" Decode.string ""


trackDecoder : Decoder TrackInfo
trackDecoder =
    succeed TrackInfo
        |> required "title" Decode.string
        |> required "id" Decode.int
        |> required "duration" Decode.int
        |> required "permalink_url" Decode.string
        |> required "user" userDecoder
        |> optional "artwork_url" Decode.string ""
        |> optional "stream_url" Decode.string ""
        |> optional "waveform_url" Decode.string ""


userDecoder : Decoder User
userDecoder =
    succeed User
        |> required "avatar_url" Decode.string
        |> required "id" Decode.int
        |> required "kind" Decode.string
        |> required "last_modified" Decode.string
        |> required "permalink" Decode.string
        |> required "permalink_url" Decode.string
        |> required "username" Decode.string


fetchUserInfo : Int -> Flags -> Cmd Msg
fetchUserInfo userId flags =
    Http.get
        { url = Api.url [ "users", String.fromInt userId ] flags []
        , expect = Http.expectJson GotUserInfo userDecoder
        }


fetchPlaylistsCollection : Int -> Flags -> Cmd Msg
fetchPlaylistsCollection userId flags =
    Http.get
        { url = Api.url [ "users", String.fromInt userId, "playlists" ] flags []
        , expect = Http.expectJson GotPlaylistsCollection (Decode.list playlistDecoder)
        }


fetchFavorites : Int -> Flags -> Cmd Msg
fetchFavorites userId flags =
    Http.get
        { url = Api.url [ "users", String.fromInt userId, "favorites" ] flags [ ( "linked_partitioning", "1" ), ( "page_size", "50" ) ]
        , expect = Http.expectJson GotFavorites favoritesDecoder
        }


searchUsers : String -> Flags -> Cmd Msg
searchUsers searchTerm flags =
    Http.get
        { url = Api.url [ "users" ] flags [ ( "q", searchTerm ) ]
        , expect = Http.expectJson GotUserSearch (Decode.list userDecoder)
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags
    , Cmd.batch
        [ fetchUserInfo flags.sc_user_id flags
        , fetchFavorites flags.sc_user_id flags
        , fetchPlaylistsCollection flags.sc_user_id flags
        ]
    )



---- UPDATE ----


type Msg
    = GotUserInfo (Result Http.Error User)
    | GotFavorites (Result Http.Error Favorites)
    | GotPlaylistsCollection (Result Http.Error PlaylistsCollection)
    | GotUserSearch (Result Http.Error (List User))
    | PlayTrack Int PlaylistInfo
    | PlaybackSuccess
    | PlaybackError
    | PauseTrack
    | ResumeTrack
    | SeekTrackStart String
    | SeekTrackRelease String
    | SeekTrack Int
    | SetDefaultUserId Int
    | SkipBack
    | FadeInNextTrack
    | NextTrack
    | SearchUsers String
    | SwitchUser User
    | TogglePlaylistAccordion Int
    | ToggleUserSearch
    | Tick
    | NoOp


initPlayback : Model -> Int -> Bool -> ( Model, Cmd Msg )
initPlayback model trackIndex isFadeIn =
    case Array.get trackIndex model.player.tracks of
        Just track ->
            let
                { flags, player } =
                    model

                updated =
                    { player
                        | currentIndex = trackIndex
                        , currentTrack = track
                        , elapsedTime = 0
                        , displayTime = 0
                        , isPlaying = False
                    }

                playCmd =
                    if isFadeIn then
                        Audio.fadeInNextTrack

                    else
                        Audio.play

                metadata =
                    { title = track.title
                    , artist = track.user.username
                    , artwork =
                        [ { src = String.replace "-large" "-t300x300" track.artwork_url, sizes = "300x300" }
                        , { src = String.replace "-large" "-t500x500" track.artwork_url, sizes = "500x500" }
                        ]
                    }
            in
            if track == player.currentTrack then
                ( model, Cmd.none )

            else
                ( { model | player = updated }
                , Cmd.batch
                    [ playCmd (Api.addQuery track.stream_url flags [])
                    , Audio.setTrackMetadata metadata
                    ]
                )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUserInfo (Ok user) ->
            ( { model | user = user }, Cmd.none )

        GotUserInfo (Err _) ->
            ( model, Cmd.none )

        GotFavorites (Ok favorites) ->
            let
                accordions =
                    Dict.insert 0 { isOpen = False } model.accordions

                likes =
                    { title = "Likes"
                    , id = 0
                    , duration = 0
                    , permalink_url = model.user.permalink_url ++ "/likes"
                    , user = model.user
                    , tracks = favorites.collection
                    , waveform_url = ""
                    }

                playlists =
                    likes :: model.playlists
            in
            ( { model | playlists = playlists, accordions = accordions }, Cmd.none )

        GotFavorites (Err _) ->
            ( model, Cmd.none )

        GotPlaylistsCollection (Ok collection) ->
            let
                newAccordions =
                    Dict.fromList (List.map (\list -> ( list.id, { isOpen = False } )) collection)

                accordions =
                    Dict.union model.accordions newAccordions

                playlists =
                    List.append model.playlists collection
            in
            ( { model | playlists = playlists, accordions = accordions }, Cmd.none )

        GotPlaylistsCollection (Err _) ->
            ( model, Cmd.none )

        GotUserSearch (Ok results) ->
            let
                { userSearch } =
                    model

                updated =
                    { userSearch | results = results }
            in
            ( { model | userSearch = updated }, Cmd.none )

        GotUserSearch (Err _) ->
            ( model, Cmd.none )

        PlayTrack trackIndex playlist ->
            let
                { player } =
                    model

                tracks =
                    Array.fromList playlist.tracks

                updated =
                    { player | tracks = tracks }
            in
            initPlayback { model | player = updated } trackIndex False

        PlaybackSuccess ->
            let
                { player } =
                    model

                updated =
                    { player | isPlaying = True }
            in
            ( { model | player = updated }, Cmd.none )

        PlaybackError ->
            -- TODO: Add error handling
            ( model, Cmd.none )

        PauseTrack ->
            let
                { player } =
                    model

                updated =
                    { player | isPlaying = False }
            in
            ( { model | player = updated }, Audio.pause () )

        ResumeTrack ->
            ( model, Audio.resume () )

        SeekTrackStart valueString ->
            case String.toInt valueString of
                Just value ->
                    let
                        { player } =
                            model

                        newTime =
                            value * player.currentTrack.duration // 500

                        updated =
                            { player | displayTime = newTime, isSeeking = True }
                    in
                    ( { model | player = updated }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SeekTrackRelease valueString ->
            case String.toInt valueString of
                Just value ->
                    let
                        { player } =
                            model

                        newTime =
                            value * player.currentTrack.duration // 500

                        updated =
                            { player | elapsedTime = newTime, displayTime = newTime, isSeeking = False }
                    in
                    ( { model | player = updated }, Audio.seek newTime )

                Nothing ->
                    ( model, Cmd.none )

        SeekTrack newTime ->
            let
                { player } =
                    model

                updated =
                    { player | elapsedTime = newTime, displayTime = newTime }
            in
            ( { model | player = updated }, Audio.seek newTime )

        SetDefaultUserId id ->
            let
                { flags } =
                    model

                updated =
                    { flags | sc_user_id = id }
            in
            ( { model | flags = updated }, Audio.setDefaultUserId id )

        SkipBack ->
            if model.player.currentIndex == 0 || model.player.elapsedTime > 3000 then
                update (SeekTrack 0) model

            else
                initPlayback model (model.player.currentIndex - 1) False

        FadeInNextTrack ->
            let
                { player } =
                    model

                nextIndex =
                    player.currentIndex + 1
            in
            initPlayback model nextIndex True

        NextTrack ->
            let
                { player } =
                    model

                nextIndex =
                    player.currentIndex + 1

                updated =
                    { player | elapsedTime = 0, displayTime = 0, isPlaying = False }
            in
            initPlayback { model | player = updated } nextIndex False

        SearchUsers input ->
            let
                { flags, userSearch } =
                    model

                updated =
                    if String.isEmpty input then
                        { userSearch | input = "", results = [] }

                    else
                        { userSearch | input = input }

                searchCmd =
                    if String.isEmpty input then
                        Cmd.none

                    else
                        searchUsers input flags
            in
            ( { model | userSearch = updated }, searchCmd )

        SwitchUser user ->
            ( { model | playlists = [], accordions = Dict.empty, user = user, userSearch = defaultUserSearch }
            , Cmd.batch
                [ fetchFavorites user.id model.flags
                , fetchPlaylistsCollection user.id model.flags
                ]
            )

        TogglePlaylistAccordion id ->
            let
                accordion =
                    case Dict.get id model.accordions of
                        Just state ->
                            { state | isOpen = not state.isOpen }

                        Nothing ->
                            { isOpen = False }

                updated =
                    Dict.insert id accordion model.accordions
            in
            ( { model | accordions = updated }, Cmd.none )

        ToggleUserSearch ->
            let
                { userSearch } =
                    model

                updated =
                    { userSearch | isOpen = not userSearch.isOpen }
            in
            ( { model | userSearch = updated }, Cmd.none )

        Tick ->
            let
                { player } =
                    model

                elapsedTime =
                    player.elapsedTime + tickAmount

                displayTime =
                    if not player.isSeeking then
                        elapsedTime

                    else
                        player.displayTime

                updated =
                    { player | elapsedTime = elapsedTime, displayTime = displayTime }

                duration =
                    player.currentTrack.duration
            in
            if (duration - elapsedTime) < fadeTime then
                update FadeInNextTrack { model | player = updated }

            else if elapsedTime < fadeTime then
                ( { model | player = updated }, Audio.volumeFade <| tickAmount / fadeTime )

            else
                ( { model | player = updated }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


fadeTime =
    6000


tickAmount =
    200


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.player.isPlaying then
            every tickAmount <| always Tick

          else
            Sub.none
        , Audio.end (always NextTrack)
        , Audio.playbackSuccess (always PlaybackSuccess)
        , Audio.playbackError (always PlaybackError)
        , Audio.mediaPlay (always ResumeTrack)
        , Audio.mediaPause (always PauseTrack)
        , Audio.mediaSeekBackward (\t -> SeekTrack t)
        , Audio.mediaSeekForward (\t -> SeekTrack t)
        , Audio.mediaPreviousTrack (always SkipBack)
        , Audio.mediaNextTrack (always NextTrack)
        ]



---- VIEW ----


formatTime : Int -> String
formatTime time =
    let
        hourInMillis =
            3600000

        posixTime =
            millisToPosix time

        units =
            if time >= hourInMillis then
                [ Time.toHour, Time.toMinute, Time.toSecond ]

            else
                [ Time.toMinute, Time.toSecond ]

        convertToUnit toUnit =
            toUnit utc posixTime
    in
    units
        |> List.map (convertToUnit >> String.fromInt >> String.padLeft 2 '0')
        |> String.join ":"


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))


onCustomInput : String -> (String -> msg) -> Attribute msg
onCustomInput event message =
    on event <| Decode.map message targetValue


getTrackArtworkUrl : TrackInfo -> String
getTrackArtworkUrl track =
    if not (String.isEmpty track.artwork_url) then
        track.artwork_url

    else if not (String.isEmpty track.user.avatar_url) then
        track.user.avatar_url

    else
        ""


view : Model -> Html Msg
view { flags, player, playlists, accordions, user, userSearch } =
    div [ class "max-w-screen-sm mx-auto mb-24" ]
        [ div [ class "p-4 sm:px-0 flex justify-between" ]
            [ h1 [ class "flex items-center" ]
                [ div
                    [ class
                        "inline-block rounded-full overflow-hidden relative w-6 h-6 mr-2 bg-gray-300"
                    ]
                    [ if not (String.isEmpty user.avatar_url) then
                        img
                            [ class "absolute h-full"
                            , src (String.replace "-large" "-small" user.avatar_url)
                            , alt "User Avatar"
                            ]
                            []

                      else
                        text ""
                    ]
                , a
                    [ class "hover:underline text-xl mr-2"
                    , href user.permalink_url
                    , rel "noopener noreferrer"
                    , target "_blank"
                    ]
                    [ text user.permalink ]
                , span [ class "text-sm font-light" ] [ text user.username ]
                ]
            , div [ class "flex items-center" ]
                [ if user.id == flags.sc_user_id then
                    p [ class "p-2 mr-2 text-xs font-light text-gray-700" ]
                        [ text "Default" ]

                  else
                    button
                        [ class "p-2 mr-2 text-xs hover:underline"
                        , onClick (SetDefaultUserId user.id)
                        ]
                        [ text "Set as Default" ]
                , button
                    [ class "border rounded hover:bg-gray-200 p-2 text-xs"
                    , onClick ToggleUserSearch
                    ]
                    [ if userSearch.isOpen then
                        text "Collapse User Search"

                      else
                        text "Switch User"
                    ]
                ]
            ]
        , if userSearch.isOpen then
            div [ class "mx-4 sm:mx-0 mb-4 relative" ]
                [ input
                    [ class "border rounded p-2 w-full"
                    , Attr.placeholder "Search Users"
                    , onInput SearchUsers
                    , Attr.type_ "text"
                    , Attr.value userSearch.input
                    ]
                    []
                , if List.length userSearch.results > 0 then
                    ul [ class "bg-white absolute border rounded w-full z-10 shadow-md" ]
                        (List.map
                            (\result ->
                                li []
                                    [ button
                                        [ class "hover:bg-gray-200 w-full p-2 text-left"
                                        , onClick (SwitchUser result)
                                        ]
                                        [ span [ class "mr-2" ] [ text result.permalink ]
                                        , span [ class "text-sm font-light" ] [ text result.username ]
                                        ]
                                    ]
                            )
                            userSearch.results
                        )

                  else
                    text ""
                ]

          else
            text ""
        , viewPlaylists playlists accordions player
        , viewPlayer player
        ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    let
        { currentIndex, currentTrack, displayTime, tracks, isPlaying } =
            player

        hasTrack =
            String.length currentTrack.stream_url > 0

        disabledClass : Bool -> String
        disabledClass isDisabled =
            if isDisabled then
                " opacity-50 cursor-not-allowed"

            else
                ""

        justify =
            if not hasTrack then
                " justify-center"

            else
                " justify-between"

        playMsg =
            if isPlaying then
                PauseTrack

            else
                ResumeTrack

        artwork_url =
            getTrackArtworkUrl currentTrack
    in
    section [ class "fixed h-16 bottom-0 left-0 w-screen bg-black text-white" ]
        [ div [ class ("flex items-center max-w-screen-sm mx-auto h-full p-4" ++ justify) ]
            [ div [ class "flex flex-none items-center" ]
                [ button
                    [ class <| "h-6" ++ (disabledClass <| not hasTrack)
                    , onClick SkipBack
                    , disabled <| not hasTrack
                    ]
                    [ Icons.toHtml [ Svg.Attributes.class "p-1" ] Icons.skipBack ]
                , button
                    [ class <| "h-6 ml-2" ++ (disabledClass <| not hasTrack)
                    , onClick playMsg
                    , disabled <| not hasTrack
                    ]
                    [ Icons.toHtml []
                        (if isPlaying then
                            Icons.pause

                         else
                            Icons.play
                        )
                    ]
                , button
                    [ class <| "h-6 ml-2" ++ (disabledClass <| currentIndex + 1 >= Array.length tracks)
                    , onClick NextTrack
                    , disabled <| currentIndex + 1 >= Array.length tracks
                    ]
                    [ Icons.toHtml [ Svg.Attributes.class "p-1" ] Icons.skipForward ]
                ]
            , if hasTrack then
                div [ class "flex flex-grow min-w-0 mx-4 md:mx-8 justify-between items-center" ]
                    [ div [ class "hidden sm:block flex-none relative w-8 h-8 mr-4 bg-gray-300" ]
                        [ if not (String.isEmpty artwork_url) then
                            img
                                [ class "absolute h-full"
                                , src (String.replace "-large" "-small" artwork_url)
                                ]
                                []

                          else
                            text ""
                        ]
                    , div [ class "flex flex-col flex-grow min-w-0" ]
                        [ p [ class "text-sm truncate" ] [ text <| currentTrack.user.username ++ " - " ++ currentTrack.title ]
                        , input
                            [ class "w-full"
                            , Attr.type_ "range"
                            , Attr.min "0"
                            , Attr.max "500"
                            , Attr.value <| String.fromInt <| displayTime * 500 // currentTrack.duration
                            , onInput SeekTrackStart
                            , onCustomInput "mouseup" SeekTrackRelease
                            , onCustomInput "touchend" SeekTrackRelease
                            ]
                            []
                        ]
                    ]

              else
                p [ class "ml-2" ] [ text "Select a Track" ]
            , if hasTrack then
                p [ class "flex-none text-sm" ] [ text <| formatTime displayTime ++ " / " ++ formatTime currentTrack.duration ]

              else
                text ""
            ]
        ]


viewPlaylists : List PlaylistInfo -> Dict Int Accordion -> Player -> Html Msg
viewPlaylists playlists accordions player =
    ul []
        (List.map
            (\playlist ->
                let
                    isOpen =
                        case Dict.get playlist.id accordions of
                            Just state ->
                                state.isOpen

                            Nothing ->
                                False
                in
                li [ class "border rounded mb-4 shadow-lg" ]
                    [ button
                        [ class "rounded text-xl font-bold flex justify-between items-center p-4 w-full group"
                        , onClick (TogglePlaylistAccordion playlist.id)
                        ]
                        [ span [ class "w-full text-left" ] [ text playlist.title ]
                        , a
                            [ class "h-6 ml-2 sr-only group-hover:not-sr-only"
                            , href playlist.permalink_url
                            , rel "noopener noreferrer"
                            , target "_blank"
                            , title "Open playlist in Soundcloud"
                            , onClickStopPropagation NoOp
                            ]
                            [ Icons.toHtml [ Svg.Attributes.class "p-1" ] Icons.externalLink ]
                        , span [ class "ml-2 text-base font-normal" ]
                            [ if playlist.duration > 0 then
                                text <| formatTime playlist.duration

                              else
                                text "––:––"
                            ]
                        ]
                    , if isOpen then
                        if not (List.isEmpty playlist.tracks) then
                            ul [ class "pb-4" ]
                                (List.indexedMap
                                    (\index track -> li [] [ viewTrack track index player playlist ])
                                    playlist.tracks
                                )

                        else
                            p [ class "px-4 py-2" ] [ text "No tracks available." ]

                      else
                        text ""
                    ]
            )
            playlists
        )


viewTrack : TrackInfo -> Int -> Player -> PlaylistInfo -> Html Msg
viewTrack track trackIndex player playlist =
    let
        isSelected =
            player.currentTrack == track

        isPlaying =
            isSelected && player.isPlaying

        backgroundColor =
            if isSelected then
                " bg-gray-200"

            else
                ""

        bold =
            if isSelected then
                " font-bold"

            else
                ""

        opacity =
            if isPlaying then
                " opacity-25"

            else
                ""

        playMsg =
            if isPlaying then
                PauseTrack

            else if isSelected then
                ResumeTrack

            else
                PlayTrack trackIndex playlist

        artwork_url =
            getTrackArtworkUrl track
    in
    button
        [ class ("flex justify-between items-center px-4 py-1 w-full group hover:bg-gray-200" ++ backgroundColor)
        , onClick playMsg
        ]
        [ div [ class "relative flex-none w-6 h-6 mr-2 bg-gray-300" ]
            [ if not (String.isEmpty artwork_url) then
                img
                    [ class ("absolute h-full" ++ opacity)
                    , src (String.replace "-large" "-small" artwork_url)
                    ]
                    []

              else
                text ""
            , if isPlaying then
                Icons.toHtml [ Svg.Attributes.class "absolute left-0 p-1" ] Icons.volume2

              else
                text ""
            ]
        , p [ class ("flex-grow text-left truncate" ++ bold) ]
            [ text (track.user.username ++ " - " ++ track.title) ]
        , a
            [ class "h-6 ml-2 sr-only group-hover:not-sr-only"
            , href track.permalink_url
            , rel "noopener noreferrer"
            , target "_blank"
            , title "Open track in Soundcloud"
            , onClickStopPropagation NoOp
            ]
            [ Icons.toHtml [ Svg.Attributes.class "p-1" ] Icons.externalLink ]
        , p [ class "ml-2 text-sm font-light" ] [ text <| formatTime track.duration ]
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
