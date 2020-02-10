module Main exposing (..)

import Api exposing (Flags)
import Array exposing (Array)
import Audio
import Browser
import Dict exposing (Dict)
import FeatherIcons as Icons
import Html exposing (..)
import Html.Attributes as Attr exposing (class, disabled, href, rel, src, target, title, type_)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
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


type alias PlaylistsCollection =
    List PlaylistInfo


type alias Accordion =
    { isOpen : Bool }


type alias Player =
    { currentTrack : TrackInfo
    , currentIndex : Int
    , elapsedTime : Int
    , tracks : Array TrackInfo
    , isPlaying : Bool
    }



---- MODEL ----


type alias Model =
    { flags : Flags
    , player : Player
    , playlists : List PlaylistInfo
    , playlistUIById : Dict Int Accordion
    , user : User
    }


initialModel : Flags -> Model
initialModel flags =
    { flags = flags
    , player = defaultPlayer
    , playlists = []
    , playlistUIById = Dict.empty
    , user = defaultUser
    }


defaultAccordion : Accordion
defaultAccordion =
    { isOpen = False }


defaultPlayer : Player
defaultPlayer =
    { currentTrack = defaultTrackInfo
    , currentIndex = 0
    , elapsedTime = 0
    , tracks = Array.empty
    , isPlaying = False
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
    , permalink_url = ""
    , username = ""
    }


collectionDecoder : Decoder PlaylistsCollection
collectionDecoder =
    Decode.list playlistDecoder


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
        |> required "permalink_url" Decode.string
        |> required "username" Decode.string


fetchUserInfo : Flags -> Cmd Msg
fetchUserInfo flags =
    Http.get
        { url = Api.url [ "users", flags.sc_user_id ] flags
        , expect = Http.expectJson GotUserInfo userDecoder
        }


fetchPlaylistsCollection : Flags -> Cmd Msg
fetchPlaylistsCollection flags =
    Http.get
        { url = Api.url [ "users", flags.sc_user_id, "playlists" ] flags
        , expect = Http.expectJson GotPlaylistsCollection collectionDecoder
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags
    , Cmd.batch
        [ fetchUserInfo flags
        , fetchPlaylistsCollection flags
        ]
    )



---- UPDATE ----


type Msg
    = GotUserInfo (Result Http.Error User)
    | GotPlaylistsCollection (Result Http.Error PlaylistsCollection)
    | PlayTrack Int PlaylistInfo
    | PlaybackSuccess
    | PlaybackError
    | PauseTrack
    | ResumeTrack
    | SeekTrack String
    | MediaSeekTrack Int
    | SkipBack
    | FadeInNextTrack
    | NextTrack
    | TogglePlaylistAccordion Int
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
                    [ playCmd (Api.addQuery track.stream_url flags)
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

        GotPlaylistsCollection (Ok collection) ->
            let
                playlistUIById =
                    Dict.fromList (List.map (\list -> ( list.id, { isOpen = False } )) collection)
            in
            ( { model | playlists = collection, playlistUIById = playlistUIById }, Cmd.none )

        GotPlaylistsCollection (Err _) ->
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

        SeekTrack valueString ->
            case String.toInt valueString of
                Just value ->
                    let
                        { player } =
                            model

                        newTime =
                            value * player.currentTrack.duration // 100

                        updated =
                            { player | elapsedTime = newTime }
                    in
                    ( { model | player = updated }, Audio.seek newTime )

                Nothing ->
                    ( model, Cmd.none )

        MediaSeekTrack newTime ->
            let
                { player } =
                    model

                updated =
                    { player | elapsedTime = newTime }
            in
            ( { model | player = updated }, Audio.seek newTime )

        SkipBack ->
            if model.player.currentIndex == 0 || model.player.elapsedTime > 3000 then
                update (SeekTrack "0") model

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
                    { player | elapsedTime = 0, isPlaying = False }
            in
            initPlayback { model | player = updated } nextIndex False

        TogglePlaylistAccordion id ->
            let
                accordion =
                    case Dict.get id model.playlistUIById of
                        Just state ->
                            { state | isOpen = not state.isOpen }

                        Nothing ->
                            { isOpen = False }

                updated =
                    Dict.insert id accordion model.playlistUIById
            in
            ( { model | playlistUIById = updated }, Cmd.none )

        Tick ->
            let
                { player } =
                    model

                elapsedTime =
                    player.elapsedTime + tickAmount

                updated =
                    { player | elapsedTime = elapsedTime }

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
        , Audio.mediaSeekBackward (\t -> MediaSeekTrack t)
        , Audio.mediaSeekForward (\t -> MediaSeekTrack t)
        , Audio.mediaPreviousTrack (always SkipBack)
        , Audio.mediaNextTrack (always NextTrack)
        ]



---- VIEW ----


formatTime : Int -> String
formatTime time =
    let
        posixTime =
            millisToPosix time
    in
    [ Time.toMinute utc posixTime, Time.toSecond utc posixTime ]
        |> List.map (String.fromInt >> String.padLeft 2 '0')
        |> String.join ":"


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))


view : Model -> Html Msg
view { player, playlists, playlistUIById, user } =
    div [ class "max-w-2xl mx-auto mb-24" ]
        [ h1 [ class "p-4 text-2xl" ]
            [ a
                [ class "hover:underline"
                , href user.permalink_url
                , rel "noopener noreferrer"
                , target "_blank"
                ]
                [ text user.username ]
            ]
        , viewPlaylists playlists playlistUIById player
        , viewPlayer player
        ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    let
        { currentIndex, currentTrack, elapsedTime, tracks, isPlaying } =
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
    in
    section [ class "fixed h-16 bottom-0 left-0 w-screen bg-black text-white" ]
        [ div [ class ("flex items-center max-w-2xl mx-auto h-full p-4" ++ justify) ]
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
                        [ if String.length currentTrack.artwork_url > 0 then
                            img
                                [ class "absolute h-full"
                                , src (String.replace "-large" "-small" currentTrack.artwork_url)
                                ]
                                []

                          else
                            text ""
                        ]
                    , div [ class "flex flex-col flex-grow min-w-0" ]
                        [ p [ class "text-sm truncate" ] [ text <| currentTrack.user.username ++ " - " ++ currentTrack.title ]
                        , input
                            [ class "w-full"
                            , type_ "range"
                            , Attr.min "0"
                            , Attr.max "100"
                            , Attr.value <| String.fromInt <| player.elapsedTime * 100 // currentTrack.duration
                            , onInput SeekTrack
                            ]
                            []
                        ]
                    ]

              else
                p [ class "ml-2" ] [ text "Select a Track" ]
            , if hasTrack then
                p [ class "flex-none text-sm" ] [ text <| formatTime elapsedTime ++ " / " ++ formatTime currentTrack.duration ]

              else
                text ""
            ]
        ]


viewPlaylists : List PlaylistInfo -> Dict Int Accordion -> Player -> Html Msg
viewPlaylists playlists playlistUIById player =
    ul []
        (List.map
            (\playlist ->
                let
                    isOpen =
                        case Dict.get playlist.id playlistUIById of
                            Just state ->
                                state.isOpen

                            Nothing ->
                                False
                in
                li [ class "border rounded mb-6 shadow-lg" ]
                    [ button
                        [ class "text-xl font-bold flex justify-between items-center p-4 w-full"
                        , onClick (TogglePlaylistAccordion playlist.id)
                        ]
                        [ span [ class "w-full text-left" ] [ text playlist.title ]
                        , span [ class "ml-2 text-base font-normal" ] [ text <| formatTime playlist.duration ]
                        , a
                            [ class "h-6 ml-2"
                            , href playlist.permalink_url
                            , rel "noopener noreferrer"
                            , target "_blank"
                            , title "Open playlist in Soundcloud"
                            , onClickStopPropagation NoOp
                            ]
                            [ Icons.toHtml [] Icons.externalLink ]
                        ]
                    , if isOpen then
                        ul [ class "pb-4" ]
                            (List.indexedMap
                                (\index track -> li [] [ viewTrack track index player playlist ])
                                playlist.tracks
                            )

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
    in
    button
        [ class ("flex justify-between items-center px-4 py-1 w-full hover:bg-gray-200" ++ backgroundColor)
        , onClick playMsg
        ]
        [ div [ class "relative flex-none w-6 h-6 mr-2 bg-gray-300" ]
            [ if String.length track.artwork_url > 0 then
                img
                    [ class ("absolute h-full" ++ opacity)
                    , src (String.replace "-large" "-small" track.artwork_url)
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
        , p [ class "ml-2 text-sm font-light" ] [ text <| formatTime track.duration ]
        , a
            [ class "h-6 ml-2"
            , href track.permalink_url
            , rel "noopener noreferrer"
            , target "_blank"
            , title "Open track in Soundcloud"
            , onClickStopPropagation NoOp
            ]
            [ Icons.toHtml [ Svg.Attributes.class "p-1" ] Icons.externalLink ]
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
