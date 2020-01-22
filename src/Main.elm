module Main exposing (..)

import Api exposing (Flags)
import Audio
import Browser
import Dict exposing (Dict)
import FeatherIcons as Icons
import Html exposing (..)
import Html.Attributes exposing (class, disabled, href, rel, src, target, title)
import Html.Events exposing (onClick, stopPropagationOn)
import Http
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Svg.Attributes
import Task exposing (Task)


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
    , permalink_url : String
    , user : User
    , artwork_url : String
    , stream_url : String
    , waveform_url : String
    }


type alias PlaylistInfo =
    { title : String
    , id : Int
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
    , isPlaying = False
    }


defaultTrackInfo : TrackInfo
defaultTrackInfo =
    { title = ""
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
        |> required "permalink_url" Decode.string
        |> required "user" userDecoder
        |> required "tracks" (Decode.list trackDecoder)
        |> optional "artwork_url" Decode.string ""


trackDecoder : Decoder TrackInfo
trackDecoder =
    succeed TrackInfo
        |> required "title" Decode.string
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
    | PauseTrack
    | PlayTrack TrackInfo
    | TogglePlaylistAccordion Int
    | NoOp


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

        PauseTrack ->
            let
                { player } =
                    model

                updated =
                    { player | isPlaying = False }
            in
            ( { model | player = updated }, Audio.pause () )

        PlayTrack track ->
            let
                { player } =
                    model

                updated =
                    { player | currentTrack = track, isPlaying = True }
            in
            ( { model | player = updated }, Audio.play (Api.addQuery track.stream_url model.flags) )

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

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))


view : Model -> Html Msg
view { player, playlists, playlistUIById, user } =
    div [ class "max-w-2xl m-6 md:mx-auto mb-24" ]
        [ h1 [ class "mb-4 text-2xl" ]
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
        { currentTrack, isPlaying } =
            player

        hasTrack =
            String.length currentTrack.stream_url > 0

        disabledClass =
            if not hasTrack then
                " opacity-50 cursor-not-allowed"

            else
                ""

        justifyCenter =
            if not hasTrack then
                " justify-center"

            else
                ""

        msg =
            if isPlaying then
                PauseTrack

            else
                PlayTrack currentTrack
    in
    section [ class "fixed h-16 bottom-0 left-0 w-screen bg-black text-white" ]
        [ div [ class ("flex items-center max-w-2xl mx-6 md:mx-auto h-full p-4 whitespace-no-wrap" ++ justifyCenter) ]
            [ button
                [ class ("mr-2 h-6" ++ disabledClass)
                , onClick msg
                , disabled (not hasTrack)
                ]
                [ Icons.toHtml []
                    (if isPlaying then
                        Icons.pause

                     else
                        Icons.play
                    )
                ]
            , if hasTrack then
                div []
                    [ if String.length currentTrack.artwork_url > 0 then
                        img
                            [ class "inline-block h-full mr-2"
                            , src (String.replace "-large" "-small" currentTrack.artwork_url)
                            ]
                            []

                      else
                        div [ class "inline-block w-8 mr-4 h-full bg-gray-200" ] []
                    , span [] [ text (currentTrack.user.username ++ " - " ++ currentTrack.title) ]
                    ]

              else
                p [] [ text "Select a Track" ]
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
                        [ text playlist.title
                        , a
                            [ class "h-6"
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
                            (List.map
                                (\track -> li [] [ viewTrack track (player.currentTrack == track) ])
                                playlist.tracks
                            )

                      else
                        text ""
                    ]
            )
            playlists
        )


viewTrack : TrackInfo -> Bool -> Html Msg
viewTrack track isPlaying =
    let
        backgroundColor =
            if isPlaying then
                " bg-gray-200"

            else
                ""

        opacity =
            if isPlaying then
                " opacity-25"

            else
                ""
    in
    button
        [ class ("flex justify-between items-center px-4 py-1 w-full hover:bg-gray-200" ++ backgroundColor)
        , onClick (PlayTrack track)
        ]
        [ p [ class "flex items-center" ]
            [ if String.length track.artwork_url > 0 then
                img
                    [ class ("inline-block h-6 mr-2" ++ opacity)
                    , src (String.replace "-large" "-small" track.artwork_url)
                    ]
                    []

              else
                div [ class "inline-block w-6 mr-4 h-6 bg-gray-200" ] []
            , span [] [ text (track.user.username ++ " - " ++ track.title) ]
            , if isPlaying then
                Icons.toHtml [ Svg.Attributes.class "absolute p-1" ] Icons.volume2

              else
                text ""
            ]
        , a
            [ class "h-6"
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
        , subscriptions = always Sub.none
        }
