module Main exposing (..)

import Api exposing (Flags, url)
import Browser
import Dict exposing (Dict)
import FeatherIcons as Icons
import Html exposing (..)
import Html.Attributes exposing (class, href, rel, src, target, title)
import Html.Events exposing (onClick, stopPropagationOn)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect)
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


type alias PlaylistState =
    { isOpen : Bool }


type alias PlayerState =
    { isPlaying : Bool
    , currentTrackId : Int
    }


type alias PlaylistsCollection =
    List PlaylistInfo


type alias UIState =
    { playlistsById : Dict Int PlaylistState
    , player : PlayerState
    }



---- MODEL ----


type alias Model =
    { flags : Flags
    , playlists : List PlaylistInfo
    , user : User
    , ui : UIState
    }


initialModel : Flags -> Model
initialModel flags =
    { flags = flags
    , playlists = []
    , ui =
        { player = defaultPlayerState
        , playlistsById = Dict.empty
        }
    , user = defaultUser
    }


defaultPlayerState : PlayerState
defaultPlayerState =
    { isPlaying = False
    , currentTrackId = 0
    }


defaultPlaylistState : PlaylistState
defaultPlaylistState =
    { isOpen = False }


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
        { url = url [ "users", flags.sc_user_id ] flags
        , expect = Http.expectJson GotUserInfo userDecoder
        }


fetchPlaylistsCollection : Flags -> Cmd Msg
fetchPlaylistsCollection flags =
    Http.get
        { url = url [ "users", flags.sc_user_id, "playlists" ] flags
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
    = NoOp
    | GotUserInfo (Result Http.Error User)
    | GotPlaylistsCollection (Result Http.Error PlaylistsCollection)
    | TogglePlaylistAccordion Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUserInfo (Ok user) ->
            ( { model | user = user }, Cmd.none )

        GotUserInfo (Err _) ->
            ( model, Cmd.none )

        GotPlaylistsCollection (Ok collection) ->
            let
                { ui } =
                    model

                playlistsById =
                    Dict.fromList (List.map (\list -> ( list.id, { isOpen = False } )) collection)

                uiState =
                    { ui | playlistsById = playlistsById }
            in
            ( { model | playlists = collection, ui = uiState }, Cmd.none )

        GotPlaylistsCollection (Err _) ->
            ( model, Cmd.none )

        TogglePlaylistAccordion id ->
            let
                { ui } =
                    model

                playlistState =
                    case Dict.get id ui.playlistsById of
                        Just state ->
                            state

                        Nothing ->
                            { isOpen = False }

                playlistsById =
                    Dict.insert id { playlistState | isOpen = not playlistState.isOpen } ui.playlistsById

                uiState =
                    { ui | playlistsById = playlistsById }
            in
            ( { model | ui = uiState }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))


view : Model -> Html Msg
view { playlists, ui, user } =
    div [ class "max-w-2xl m-6 md:mx-auto" ]
        [ h1 [ class "mb-4" ]
            [ a
                [ class "hover:underline"
                , href user.permalink_url
                , rel "noopener noreferrer"
                , target "_blank"
                ]
                [ text user.username ]
            ]
        , viewPlaylists playlists ui.playlistsById
        ]


viewPlaylists : List PlaylistInfo -> Dict Int PlaylistState -> Html Msg
viewPlaylists playlists uiPlaylistsById =
    ul []
        (List.map
            (\playlist ->
                let
                    isOpen =
                        case Dict.get playlist.id uiPlaylistsById of
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
                            [ href playlist.permalink_url
                            , rel "noopener noreferrer"
                            , target "_blank"
                            , title "Open playlist in Soundcloud"
                            , onClickStopPropagation NoOp
                            ]
                            [ Icons.toHtml [] Icons.externalLink ]
                        ]
                    , if isOpen then
                        ul [ class "px-4 pb-4" ] (List.map viewTrack playlist.tracks)

                      else
                        text ""
                    ]
            )
            playlists
        )


viewTrack : TrackInfo -> Html Msg
viewTrack track =
    li [ class "flex justify-between items-center py-1" ]
        [ p []
            [ span [] [ text track.user.username ]
            , text " - "
            , span [] [ text track.title ]
            ]
        , a
            [ class "hover:underline"
            , href track.permalink_url
            , rel "noopener noreferrer"
            , target "_blank"
            , title "Open track in Soundcloud"
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
