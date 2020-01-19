module Main exposing (..)

import Api exposing (Flags, url)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, rel, src, target)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
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
    , permalink_url : String
    , user : User
    , tracks : List TrackInfo
    , waveform_url : String
    }


type alias PlaylistsCollection =
    List PlaylistInfo



---- MODEL ----


type alias Model =
    { flags : Flags
    , playlists : List PlaylistInfo
    , user : User
    }


initialModel : Flags -> Model
initialModel flags =
    { flags = flags
    , playlists = []
    , user =
        { avatar_url = ""
        , id = 0
        , kind = ""
        , last_modified = ""
        , permalink_url = ""
        , username = ""
        }
    }


collectionDecoder : Decoder PlaylistsCollection
collectionDecoder =
    Decode.list playlistDecoder


playlistDecoder : Decoder PlaylistInfo
playlistDecoder =
    succeed PlaylistInfo
        |> required "title" Decode.string
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUserInfo (Ok user) ->
            ( { model | user = user }, Cmd.none )

        GotUserInfo (Err _) ->
            ( model, Cmd.none )

        GotPlaylistsCollection (Ok collection) ->
            ( { model | playlists = collection }, Cmd.none )

        GotPlaylistsCollection (Err _) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "max-w-2xl m-6 md:mx-auto" ]
        [ h1 [ class "mb-4" ]
            [ a
                [ class "hover:underline"
                , href model.user.permalink_url
                , rel "noopener noreferrer"
                , target "_blank"
                ]
                [ text model.user.username ]
            ]
        , viewPlaylists model.playlists
        ]


viewPlaylists : List PlaylistInfo -> Html Msg
viewPlaylists playlists =
    ul []
        (List.map
            (\playlist ->
                li [ class "border rounded mb-6 p-4 shadow-lg" ]
                    [ a
                        [ class "text-xl font-bold hover:underline"
                        , href playlist.permalink_url
                        , rel "noopener noreferrer"
                        , target "_blank"
                        ]
                        [ text playlist.title ]
                    , ul [ class "mt-4" ] (List.map viewTrack playlist.tracks)
                    ]
            )
            playlists
        )


viewTrack : TrackInfo -> Html Msg
viewTrack track =
    li []
        [ a
            [ class "hover:underline"
            , href track.permalink_url
            , rel "noopener noreferrer"
            , target "_blank"
            ]
            [ span [] [ text track.user.username ]
            , text " - "
            , span [] [ text track.title ]
            ]
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
