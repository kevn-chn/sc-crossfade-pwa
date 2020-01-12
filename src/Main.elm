module Main exposing (..)

import Api exposing (Flags, url)
import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
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


type alias PlaylistInfo =
    { title : String
    , permalink_url : String
    , user : User
    }


type alias PlaylistsCollection =
    List PlaylistInfo



---- MODEL ----


type alias Model =
    { flags : Flags
    , playlists : List PlaylistInfo
    }


initialModel : Flags -> Model
initialModel flags =
    { flags = flags
    , playlists = []
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


userDecoder : Decoder User
userDecoder =
    succeed User
        |> required "avatar_url" Decode.string
        |> required "id" Decode.int
        |> required "kind" Decode.string
        |> required "last_modified" Decode.string
        |> required "permalink_url" Decode.string
        |> required "username" Decode.string


fetchPlaylistsCollection : Flags -> Cmd Msg
fetchPlaylistsCollection flags =
    Http.get
        { url = url [ "users", flags.sc_user_id, "playlists" ] flags
        , expect = Http.expectJson GotPlaylistsCollection collectionDecoder
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags
    , fetchPlaylistsCollection flags
    )



---- UPDATE ----


type Msg
    = NoOp
    | GotPlaylistsCollection (Result Http.Error PlaylistsCollection)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPlaylistsCollection (Ok collection) ->
            ( { model | playlists = collection }, Cmd.none )

        GotPlaylistsCollection (Err _) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , ul
            []
            (List.map (\playlist -> li [] [ text playlist.title ]) model.playlists)
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
