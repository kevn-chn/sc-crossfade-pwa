module Api exposing (Flags, addQuery, url)

import Http
import Json.Decode as Decode exposing (Decoder, bool, decodeString, field, int, string)
import Url.Builder exposing (QueryParameter)


type alias Flags =
    { sc_api_client_id : String
    , sc_app_version : String
    , sc_user_id : Int
    }



-- URL


apiHost : String
apiHost =
    "https://api.soundcloud.com"


appLocale : String
appLocale =
    "en"


queryString : Flags -> List ( String, String ) -> List QueryParameter
queryString flags queries =
    List.append
        [ Url.Builder.string "client_id" flags.sc_api_client_id
        , Url.Builder.string "app_version" flags.sc_app_version
        , Url.Builder.string "app_locale" appLocale
        ]
        (List.map
            (\( key, value ) -> Url.Builder.string key value)
            queries
        )


{-| Get a URL to the SoundCloud API.
-}
url : List String -> Flags -> List ( String, String ) -> String
url paths flags queries =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin apiHost paths (queryString flags queries)


addQuery : String -> Flags -> List ( String, String ) -> String
addQuery path flags queries =
    Url.Builder.crossOrigin path [] (queryString flags queries)
