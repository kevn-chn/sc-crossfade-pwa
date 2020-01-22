module Api exposing (Flags, addQuery, url)

import Http
import Json.Decode as Decode exposing (Decoder, bool, decodeString, field, int, string)
import Url.Builder exposing (QueryParameter)


type alias Flags =
    { sc_api_client_id : String
    , sc_app_version : String
    , sc_user_id : String
    }



-- URL


apiHost : String
apiHost =
    "https://api.soundcloud.com"


appLocale : String
appLocale =
    "en"


queryString : Flags -> List QueryParameter
queryString flags =
    [ Url.Builder.string "client_id" flags.sc_api_client_id
    , Url.Builder.string "app_version" flags.sc_app_version
    , Url.Builder.string "app_locale" appLocale
    ]


{-| Get a URL to the SoundCloud API.
-}
url : List String -> Flags -> String
url paths flags =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin apiHost paths (queryString flags)


addQuery : String -> Flags -> String
addQuery path flags =
    Url.Builder.crossOrigin path [] (queryString flags)
