port module Audio exposing (..)


type alias TrackMetadata =
    { title : String
    , artist : String
    , artwork : List Artwork
    }


type alias Artwork =
    { src : String
    , sizes : String
    }


port setDefaultUserId : Int -> Cmd msg


port setTrackMetadata : TrackMetadata -> Cmd msg


port play : String -> Cmd msg


port playbackSuccess : (() -> msg) -> Sub msg


port playbackError : (() -> msg) -> Sub msg


port pause : () -> Cmd msg


port resume : () -> Cmd msg


port seek : Int -> Cmd msg


port fadeInNextTrack : String -> Cmd msg


port volumeFade : Float -> Cmd msg


port end : (() -> msg) -> Sub msg



---- Media Session ----


port mediaPlay : (() -> msg) -> Sub msg


port mediaPause : (() -> msg) -> Sub msg


port mediaSeekBackward : (Int -> msg) -> Sub msg


port mediaSeekForward : (Int -> msg) -> Sub msg


port mediaPreviousTrack : (() -> msg) -> Sub msg


port mediaNextTrack : (() -> msg) -> Sub msg
