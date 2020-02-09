port module Audio exposing (..)


port play : String -> Cmd msg


port playbackSuccess : (() -> msg) -> Sub msg


port playbackError : (() -> msg) -> Sub msg


port pause : () -> Cmd msg


port stop : () -> Cmd msg


port seek : Int -> Cmd msg


port end : (() -> msg) -> Sub msg
