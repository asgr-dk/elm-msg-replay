module MsgReplay exposing (Configuration, Program, application)

import Browser
import Browser.Navigation
import Json.Decode
import Json.Encode
import Url exposing (Url)


type alias Program flags model msg =
    Platform.Program flags model msg


type alias Configuration flags msg =
    { encodeMsg : msg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , fromCache : flags -> Maybe String
    , toCache : String -> Cmd msg
    }


application :
    Configuration flags msg
    ->
        { init : flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Browser.Document msg
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        }
    -> Program flags model msg
application config app =
    Browser.application
        { init = app.init
        , update = app.update
        , view = app.view
        , subscriptions = app.subscriptions
        , onUrlChange = app.onUrlChange
        , onUrlRequest = app.onUrlRequest
        }
