module MsgReplay exposing (Program, application)

import Browser
import Browser.Navigation
import Json.Decode
import Json.Encode
import Url exposing (Url)


type alias Program flags model msg =
    Platform.Program flags (Model model) msg


type alias Model model =
    { model : model
    , msgs : List Json.Encode.Value
    }


type alias Init flags model msg =
    flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )


init :
    (flags -> Maybe String)
    -> (msg -> Json.Encode.Value)
    -> Json.Decode.Decoder msg
    -> Update msg model
    -> Init flags model msg
    -> Init flags (Model model) msg
init loadMsgs encodeMsg msgDecoder update_ init_ flags url key =
    Tuple.mapFirst
        (\model_ ->
            incrementalComputeModel
                update_
                msgDecoder
                (loadMsgs flags
                    |> Maybe.andThen (Json.Decode.decodeString (Json.Decode.list Json.Decode.value) >> Result.toMaybe)
                    |> Maybe.withDefault []
                )
                { model = model_, msgs = [] }
        )
        (init_ flags url key)


incrementalComputeModel : Update msg model -> Json.Decode.Decoder msg -> List Json.Decode.Value -> Model model -> Model model
incrementalComputeModel update_ msgDecoder msgs model =
    case msgs of
        [] ->
            model

        head :: tail ->
            case Json.Decode.decodeValue msgDecoder head of
                Ok msg ->
                    incrementalComputeModel update_
                        msgDecoder
                        tail
                        { model
                            | model = Tuple.first (update_ msg model.model)
                            , msgs = head :: model.msgs
                        }

                Err _ ->
                    model


type alias Update msg model =
    msg -> model -> ( model, Cmd msg )


update : (msg -> Json.Encode.Value) -> (String -> Cmd msg) -> Update msg model -> Update msg (Model model)
update encodeMsg saveMsgs update_ msg model =
    Tuple.mapBoth
        (\model_ -> { model | model = model_, msgs = encodeMsg msg :: model.msgs })
        (\cmd -> Cmd.batch [ cmd, saveMsgs (Json.Encode.encode 0 (Json.Encode.list identity (List.reverse (encodeMsg msg :: model.msgs)))) ])
        (update_ msg model.model)


type alias View model msg =
    model -> Browser.Document msg


view : View model msg -> View (Model model) msg
view view_ model =
    view_ model.model


type alias Subscriptions model msg =
    model -> Sub msg


subscriptions : Subscriptions model msg -> Subscriptions (Model model) msg
subscriptions subscriptions_ model =
    subscriptions_ model.model


application :
    { init : flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Browser.Document msg
    , onUrlChange : Url -> msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , encodeMsg : msg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , loadMsgs : flags -> Maybe String
    , saveMsgs : String -> Cmd msg
    }
    -> Program flags model msg
application app =
    Browser.application
        { init = init app.loadMsgs app.encodeMsg app.msgDecoder app.update app.init
        , update = update app.encodeMsg app.saveMsgs app.update
        , view = view app.view
        , subscriptions = subscriptions app.subscriptions
        , onUrlChange = app.onUrlChange
        , onUrlRequest = app.onUrlRequest
        }
