module MsgReplay exposing (Program, application, document, element)

{-| Reliable hot reloading for Elm programs!


# Programs

This API mirrors the `elm/browser` package found [here](https://package.elm-lang.org/packages/elm/browser/latest).

@docs Program, application, document, element

-}

import Browser
import Browser.Navigation
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Process
import Task
import Url exposing (Url)



-- API


{-| A program with message-replay enabled.
-}
type alias Program flags appModel appMsg =
    Platform.Program flags (Model appModel) (Msg appMsg)


{-| Create an HTML element managed by Elm. More about that [here](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).
-}
element :
    { init : flags -> ( appModel, Cmd appMsg )
    , update : appMsg -> appModel -> ( appModel, Cmd appMsg )
    , subscriptions : appModel -> Sub appMsg
    , view : appModel -> Html appMsg
    , enableReplay : flags -> Bool
    , initMsgs : flags -> Json.Decode.Value
    , saveMsgs : Json.Encode.Value -> Cmd appMsg
    , saveMsgsDelayMillis : Float
    , encodeMsg : appMsg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder appMsg
    }
    -> Program flags appModel appMsg
element app =
    Browser.element
        { init = init app
        , update = update app
        , view = .appModel >> app.view >> Html.map AppMsg
        , subscriptions = .appModel >> app.subscriptions >> Sub.map AppMsg
        }


{-| Create an HTML document managed by Elm. More about that [here](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document).
-}
document :
    { init : flags -> ( appModel, Cmd appMsg )
    , update : appMsg -> appModel -> ( appModel, Cmd appMsg )
    , subscriptions : appModel -> Sub appMsg
    , view : appModel -> Browser.Document appMsg
    , enableReplay : flags -> Bool
    , initMsgs : flags -> Json.Decode.Value
    , saveMsgs : Json.Encode.Value -> Cmd appMsg
    , saveMsgsDelayMillis : Float
    , encodeMsg : appMsg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder appMsg
    }
    -> Program flags appModel appMsg
document app =
    Browser.document
        { init = init app
        , update = update app
        , view = viewDocument app.view
        , subscriptions = .appModel >> app.subscriptions >> Sub.map AppMsg
        }


{-| Create an application that manages Url changes. More about that [here](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application).
-}
application :
    { init : flags -> Url -> Browser.Navigation.Key -> ( appModel, Cmd appMsg )
    , update : appMsg -> appModel -> ( appModel, Cmd appMsg )
    , subscriptions : appModel -> Sub appMsg
    , view : appModel -> Browser.Document appMsg
    , onUrlChange : Url -> appMsg
    , onUrlRequest : Browser.UrlRequest -> appMsg
    , enableReplay : flags -> Bool
    , initMsgs : flags -> Json.Decode.Value
    , saveMsgs : Json.Encode.Value -> Cmd appMsg
    , saveMsgsDelayMillis : Float
    , encodeMsg : appMsg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder appMsg
    }
    -> Program flags appModel appMsg
application app =
    Browser.application
        { update = update app
        , view = viewDocument app.view
        , subscriptions = .appModel >> app.subscriptions >> Sub.map AppMsg
        , onUrlChange = app.onUrlChange >> AppMsg
        , onUrlRequest = app.onUrlRequest >> AppMsg
        , init =
            \flags url key ->
                init
                    { init = \flags_ -> app.init flags_ url key
                    , enableReplay = app.enableReplay
                    , initMsgs = app.initMsgs
                    , encodeMsg = app.encodeMsg
                    , msgDecoder = app.msgDecoder
                    , update = app.update
                    }
                    flags
        }



-- Internals


type Msg appMsg
    = AppMsg appMsg
    | SaveAppModel


type alias Model appModel =
    { appModel : appModel
    , appMsgs : List Json.Encode.Value
    , isSaving : Bool
    , isEnabled : Bool
    }


init :
    { app
        | enableReplay : flags -> Bool
        , initMsgs : flags -> Json.Decode.Value
        , encodeMsg : appMsg -> Json.Encode.Value
        , msgDecoder : Json.Decode.Decoder appMsg
        , update : appMsg -> appModel -> ( appModel, Cmd appMsg )
        , init : flags -> ( appModel, Cmd appMsg )
    }
    -> flags
    -> ( Model appModel, Cmd (Msg appMsg) )
init app flags =
    Tuple.mapBoth
        (if app.enableReplay flags then
            initEnabled app flags

         else
            initDisabled
        )
        (Cmd.map AppMsg)
        (app.init flags)


initDisabled : appModel -> Model appModel
initDisabled appModel =
    { appModel = appModel
    , appMsgs = []
    , isSaving = False
    , isEnabled = False
    }


initEnabled :
    { app
        | update : appMsg -> appModel -> ( appModel, Cmd appMsg )
        , msgDecoder : Json.Decode.Decoder appMsg
        , initMsgs : flags -> Json.Decode.Value
    }
    -> flags
    -> appModel
    -> Model appModel
initEnabled app flags appModel =
    replayAppMsgs
        app
        (Result.withDefault []
            (Json.Decode.decodeValue
                (Json.Decode.list Json.Decode.value)
                (app.initMsgs flags)
            )
        )
        { appModel = appModel
        , appMsgs = []
        , isSaving = False
        , isEnabled = True
        }


replayAppMsgs :
    { app
        | update : appMsg -> appModel -> ( appModel, Cmd appMsg )
        , msgDecoder : Json.Decode.Decoder appMsg
    }
    -> List Json.Decode.Value
    -> Model appModel
    -> Model appModel
replayAppMsgs app appMsgs model =
    case appMsgs of
        [] ->
            model

        head :: tail ->
            case Json.Decode.decodeValue app.msgDecoder head of
                Ok appMsg ->
                    replayAppMsgs app
                        tail
                        { model
                            | appModel = Tuple.first (app.update appMsg model.appModel)
                            , appMsgs = head :: model.appMsgs
                        }

                Err _ ->
                    model


update :
    { app
        | saveMsgsDelayMillis : Float
        , encodeMsg : appMsg -> Json.Encode.Value
        , saveMsgs : Json.Encode.Value -> Cmd appMsg
        , update : appMsg -> appModel -> ( appModel, Cmd appMsg )
    }
    -> Msg appMsg
    -> Model appModel
    -> ( Model appModel, Cmd (Msg appMsg) )
update app msg model =
    case msg of
        AppMsg appMsg ->
            if model.isEnabled then
                updateAppMsgEnabled model
                    app.saveMsgsDelayMillis
                    (app.encodeMsg appMsg)
                    (app.update appMsg model.appModel)

            else
                updateAppMsgDisabled model
                    (app.update appMsg model.appModel)

        SaveAppModel ->
            updateSaveAppModel app.saveMsgs model


updateAppMsgDisabled : Model appModel -> ( appModel, Cmd appMsg ) -> ( Model appModel, Cmd (Msg appMsg) )
updateAppMsgDisabled model ( appModel, appCmd ) =
    ( { model | appModel = appModel }, Cmd.map AppMsg appCmd )


updateAppMsgEnabled :
    Model appModel
    -> Float
    -> Json.Encode.Value
    -> ( appModel, Cmd appMsg )
    -> ( Model appModel, Cmd (Msg appMsg) )
updateAppMsgEnabled model saveAppMsgsTimeout appMsgJson ( appModel, appCmd ) =
    ( { model
        | appModel = appModel
        , appMsgs = appMsgJson :: model.appMsgs
        , isSaving = True
      }
    , Cmd.batch
        [ Cmd.map AppMsg appCmd
        , if not model.isSaving then
            Task.perform (always SaveAppModel) (Process.sleep saveAppMsgsTimeout)

          else
            Cmd.none
        ]
    )


updateSaveAppModel : (Json.Encode.Value -> Cmd appMsg) -> Model appModel -> ( Model appModel, Cmd (Msg appMsg) )
updateSaveAppModel saveAppMsgs model =
    ( { model | isSaving = False }
    , Cmd.map AppMsg
        (saveAppMsgs
            (Json.Encode.list identity
                (List.reverse model.appMsgs)
            )
        )
    )


viewDocument : (appModel -> Browser.Document appMsg) -> Model appModel -> Browser.Document (Msg appMsg)
viewDocument viewApp model =
    let
        { title, body } =
            viewApp model.appModel
    in
    { title = title
    , body = List.map (Html.map AppMsg) body
    }
