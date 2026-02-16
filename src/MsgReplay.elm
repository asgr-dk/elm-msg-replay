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
type alias Program flags model msg =
    Platform.Program flags (Model model) (Msg msg)


{-| Create an HTML element managed by Elm. More about that [here](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).
-}
element :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    , encodeMsg : msg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , initMsgs : flags -> Maybe String
    , saveMsgs : String -> Cmd msg
    }
    -> Program flags model msg
element app =
    Browser.element
        { init = init app.initMsgs app.encodeMsg app.msgDecoder app.update app.init
        , update = update app.encodeMsg app.saveMsgs app.update
        , view = .model >> app.view >> Html.map Msg
        , subscriptions = .model >> app.subscriptions >> Sub.map Msg
        }


{-| Create an HTML document managed by Elm. More about that [here](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document).
-}
document :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Browser.Document msg
    , encodeMsg : msg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , initMsgs : flags -> Maybe String
    , saveMsgs : String -> Cmd msg
    }
    -> Program flags model msg
document app =
    Browser.document
        { init = init app.initMsgs app.encodeMsg app.msgDecoder app.update app.init
        , update = update app.encodeMsg app.saveMsgs app.update
        , view = viewDocument app.view
        , subscriptions = .model >> app.subscriptions >> Sub.map Msg
        }


{-| Create an application that manages Url changes. More about that [here](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application).
-}
application :
    { init : flags -> Url -> Browser.Navigation.Key -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Browser.Document msg
    , onUrlChange : Url -> msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , encodeMsg : msg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , initMsgs : flags -> Maybe String
    , saveMsgs : String -> Cmd msg
    }
    -> Program flags model msg
application app =
    Browser.application
        { init = \flags url key -> init app.initMsgs app.encodeMsg app.msgDecoder app.update (\flags_ -> app.init flags_ url key) flags
        , update = update app.encodeMsg app.saveMsgs app.update
        , view = viewDocument app.view
        , subscriptions = .model >> app.subscriptions >> Sub.map Msg
        , onUrlChange = app.onUrlChange >> Msg
        , onUrlRequest = app.onUrlRequest >> Msg
        }



-- Internals


type Msg msg
    = Msg msg
    | SaveModel


type alias Model model =
    { model : model
    , msgs : List Json.Encode.Value
    , isSaving : Bool
    }


saveModelTimeoutMilliseconds : Float
saveModelTimeoutMilliseconds =
    1000


init :
    (flags -> Maybe String)
    -> (msg -> Json.Encode.Value)
    -> Json.Decode.Decoder msg
    -> (msg -> model -> ( model, Cmd msg ))
    -> (flags -> ( model, Cmd msg ))
    -> flags
    -> ( Model model, Cmd (Msg msg) )
init initMsgs encodeMsg msgDecoder update_ init_ flags =
    Tuple.mapBoth (initModel update_ msgDecoder (initMsgs flags))
        (Cmd.map Msg)
        (init_ flags)


initModel : (msg -> model -> ( model, Cmd msg )) -> Json.Decode.Decoder msg -> Maybe String -> model -> Model model
initModel update_ msgDecoder msgsString model_ =
    replayMsgs
        update_
        msgDecoder
        (msgsString
            |> Maybe.andThen (Json.Decode.decodeString (Json.Decode.list Json.Decode.value) >> Result.toMaybe)
            |> Maybe.withDefault []
        )
        { model = model_
        , msgs = []
        , isSaving = False
        }


replayMsgs :
    (msg -> model -> ( model, Cmd msg ))
    -> Json.Decode.Decoder msg
    -> List Json.Decode.Value
    -> Model model
    -> Model model
replayMsgs update_ msgDecoder msgs model =
    case msgs of
        [] ->
            model

        head :: tail ->
            case Json.Decode.decodeValue msgDecoder head of
                Ok msg ->
                    replayMsgs update_
                        msgDecoder
                        tail
                        { model
                            | model = Tuple.first (update_ msg model.model)
                            , msgs = head :: model.msgs
                        }

                Err _ ->
                    model


update :
    (msg -> Json.Encode.Value)
    -> (String -> Cmd msg)
    -> (msg -> model -> ( model, Cmd msg ))
    -> Msg msg
    -> Model model
    -> ( Model model, Cmd (Msg msg) )
update encodeMsg saveMsgs update_ msg model =
    case msg of
        Msg msg_ ->
            updateMsg model (encodeMsg msg_) (update_ msg_ model.model)

        SaveModel ->
            updateSaveModel saveMsgs model


updateMsg :
    Model model
    -> Json.Encode.Value
    -> ( model, Cmd msg )
    -> ( Model model, Cmd (Msg msg) )
updateMsg model encodedMsg ( model_, cmd_ ) =
    ( { model
        | model = model_
        , msgs = encodedMsg :: model.msgs
        , isSaving = True
      }
    , Cmd.batch
        [ Cmd.map Msg cmd_
        , if not model.isSaving then
            Task.perform (always SaveModel) (Process.sleep saveModelTimeoutMilliseconds)

          else
            Cmd.none
        ]
    )


updateSaveModel : (String -> Cmd msg) -> Model model -> ( Model model, Cmd (Msg msg) )
updateSaveModel saveMsgs model =
    ( { model | isSaving = False }
    , Cmd.map Msg
        (saveMsgs
            (Json.Encode.encode 0
                (Json.Encode.list identity
                    (List.reverse model.msgs)
                )
            )
        )
    )


viewDocument : (model -> Browser.Document msg) -> Model model -> Browser.Document (Msg msg)
viewDocument view_ model =
    let
        { title, body } =
            view_ model.model
    in
    { title = title
    , body = List.map (Html.map Msg) body
    }
