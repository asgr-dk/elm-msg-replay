port module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (button, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import MsgReplay
import Url exposing (Url)


type alias Flags =
    { msgs : Maybe String }


type alias Model =
    { name : String
    , pass : String
    , isLoggedIn : Bool
    }


type Msg
    = InputName String
    | InputPass String
    | LogIn
    | LogOut


encodeMsg : Msg -> Json.Encode.Value
encodeMsg msg =
    Json.Encode.list Json.Encode.string
        (case msg of
            InputName name ->
                [ "InputName", name ]

            InputPass pass ->
                [ "InputPass", pass ]

            LogIn ->
                [ "LogIn" ]

            LogOut ->
                [ "LogOut" ]
        )


msgDecoder : Json.Decode.Decoder Msg
msgDecoder =
    Json.Decode.andThen
        (\strings ->
            case strings of
                [ "InputName", name ] ->
                    Json.Decode.succeed (InputName name)

                [ "InputPass", pass ] ->
                    Json.Decode.succeed (InputPass pass)

                [ "LogIn" ] ->
                    Json.Decode.succeed LogIn

                [ "LogOut" ] ->
                    Json.Decode.succeed LogOut

                _ ->
                    Json.Decode.fail
                        ("unrecognized message ["
                            ++ String.join ", " strings
                            ++ "]"
                        )
        )
        (Json.Decode.list Json.Decode.string)


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { name = "", pass = "", isLoggedIn = False }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputName name ->
            ( { model | name = name }, Cmd.none )

        InputPass pass ->
            ( { model | pass = pass }, Cmd.none )

        LogIn ->
            ( { model | isLoggedIn = True }, Cmd.none )

        LogOut ->
            ( { model | isLoggedIn = False }, Cmd.none )


view : Model -> Browser.Document Msg
view { isLoggedIn, name, pass } =
    { title = "Example"
    , body =
        if isLoggedIn then
            [ text ("hi " ++ name)
            , button [ onClick LogOut ] [ text "Log Out" ]
            ]

        else
            [ input [ onInput InputName, value name ] []
            , input [ onInput InputPass, value pass, type_ "password" ] []
            , button [ onClick LogIn ] [ text "Log In" ]
            ]
    }


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


port saveMsgs : String -> Cmd msg


main : MsgReplay.Program Flags Model Msg
main =
    MsgReplay.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , encodeMsg = encodeMsg
        , msgDecoder = msgDecoder
        , initMsgs = .msgs
        , saveMsgs = saveMsgs
        }
