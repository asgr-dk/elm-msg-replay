port module Main exposing (main)

import Browser
import Browser.Navigation
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import MsgReplay
import Url exposing (Url)


port input : (String -> msg) -> Sub msg


port output : String -> Cmd msg


type Msg
    = DoNothing
    | InputName String
    | InputPass String
    | LogIn
    | LogOut


type Model
    = LoggedOut { name : String, pass : String }
    | LoggedIn { name : String }


type alias Flags =
    {}


main : MsgReplay.Program Flags Model Msg
main =
    MsgReplay.application
        { encodeMsg = encodeMsg
        , msgDecoder = msgDecoder
        , fromCache = always Nothing
        , toCache = output
        }
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = always DoNothing
        , onUrlRequest = always DoNothing
        }


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( LoggedOut { name = "", pass = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( DoNothing, _ ) ->
            ( model, Cmd.none )

        ( InputName name, LoggedOut loggedOut ) ->
            ( LoggedOut { loggedOut | name = name }, Cmd.none )

        ( InputPass pass, LoggedOut loggedOut ) ->
            ( LoggedOut { loggedOut | pass = pass }, Cmd.none )

        ( LogIn, LoggedOut { name } ) ->
            ( LoggedIn { name = name }, Cmd.none )

        ( LogOut, LoggedIn { name } ) ->
            ( LoggedOut { name = name, pass = "" }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        case model of
            LoggedOut { name, pass } ->
                [ Html.input [ Html.Events.onInput InputName, Html.Attributes.value name ] []
                , Html.input [ Html.Events.onInput InputPass, Html.Attributes.value pass ] []
                , Html.button [ Html.Events.onClick LogIn ] [ Html.text "Log In" ]
                ]

            LoggedIn { name } ->
                [ Html.text ("hi " ++ name)
                , Html.button [ Html.Events.onClick LogOut ] [ Html.text "Log Out" ]
                ]
    }


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


encodeMsg : Msg -> Json.Encode.Value
encodeMsg msg =
    case msg of
        DoNothing ->
            Json.Encode.null

        InputName name ->
            Json.Encode.list Json.Encode.string [ "InputName", name ]

        InputPass pass ->
            Json.Encode.list Json.Encode.string [ "InputPass", pass ]

        LogIn ->
            Json.Encode.string "LogIn"

        LogOut ->
            Json.Encode.string "LogOut"


msgDecoder : Json.Decode.Decoder Msg
msgDecoder =
    Json.Decode.oneOf
        [ Json.Decode.null DoNothing
        , Json.Decode.andThen
            (\list ->
                case list of
                    [ "InputName", name ] ->
                        Json.Decode.succeed (InputName name)

                    [ "InputPass", pass ] ->
                        Json.Decode.succeed (InputPass pass)

                    _ ->
                        Json.Decode.fail ("unrecognized list [" ++ String.join "," list ++ "]")
            )
            (Json.Decode.list Json.Decode.string)
        , Json.Decode.andThen
            (\string ->
                case string of
                    "LogIn" ->
                        Json.Decode.succeed LogIn

                    "LogOut" ->
                        Json.Decode.succeed LogOut

                    _ ->
                        Json.Decode.fail ("unrecognized string '" ++ string ++ "'")
            )
            Json.Decode.string
        ]
