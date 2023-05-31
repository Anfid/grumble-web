module Auth exposing (Model, Msg, defaultLoginModel, defaultRegisterModel, update, view)

import Element exposing (Element)
import Element.Input as Input
import ElementFix exposing (text)
import Http
import Json.Decode exposing (Decoder)
import Url
import Util exposing (onEnter)


type alias Model =
    { view : AuthView
    , username : String
    , password : String
    }


type AuthView
    = Login
    | Register


defaultRegisterModel : Model
defaultRegisterModel =
    { view = Register, username = "", password = "" }


defaultLoginModel : Model
defaultLoginModel =
    { view = Login, username = "", password = "" }



-- VIEW


view : Model -> Element Msg
view model =
    case model.view of
        Login ->
            Element.column []
                [ text "Welcome back!"
                , Input.username [ onEnter Submit ]
                    { onChange = \username -> UsernameUpdated username
                    , text = model.username
                    , placeholder = Just <| Input.placeholder [] (text "Username")
                    , label = Input.labelAbove [] <| text "Username"
                    }
                , Input.currentPassword [ onEnter Submit ]
                    { onChange = \pwd -> PasswordUpdated pwd
                    , text = model.password
                    , placeholder = Just <| Input.placeholder [] (text "Password")
                    , label = Input.labelAbove [] <| text "Password"
                    , show = False
                    }
                , Input.button [] { onPress = Just Submit, label = text "Login" }
                ]

        Register ->
            Element.column []
                [ text "Welcome!"
                , Input.username [ onEnter <| Submit ]
                    { onChange = \username -> UsernameUpdated username
                    , text = model.username
                    , placeholder = Just <| Input.placeholder [] (text "Username")
                    , label = Input.labelAbove [] <| text "Username"
                    }
                , Input.currentPassword [ onEnter Submit ]
                    { onChange = \pwd -> PasswordUpdated pwd
                    , text = model.password
                    , placeholder = Just <| Input.placeholder [] (text "Password")
                    , label = Input.labelAbove [] <| text "Password"
                    , show = False
                    }
                , Input.button [] { onPress = Just Submit, label = text "Register" }
                ]



-- UPDATE


type Msg
    = UsernameUpdated String
    | PasswordUpdated String
    | Submit
    | LoginResponse (Result Http.Error AuthInfo)
    | RegisterResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameUpdated username ->
            ( { model | username = username }, Cmd.none )

        PasswordUpdated password ->
            ( { model | password = password }, Cmd.none )

        Submit ->
            case model.view of
                Login ->
                    ( model
                    , Http.post
                        { url = "http://localhost:8080/api/v1/users/login"
                        , body =
                            formBody
                                [ ( "login", model.username )
                                , ( "password", model.password )
                                ]
                        , expect = Http.expectJson LoginResponse authDecoder
                        }
                    )

                Register ->
                    ( model
                    , Http.post
                        { url = "http://localhost:8080/api/v1/users/register"
                        , body =
                            formBody
                                [ ( "login", model.username )
                                , ( "password", model.password )
                                ]
                        , expect = Http.expectString RegisterResponse
                        }
                    )

        LoginResponse _ ->
            Debug.todo ""

        RegisterResponse _ ->
            Debug.todo ""



-- API


urlencodeKeyValue : ( String, String ) -> String
urlencodeKeyValue ( key, value ) =
    Url.percentEncode key ++ "=" ++ Url.percentEncode value


formBody : List ( String, String ) -> Http.Body
formBody =
    List.map urlencodeKeyValue
        >> String.join "&"
        >> Http.stringBody "application/x-www-form-urlencoded"


type alias AuthInfo =
    { jwt : String
    , token_type : String
    , expires_in : Int
    , refresh_token : String
    }


authDecoder : Decoder AuthInfo
authDecoder =
    Json.Decode.map4 AuthInfo
        (Json.Decode.field "jwt" Json.Decode.string)
        (Json.Decode.field "token_type" Json.Decode.string)
        (Json.Decode.field "expires_in" Json.Decode.int)
        (Json.Decode.field "refresh_token" Json.Decode.string)