module Auth exposing
    ( AuthFlow(..)
    , AuthInfo
    , Model
    , Msg
    , defaultLoginModel
    , defaultRegisterModel
    , toLoginModel
    , toRegisterModel
    , tokenRefreshRequest
    , tokenRevokeRequest
    , update
    , view
    )

import Element exposing (Element, fill)
import Element.Input as Input
import ElementFix exposing (text)
import Http
import Json.Decode exposing (Decoder)
import Style exposing (Style)
import Url
import Util exposing (onEnter)


type alias Model =
    { view : AuthView
    , username : String
    , password : String
    , rememberMe : Bool
    }


type AuthView
    = Login
    | Register


defaultRegisterModel : Model
defaultRegisterModel =
    { view = Register, username = "", password = "", rememberMe = False }


defaultLoginModel : Model
defaultLoginModel =
    { view = Login, username = "", password = "", rememberMe = False }


toLoginModel : Model -> Model
toLoginModel model =
    { model | view = Login }


toRegisterModel : Model -> Model
toRegisterModel model =
    { model | view = Register }



-- VIEW


view : Model -> Style -> Element Msg
view model style =
    case model.view of
        Login ->
            Element.column [ Element.width fill, Element.height fill, Style.background style ]
                [ Element.link (Element.alignRight :: Style.button style) { url = "/register", label = text "Create new account" }
                , Element.column [ Element.centerX, Element.centerY, Element.padding 20, Element.spacing 20 ]
                    [ Element.el (Element.centerX :: Style.text style headingSize) <| text "Welcome back!"
                    , Input.username (onEnter Submit :: Style.textInput style)
                        { onChange = \username -> UsernameUpdated username
                        , text = model.username
                        , placeholder = Just <| Input.placeholder [] (text "Username")
                        , label = Input.labelAbove (Style.text style 20) <| text "Username"
                        }
                    , Input.currentPassword (onEnter Submit :: Style.textInput style)
                        { onChange = \pwd -> PasswordUpdated pwd
                        , text = model.password
                        , placeholder = Just <| Input.placeholder [] (text "Password")
                        , label = Input.labelAbove (Style.text style 20) <| text "Password"
                        , show = False
                        }
                    , Input.checkbox []
                        { onChange = SetRememberMe
                        , icon = Input.defaultCheckbox
                        , checked = model.rememberMe
                        , label = Input.labelRight (Style.text style 16) <| text "Remember me (uses cookies)"
                        }
                    , Input.button (Element.centerX :: Style.button style) { onPress = Just Submit, label = text "Login" }
                    ]
                ]

        Register ->
            Element.column [ Element.width fill, Element.height fill, Style.background style ]
                [ Element.link (Element.alignRight :: Style.button style) { url = "/login", label = text "Log in with existing account" }
                , Element.column [ Element.centerX, Element.centerY, Element.padding 20, Element.spacing 20 ]
                    [ Element.el (Element.centerX :: Style.text style headingSize) <| text "Welcome!"
                    , Input.username (onEnter Submit :: Style.textInput style)
                        { onChange = \username -> UsernameUpdated username
                        , text = model.username
                        , placeholder = Just <| Input.placeholder [] (text "Username")
                        , label = Input.labelAbove (Style.text style 20) <| text "Username"
                        }
                    , Input.newPassword (onEnter Submit :: Style.textInput style)
                        { onChange = \pwd -> PasswordUpdated pwd
                        , text = model.password
                        , placeholder = Just <| Input.placeholder [] (text "Password")
                        , label = Input.labelAbove (Style.text style 20) <| text "Password"
                        , show = False
                        }
                    , Input.button (Element.centerX :: Style.button style) { onPress = Just Submit, label = text "Register" }
                    ]
                ]


headingSize : Int
headingSize =
    36



-- UPDATE


type Msg
    = UsernameUpdated String
    | PasswordUpdated String
    | SetRememberMe Bool
    | Submit
    | LoginResponse (Result Http.Error AuthInfo)
    | RegisterResponse (Result Http.Error String)


type alias AuthInfo =
    { authorization : String
    , expiresIn : Int
    }


type AuthFlow
    = Continue ( Model, Cmd Msg )
    | Finish AuthInfo


update : Msg -> Model -> AuthFlow
update msg model =
    case msg of
        UsernameUpdated username ->
            Continue ( { model | username = username }, Cmd.none )

        PasswordUpdated password ->
            Continue ( { model | password = password }, Cmd.none )

        SetRememberMe value ->
            Continue ( { model | rememberMe = value }, Cmd.none )

        Submit ->
            case model.view of
                Login ->
                    Continue
                        ( model, loginRequest model.username model.password model.rememberMe LoginResponse )

                Register ->
                    Continue
                        ( model, registerRequest model.username model.password RegisterResponse )

        LoginResponse response ->
            case response of
                Err _ ->
                    Debug.todo ""

                Ok authInfo ->
                    Finish authInfo

        RegisterResponse response ->
            case response of
                Err _ ->
                    Debug.todo ""

                Ok _ ->
                    Debug.todo ""


loginRequest : String -> String -> Bool -> (Result Http.Error AuthInfo -> msg) -> Cmd msg
loginRequest username password rememberMe msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/api/v1/auth/login"
        , body =
            formBody
                [ ( "login", username )
                , ( "password", password )
                , ( "remember_me"
                  , if rememberMe then
                        "true"

                    else
                        "false"
                  )
                ]
        , expect = Http.expectJson msg authDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


registerRequest : String -> String -> (Result Http.Error String -> msg) -> Cmd msg
registerRequest username password msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/api/v1/users/register"
        , body =
            formBody
                [ ( "login", username )
                , ( "password", password )
                ]
        , expect = Http.expectString msg
        , timeout = Nothing
        , tracker = Nothing
        }


tokenRefreshRequest : (Result Http.Error AuthInfo -> msg) -> Cmd msg
tokenRefreshRequest msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/api/v1/auth/token"
        , body = Http.emptyBody
        , expect = Http.expectJson msg authDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


tokenRevokeRequest : (Result Http.Error () -> msg) -> Cmd msg
tokenRevokeRequest msg =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/api/v1/auth/revoke"
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }



-- API


urlencodeKeyValue : ( String, String ) -> String
urlencodeKeyValue ( key, value ) =
    Url.percentEncode key ++ "=" ++ Url.percentEncode value


formBody : List ( String, String ) -> Http.Body
formBody =
    List.map urlencodeKeyValue
        >> String.join "&"
        >> Http.stringBody "application/x-www-form-urlencoded"


authDecoder : Decoder AuthInfo
authDecoder =
    Json.Decode.map3 authResponseToAuthInfo
        (Json.Decode.field "jwt" Json.Decode.string)
        (Json.Decode.field "token_type" Json.Decode.string)
        (Json.Decode.field "expires_in" Json.Decode.int)


authResponseToAuthInfo : String -> String -> Int -> AuthInfo
authResponseToAuthInfo jwt tokenType expiresIn =
    { authorization = tokenType ++ " " ++ jwt
    , expiresIn = expiresIn
    }
