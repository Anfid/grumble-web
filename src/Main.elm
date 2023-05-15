module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Element exposing (Element, fill)
import Element.Input as Input
import ElementFix exposing (text)
import Http exposing (emptyBody)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)
import Util exposing (onEnter)



-- MAIN


main : Program Dimensions Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        }



-- MODEL


init : Dimensions -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init dimensions url key =
    gotoUrl url <| Model key dimensions url (Login { username = "", password = "" })


type alias Model =
    { key : Navigation.Key
    , dimensions : Dimensions
    , url : Url
    , page : Page
    }


type alias FormData =
    { username : String
    , password : String
    }


type Page
    = Register FormData
    | Login FormData
    | Messages
    | NotFound


type alias Dimensions =
    { width : Int
    , height : Int
    }


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | Resize Int Int
    | FormMsg FormMsg
    | LoginResponse (Result Http.Error String)
    | RegisterResponse (Result Http.Error String)


type FormMsg
    = Submit
    | UsernameUpdated String
    | PasswordUpdated String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( Login data, FormMsg formMsg ) ->
            case formMsg of
                UsernameUpdated username ->
                    ( { model | page = Login { data | username = username } }, Cmd.none )

                PasswordUpdated password ->
                    ( { model | page = Login { data | password = password } }, Cmd.none )

                Submit ->
                    ( model, Http.post { url = "http://localhost:8080/api/login", body = emptyBody, expect = Http.expectString LoginResponse } )

        ( Register data, FormMsg formMsg ) ->
            case formMsg of
                UsernameUpdated username ->
                    ( { model | page = Register { data | username = username } }, Cmd.none )

                PasswordUpdated password ->
                    ( { model | page = Register { data | password = password } }, Cmd.none )

                Submit ->
                    ( model, Http.post { url = "http://localhost:8080/api/register", body = emptyBody, expect = Http.expectString RegisterResponse } )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Chat"
    , body = [ Element.layout [ Element.width fill ] (body model) ]
    }


body : Model -> Element Msg
body model =
    case model.page of
        Register _ ->
            text "Welcome!"

        Login form ->
            Element.column []
                [ text "Welcome back!"
                , Input.username [ onEnter <| FormMsg Submit ]
                    { onChange = \username -> FormMsg <| UsernameUpdated username
                    , text = form.username
                    , placeholder = Just <| Input.placeholder [] (text "Username")
                    , label = Input.labelAbove [] <| text "Username"
                    }
                , Input.currentPassword [ onEnter <| FormMsg Submit ]
                    { onChange = \pwd -> FormMsg <| PasswordUpdated pwd
                    , text = form.password
                    , placeholder = Just <| Input.placeholder [] (text "Password")
                    , label = Input.labelAbove [] <| text "Password"
                    , show = False
                    }
                , Input.button [] { onPress = Just (FormMsg Submit), label = text "Login" }
                ]

        Messages ->
            text "todo: chat list"

        NotFound ->
            text "Now, how did we get here?"



-- ROUTER


type Route
    = HomeRoute
    | LoginRoute
    | RegisterRoute


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map LoginRoute <| Parser.s "login"
        , Parser.map RegisterRoute <| Parser.s "register"
        ]


gotoUrl : Url -> Model -> ( Model, Cmd Msg )
gotoUrl url model =
    case Parser.parse parser url of
        Just HomeRoute ->
            ( { model | page = Login { username = "", password = "" } }, Cmd.none )

        Just LoginRoute ->
            ( { model | page = Login { username = "", password = "" } }, Cmd.none )

        Just RegisterRoute ->
            ( { model | page = Register { username = "", password = "" } }, Cmd.none )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
