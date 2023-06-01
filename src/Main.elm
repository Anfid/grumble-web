module Main exposing (..)

import Auth
import Browser
import Browser.Events
import Browser.Navigation as Navigation
import Element exposing (Element, fill)
import Element.Input as Input
import ElementFix exposing (text)
import Http
import Messaging
import Style exposing (Style)
import Time
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)



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
    gotoUrl url <| Model key dimensions url Guest Welcome Style.default


type alias Model =
    { key : Navigation.Key
    , dimensions : Dimensions
    , url : Url
    , session : Session
    , page : Page
    , style : Style
    }


type Session
    = Guest
    | User Auth.AuthInfo


type Page
    = Welcome
    | Auth Auth.Model
    | Messaging Messaging.Model
    | NotFound


type alias Dimensions =
    { width : Int
    , height : Int
    }


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | Resize Int Int
    | TokenRefreshTick Time.Posix
    | TokenRefreshResult (Result Http.Error Auth.AuthInfo)
    | Logout
    | LogoutResult (Result Http.Error ())
    | AuthMsg Auth.Msg
    | MessagingMsg Messaging.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlChanged url, _ ) ->
            gotoUrl url model

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( TokenRefreshTick _, _ ) ->
            case model.session of
                User _ ->
                    ( model, Auth.tokenRefreshRequest TokenRefreshResult )

                Guest ->
                    ( model, Cmd.none )

        ( Logout, _ ) ->
            case model.session of
                User _ ->
                    ( { model | session = Guest }, Auth.tokenRevokeRequest LogoutResult )

                Guest ->
                    ( model, Cmd.none )

        ( TokenRefreshResult tokenRefreshResult, _ ) ->
            case tokenRefreshResult of
                Ok authInfo ->
                    ( { model | session = User authInfo }, Cmd.none )

                Err _ ->
                    Debug.todo ""

        ( AuthMsg authMsg, Auth authModel ) ->
            case Auth.update authMsg authModel of
                Auth.Continue state ->
                    handleUpdate Auth AuthMsg model state

                Auth.Finish result ->
                    ( { model | session = User result }, Navigation.pushUrl model.key "/" )

        _ ->
            ( model, Cmd.none )


handleUpdate : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
handleUpdate toPage toMsg model ( subModel, subCmd ) =
    ( { model | page = toPage subModel }
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Grumble"
    , body = [ Element.layout [ Element.width fill ] (body model) ]
    }


body : Model -> Element Msg
body model =
    case model.page of
        Welcome ->
            Element.el [ Element.width fill, Element.height fill, Style.background model.style ] <|
                case model.session of
                    Guest ->
                        Element.wrappedRow
                            [ Element.alignRight, Element.spacing 5 ]
                            [ Element.link (Style.button model.style)
                                { url = "/login"
                                , label = text "Log in"
                                }
                            , Element.link (Style.button model.style)
                                { url = "/register"
                                , label = text "Create an account"
                                }
                            ]

                    User creds ->
                        Element.column
                            [ Element.width fill ]
                            [ Input.button (Element.alignRight :: Style.button model.style)
                                { onPress = Just Logout
                                , label = text "Log out"
                                }
                            , Element.paragraph (Element.width fill :: Style.text model.style 20) <| [ text "Logged in with token ", text creds.authorization ]
                            ]

        Auth authModel ->
            Element.map AuthMsg <| Auth.view authModel model.style

        Messaging messagingModel ->
            Element.map MessagingMsg <| Messaging.view messagingModel model.style

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
            ( { model | page = Welcome }, Cmd.none )

        Just LoginRoute ->
            case model.page of
                Auth authModel ->
                    ( { model | page = Auth <| Auth.toLoginModel authModel }, Cmd.none )

                _ ->
                    ( { model | page = Auth Auth.defaultLoginModel }, Cmd.none )

        Just RegisterRoute ->
            case model.page of
                Auth authModel ->
                    ( { model | page = Auth <| Auth.toRegisterModel authModel }, Cmd.none )

                _ ->
                    ( { model | page = Auth Auth.defaultRegisterModel }, Cmd.none )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        authRefreshTimeSub =
            case model.session of
                Guest ->
                    Sub.none

                User auth ->
                    Time.every (toFloat (1000 * auth.expiresIn)) TokenRefreshTick
    in
    Sub.batch
        [ Browser.Events.onResize Resize
        , authRefreshTimeSub
        ]
