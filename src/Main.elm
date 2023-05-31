module Main exposing (..)

import Auth
import Browser
import Browser.Navigation as Navigation
import Element exposing (Element, fill)
import ElementFix exposing (text)
import Style exposing (Style)
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
    gotoUrl url <| Model key dimensions url Welcome Style.default


type alias Model =
    { key : Navigation.Key
    , dimensions : Dimensions
    , url : Url
    , page : Page
    , style : Style
    }


type Page
    = Welcome
    | Auth Auth.Model
    | Messages String
    | NotFound


type alias Dimensions =
    { width : Int
    , height : Int
    }


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | Resize Int Int
    | AuthMsg Auth.Msg


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

        ( AuthMsg authMsg, Auth authModel ) ->
            Auth.update authMsg authModel
                |> handleUpdate Auth AuthMsg model

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

        Auth authModel ->
            Element.map AuthMsg <| Auth.view authModel model.style

        Messages message ->
            text <| "Login success: " ++ message

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
subscriptions _ =
    Sub.none
