module Messaging exposing (Model, Msg, defaultModel, update, view)

import Element exposing (Element)
import Style exposing (Style)



-- MODEL


type alias Model =
    {}


defaultModel : Model
defaultModel =
    {}



-- VIEW


view : Model -> Style -> Element Msg
view _ _ =
    Debug.todo ""



-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )
