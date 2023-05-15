module ElementFix exposing (text)

import Element exposing (Element)
import Html exposing (Html)


text : String -> Element msg
text str =
    Element.html <| Html.text str
