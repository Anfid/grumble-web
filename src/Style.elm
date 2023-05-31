module Style exposing (..)

import Element exposing (Color, rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


type alias Style =
    { colors : Colors
    , scaling : Float
    }


default : Style
default =
    { colors = defaultColors
    , scaling = 1
    }


type alias Colors =
    { background : Color
    , foreground : Color
    , button : InteractiveColors
    }


type alias InteractiveColors =
    { normal : Color
    , hover : Color
    , focused : Color
    , down : Color
    }


defaultColors : Colors
defaultColors =
    { background = rgb255 40 40 40
    , foreground = rgb255 251 241 199
    , button =
        { normal = rgb255 76 175 80
        , hover = rgb255 92 207 96
        , focused = rgb255 92 207 96
        , down = rgb255 60 159 64
        }
    }


background : Style -> Element.Attribute msg
background style =
    Background.color style.colors.background


text : Style -> Int -> List (Element.Attribute msg)
text style size =
    [ Font.color style.colors.foreground
    , Font.family [ Font.typeface "Georgia", Font.serif ]
    , Font.size <| round <| toFloat size * style.scaling
    ]


button : Style -> List (Element.Attribute msg)
button style =
    [ Element.padding <| round <| toFloat 7 * style.scaling
    , Border.rounded <| round <| toFloat 5 * style.scaling
    , Background.color style.colors.button.normal
    , Font.color style.colors.foreground
    , Font.family [ Font.typeface "Georgia", Font.serif ]
    , Element.mouseDown [ Background.color style.colors.button.down ]
    , Element.mouseOver [ Background.color style.colors.button.hover ]
    , Element.focused [ Background.color style.colors.button.focused ]
    ]


textInput : Style -> List (Element.Attribute msg)
textInput style =
    [ Element.padding <| round <| toFloat 7 * style.scaling
    , Border.rounded <| round <| toFloat 5 * style.scaling
    , Background.color style.colors.foreground
    , Font.color style.colors.background
    , Font.family [ Font.typeface "Georgia", Font.serif ]
    ]
