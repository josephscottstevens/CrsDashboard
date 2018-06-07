module Common.Html exposing (role)

import Html exposing (Html, a, br, button, div, h1, input, label, li, span, text, ul)
import Html.Attributes exposing (class, href, id, tabindex)
import Json.Encode as Encode


role : String -> Html.Attribute msg
role name =
    Html.Attributes.property "role" (Encode.string name)
