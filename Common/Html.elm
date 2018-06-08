module Common.Html
    exposing
        ( ariaControls
        , ariaExpanded
        , ariaHidden
        , ariaLabelledby
        , ariaSelected
        , border
        , cellpadding
        , cellspacing
        , role
        )

import Html exposing (Attribute, Html, a, br, button, div, h1, input, label, li, span, text, ul)
import Html.Attributes exposing (class, href, id, property, tabindex)
import Json.Encode as Encode


-- Private


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"
    else
        "false"



-- Public


role : String -> Attribute msg
role name =
    property "role" (Encode.string name)


cellpadding : String -> Attribute msg
cellpadding name =
    property "cellpadding" (Encode.string name)


cellspacing : String -> Attribute msg
cellspacing name =
    property "cellspacing" (Encode.string name)


border : String -> Attribute msg
border name =
    property "border" (Encode.string name)


ariaControls : String -> Attribute msg
ariaControls name =
    property "aria-controls" (Encode.string name)


ariaLabelledby : String -> Attribute msg
ariaLabelledby name =
    property "aria-labelledby" (Encode.string name)


ariaSelected : Bool -> Attribute msg
ariaSelected bool =
    property "aria-selected" (Encode.string (boolToString bool))


ariaExpanded : Bool -> Attribute msg
ariaExpanded bool =
    property "aria-expanded" (Encode.string (boolToString bool))


ariaHidden : Bool -> Attribute msg
ariaHidden bool =
    property "aria-hidden" (Encode.string (boolToString bool))
