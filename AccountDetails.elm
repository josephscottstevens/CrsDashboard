module AccountDetails exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Functions as Functions
import Common.Html exposing (border, cellpadding, cellspacing, role)
import Common.Types exposing (..)
import Html exposing (Html, a, br, button, div, h1, input, label, span, table, tbody, td, text, tr)
import Html.Attributes exposing (class, href, id, style, type_)
import Html.Events exposing (onClick, onInput)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { str : String
    }


tdStyle : Html.Attribute msg
tdStyle =
    style [ ( "font-weight", "bold" ), ( "font-size", "13px" ), ( "padding-bottom", "0.5%" ), ( "color", "#565353" ) ]


spanStyle : Html.Attribute msg
spanStyle =
    style [ ( "font-weight", "normal" ), ( "font-size", "12px" ), ( "color", "black" ) ]


trRow : String -> String -> Html msg
trRow labelText valueText =
    tr []
        [ td [ tdStyle ]
            [ text (labelText ++ ":")
            , span [ spanStyle ]
                [ text valueText ]
            ]
        ]


isAccountActive : Bool -> Html msg
isAccountActive active =
    if active then
        span [ style [ ( "font-weight", "bold" ), ( "color", "green" ), ( "font-size", "13px" ) ] ]
            [ text "Active"
            ]
    else
        span [ style [ ( "font-weight", "bold" ), ( "color", "red" ), ( "font-size", "13px" ) ] ]
            [ text "Inactive"
            ]


view : Model -> Company -> Html Msg
view model accountDetails =
    table
        [ cellpadding "0"
        , cellspacing "0"
        , border "0"
        , class "display datatableHighlight"
        , id "accountDetailsTable"
        ]
        [ tbody []
            [ tr []
                [ td [ style [ ( "font-weight", "bold" ), ( "font-size", "22px" ), ( "padding-bottom", "1%" ), ( "color", "#565353" ) ] ]
                    [ text accountDetails.company
                    , span [ style [ ( "padding-left", "1%" ), ( "font-weight", "normal" ), ( "font-size", "12px" ), ( "color", "black" ) ] ]
                        [ text "( "
                        , isAccountActive accountDetails.active
                        , text " )"
                        ]
                    ]
                ]
            , trRow "Account Id" (toString accountDetails.company_id)
            , trRow "Status" accountDetails.status
            , trRow "Sales Rep" accountDetails.salesperson
            , trRow "Type" accountDetails.type_
            , trRow "Subscription" accountDetails.subscription
            , trRow "Vote" accountDetails.vote
            , trRow "Total Payment" ("$" ++ toString accountDetails.totalPayments)
            , tr []
                [ td [ tdStyle ] [ text "Active Products Purchased" ]
                ]
            , tr []
                [ td [ tdStyle ] [ text accountDetails.activeProductsPurchased ]
                ]
            ]
        ]


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )


init : ( Model, Cmd Msg )
init =
    ( emptyModel
    , Cmd.none
    )


emptyModel : Model
emptyModel =
    { str = ""
    }
