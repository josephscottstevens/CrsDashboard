module AccountDetails exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Functions as Functions
import Common.Types exposing (AccountDetails, Flags)
import Html exposing (Html, a, br, button, div, h1, input, label, span, table, tbody, td, text, tr)
import Html.Attributes exposing (class, href, id, style, type_)
import Html.Events exposing (onClick, onInput)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { accountDetails : AccountDetails
    }


tdStyle : Html.Attribute msg
tdStyle =
    style [ ( "font-weight", "bold" ), ( "font-size", "22px" ), ( "padding-bottom", "1%" ), ( "color", "#565353" ) ]


view : Model -> Html Msg
view model =
    table [ class "display datatableHighlight", id "accountDetailsTable" ]
        [ tbody []
            [ tr []
                [ td [ tdStyle ]
                    [ text "Ashburton Jersey, Ltd."
                    , span [ style [ ( "padding-left", "1%" ), ( "font-weight", "normal" ), ( "font-size", "12pt" ), ( "color", "black" ) ] ]
                        [ text "("
                        , span [ style [ ( "font-weight", "bold" ), ( "color", "green" ), ( "font-size", "13pt" ) ] ]
                            [ text "Active"
                            ]
                        , text ")"
                        ]
                    ]
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


init : AccountDetails -> ( Model, Cmd Msg )
init accountDetails =
    ( emptyModel accountDetails
    , Cmd.none
    )


emptyModel : AccountDetails -> Model
emptyModel accountDetails =
    { accountDetails = accountDetails
    }
