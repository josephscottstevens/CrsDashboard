port module AccountContacts exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Functions as Functions
import Common.Table as Table exposing (ColumnStyle(..))
import Common.Types exposing (..)
import Html exposing (Html, a, br, button, div, h1, input, label, text)
import Html.Attributes exposing (class, href, type_)
import Html.Events exposing (onClick, onInput)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { tableState : Table.State
    , filterStr : String
    , showInactive : Bool
    }


inactiveHelper : Model -> Clients -> Bool
inactiveHelper model row =
    if model.showInactive then
        row.client_active
    else
        True


searchHelper : Model -> Clients -> Bool
searchHelper model row =
    let
        searchText =
            String.toLower model.filterStr

        rowText =
            String.toLower (toString row)
    in
        String.contains searchText rowText


view : Model -> List Clients -> Html Msg
view model rows =
    let
        filteredClientss =
            rows
                |> List.filter (inactiveHelper model)
                |> List.filter (searchHelper model)
    in
        div []
            [ Table.view model.tableState filteredClientss (gridConfig model)
            ]


type Msg
    = SetTableState Table.State
    | UpdateFilter String
    | ToggleShowInactive
    | OpenItem String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        UpdateFilter str ->
            ( { model | filterStr = str }
            , Cmd.none
            )

        ToggleShowInactive ->
            ( { model | showInactive = not model.showInactive }
            , Cmd.none
            )

        OpenItem str ->
            ( model
            , Functions.openItem str
            )


statusHelper : Clients -> String
statusHelper row =
    if row.client_active then
        "Active"
    else
        "Inactive"


columns : Model -> List (Table.Column Clients Msg)
columns model =
    [ Table.stringColumn "Code" .code NoStyle
    , Table.stringColumn "Contact" (\t -> t.first_name ++ " " ++ t.last_name) NoStyle
    , Table.stringColumn "Account" .company NoStyle
    , Table.stringColumn "Active Status" statusHelper NoStyle
    , Table.stringColumn "Contact Status" .status NoStyle
    ]


gridConfig : Model -> Table.Config Clients Msg
gridConfig model =
    { domTableId = "AccountEntitlementsTable"
    , toolbar =
        -- First div should have class .detailsEntitlementToolbar
        [ div [ class "detailsEntitlementToolbar" ]
            [ input [ type_ "checkbox", onClick ToggleShowInactive ] []
            , label [] [ text "Show Inactive Contacts" ]
            ]
        , div [ class "detailsEntitlementToolbarElementLeft" ]
            [ label [] [ text "Contact Search " ]
            , input [ type_ "text", onInput UpdateFilter ] []
            ]
        ]
    , toMsg = SetTableState
    , columns = columns model
    , toRowId = .id
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( emptyModel flags
    , Cmd.none
    )


emptyModel : Flags -> Model
emptyModel flags =
    { tableState = Table.init "Year" flags.displayLength
    , filterStr = ""
    , showInactive = True
    }
