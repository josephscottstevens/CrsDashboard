port module AccountContents exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

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


inactiveHelper : Model -> Contents -> Bool
inactiveHelper model row =
    if model.showInactive then
        row.contentActive
    else
        True


searchHelper : Model -> Contents -> Bool
searchHelper model row =
    let
        searchText =
            String.toLower model.filterStr

        rowText =
            String.toLower (toString row)
    in
        String.contains searchText rowText


view : Model -> List Contents -> Html Msg
view model rows =
    let
        filteredRows =
            rows
                |> List.filter (inactiveHelper model)
                |> List.filter (searchHelper model)
    in
        div []
            [ Table.view model.tableState filteredRows (gridConfig model)
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


codeHelper : Contents -> Html Msg
codeHelper row =
    if row.contentTypeId /= 11 then
        a
            [ href "javascript:void(0)"
            , onClick (OpenItem row.contentKey)
            ]
            [ text row.contentKey ]
    else
        text row.contentKey


statusHelper : Contents -> String
statusHelper row =
    if row.contentActive then
        "Active"
    else
        "Inactive"


columns : Model -> List (Table.Column Contents Msg)
columns model =
    [ Table.stringColumn "Content Key" .contentKey NoStyle
    , Table.stringColumn "Title" .title NoStyle
    , Table.stringColumn "Format" .defaultFormat NoStyle
    , Table.stringColumn "Schedule" .schedule NoStyle
    , Table.stringColumn "Active Status" statusHelper NoStyle
    ]


gridConfig : Model -> Table.Config Contents Msg
gridConfig model =
    { domTableId = "AccountContentsTable"
    , toolbar =
        [ div [ class "detailsEntitlementToolbarElementLeft" ]
            [ input [ type_ "checkbox", onClick ToggleShowInactive ] []
            , label [] [ text "Show Inactive Contents" ]
            ]
        , div [ class "detailsEntitlementToolbarElementLeft" ]
            [ label [] [ text "Content Search " ]
            , input [ type_ "text", onInput UpdateFilter ] []
            ]
        ]
    , toMsg = SetTableState
    , columns = columns model
    , toRowId = .contentId
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
