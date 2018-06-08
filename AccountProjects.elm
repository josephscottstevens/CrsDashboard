port module AccountProjects exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Functions as Functions
import Common.Table as Table exposing (ColumnStyle(..))
import Common.Types exposing (AccountProjectsRow, Flags)
import Date exposing (..)
import Html exposing (Html, a, br, button, div, h1, input, label, text)
import Html.Attributes exposing (class, href, type_)
import Html.Events exposing (onClick, onInput)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { rows : List AccountProjectsRow
    , tableState : Table.State
    , filterStr : String
    , showInactive : Bool
    }



-- inactiveHelper : Model -> Row -> Bool
-- inactiveHelper model row =
--     if model.showInactive then
--         row.activeStatus
--     else
--         True


searchHelper : Model -> AccountProjectsRow -> Bool
searchHelper model row =
    let
        searchText =
            String.toLower model.filterStr

        rowText =
            String.toLower (toString row)
    in
    String.contains searchText rowText


view : Model -> Html Msg
view model =
    let
        filteredRows =
            model.rows
                --|> List.filter (inactiveHelper model)
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


columns : Model -> List (Table.Column AccountProjectsRow Msg)
columns model =
    [ --codeColumn,
      Table.intColumn "#" .projectId NoStyle
    , Table.stringColumn "Start Date" .startDate NoStyle
    , Table.stringColumn "Completion Date" .completionDate NoStyle
    , Table.stringColumn "Contact Name" .contactName NoStyle
    , Table.stringColumn "Project Description" .projectDescription NoStyle
    ]


gridConfig : Model -> Table.Config AccountProjectsRow Msg
gridConfig model =
    { domTableId = "AccountProjectsTable"
    , toolbar =
        -- [ div [ class "detailsEntitlementToolbarElementLeft" ]
        --     [ input [ type_ "checkbox", onClick ToggleShowInactive ] []
        --     , label [] [ text "Show Inactive Projects" ]
        --     ] ,
        [ div [ class "detailsEntitlementToolbarElementLeft" ]
            [ label [] [ text "Project Search " ]
            , input [ type_ "text", onInput UpdateFilter ] []
            ]
        ]
    , toMsg = SetTableState
    , columns = columns model
    , toRowId = .projectId
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( emptyModel flags []
    , Cmd.none
    )


emptyModel : Flags -> List AccountProjectsRow -> Model
emptyModel flags rows =
    { rows = rows
    , tableState = Table.init "Year" flags.displayLength
    , filterStr = ""
    , showInactive = True
    }
