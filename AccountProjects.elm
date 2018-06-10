port module AccountProjects exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Functions as Functions
import Common.Table as Table exposing (ColumnStyle(..))
import Common.Types exposing (..)
import Date exposing (..)
import Html exposing (Html, a, br, button, div, h1, input, label, text)
import Html.Attributes exposing (class, href, type_)
import Html.Events exposing (onClick, onInput)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { rows : List Projects
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


searchHelper : Model -> Projects -> Bool
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


contactHelper : Projects -> String
contactHelper projects =
    projects.first_name ++ " " ++ projects.last_name


columns : Model -> List (Table.Column Projects Msg)
columns model =
    [ --codeColumn,
      Table.intColumn "#" .proj_num NoStyle
    , Table.intColumn "Start Date" .start_date NoStyle
    , Table.intColumn "Completion Date" (\t -> Maybe.withDefault 0 t.completion_date) NoStyle
    , Table.stringColumn "Contact Name" contactHelper NoStyle
    , Table.stringColumn "Project Description" .proj_desc NoStyle
    ]


gridConfig : Model -> Table.Config Projects Msg
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
    , toRowId = .id
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( emptyModel flags []
    , Cmd.none
    )


emptyModel : Flags -> List Projects -> Model
emptyModel flags rows =
    { rows = rows
    , tableState = Table.init "Year" flags.displayLength
    , filterStr = ""
    , showInactive = True
    }
