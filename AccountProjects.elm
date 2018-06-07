port module AccountProjects exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Functions as Functions
import Common.Table as Table exposing (ColumnStyle(..))
import Common.Types exposing (Flags)
import Html exposing (Html, a, br, button, div, h1, input, label, text)
import Html.Attributes exposing (class, href, type_)
import Html.Events exposing (onClick, onInput)
import Date exposing (..)


port loadAccountProjectsData : (List Row -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    loadAccountProjectsData LoadAccountProjectsData


type alias Row =
    { projectId : Int
    , startDate : Maybe String
    , completionDate : Maybe String
    , contactName : Maybe String
    , projectDescription : Maybe String
    }


type alias Model =
    { rows : List Row
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


searchHelper : Model -> Row -> Bool
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
    | LoadAccountProjectsData (List Row)


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

        LoadAccountProjectsData rows ->
            ( { model | rows = rows }
            , Cmd.none
            )



-- codeHelper : Row -> Html Msg
-- codeHelper row =
--     let
--         contentKey =
--             Maybe.withDefault "" row.contentKey
--     in
--         if row.contentTypeId /= 11 then
--             a
--                 [ href "javascript:void(0)"
--                 , onClick (OpenItem contentKey)
--                 ]
--                 [ text contentKey ]
--         else
--             text contentKey
-- codeColumn : Table.Column Row Msg
-- codeColumn =
--     { header = text "Code"
--     , viewData = codeHelper
--     , columnStyle = CustomStyle [ ( "width", "1%" ), ( "border-right", "1px solid black" ) ]
--     , sorter = Table.IncOrDec (List.sortBy (\t -> Functions.defaultString t.contentKey))
--     , columnId = "code"
--     }
-- statusHelper : Row -> Maybe String
-- statusHelper row =
--     if row.activeStatus then
--         Just "Active"
--     else
--         Just "Inactive"


columns : Model -> List (Table.Column Row Msg)
columns model =
    [ --codeColumn,
      Table.intColumn "#" .projectId NoStyle
    , Table.stringColumn "Start Date" .startDate NoStyle
    , Table.stringColumn "Completion Date" .completionDate NoStyle
    , Table.stringColumn "Contact Name" .contactName NoStyle
    , Table.stringColumn "Project Description" .projectDescription NoStyle
    ]


gridConfig : Model -> Table.Config Row Msg
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
    ( emptyModel flags
    , Cmd.none
    )


emptyModel : Flags -> Model
emptyModel flags =
    { rows = []
    , tableState = Table.init "Year" flags.displayLength
    , filterStr = ""
    , showInactive = True
    }
