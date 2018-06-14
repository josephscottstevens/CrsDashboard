port module AccountEntitlements exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Functions as Functions
import Common.Table as Table exposing (ColumnStyle(CustomStyle, Width))
import Common.Types exposing (..)
import Html exposing (Html, a, br, button, div, h1, input, label, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, classList, colspan, disabled, href, id, rowspan, selected, style, target, type_, value)
import Html.Events exposing (onClick, onInput)


port openContactItem : String -> Cmd msg


port excelExport : ( List (List String), String ) -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { tableState : Table.State
    , filterStr : String
    , showInactive : Bool
    , showExportBtnToggle : Bool
    }


view : Model -> List Contents -> List Clients -> Company -> Html Msg
view model rows clients company =
    let
        filteredRows =
            List.filter
                (\t ->
                    if model.showInactive then
                        t.contentActive
                    else
                        t.contentActive
                )
                rows
    in
        div []
            [ Table.view model.tableState filteredRows (gridConfig model rows clients company)
            ]


type Msg
    = SetTableState Table.State
    | UpdateFilter String
    | ToggleShowInactive
    | OpenItem String
    | OpenContactItem String
    | ExcelExport (List (List String)) String


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

        OpenContactItem str ->
            ( model
            , openContactItem str
            )

        ExcelExport items company ->
            ( model
            , excelExport ( items, company )
            )


filterColumns : Model -> List Clients -> List Clients
filterColumns model items =
    let
        filterStr =
            String.toLower model.filterStr

        formatStr t =
            String.toLower (t.code ++ t.first_name ++ " " ++ t.last_name)

        contains t =
            String.contains filterStr (formatStr t)

        filterHelper t =
            contains t && t.client_active == True
    in
        List.filter filterHelper items


formatClients : Clients -> String
formatClients customer =
    customer.first_name ++ " " ++ customer.last_name



--++ "<br />(" ++ "<a href=\"javascript:void(0)\" onclick=\"openItem('contact','" ++ customer.code ++ "')\">" ++ customer.code ++ "</a>" ++ ")"


customerDataToString : Clients -> String
customerDataToString customer =
    formatClients customer ++ (" (" ++ customer.code ++ ")")


customerDataToHtml : Clients -> Html Msg
customerDataToHtml customer =
    div []
        [ text (customer.first_name ++ " " ++ customer.last_name)
        , br [] []
        , a
            [ href "javascript:void(0)"
            , onClick (OpenContactItem customer.code)
            ]
            [ text (" (" ++ customer.code ++ ")") ]
        ]


rowHelper : String -> Contents -> String
rowHelper custCode row =
    if List.member custCode row.customerCode then
        if List.member "crsEntitlementContent" row.relationshipType && List.member "pushPreferenceContent" row.relationshipType then
            if List.length row.methodDesc > 1 then
                "X HYPERLINK"
            else
                case List.head row.methodDesc of
                    Just t ->
                        "X " ++ t

                    Nothing ->
                        Debug.crash "bad data"
        else
            "X"
    else
        ""


contentHelper : Contents -> Html Msg
contentHelper row =
    if row.contentTypeId /= 11 then
        a
            [ href "javascript:void(0)"
            , onClick (OpenItem row.contentKey)
            ]
            [ text row.contentKey ]
    else
        text row.contentKey


excelExportData : Model -> List Contents -> List Clients -> List (List String)
excelExportData model rows clients =
    [ modelToHeader model clients ]
        ++ List.map (\row -> rowToList model row clients) rows


modelToHeader : Model -> List Clients -> List String
modelToHeader model clients =
    [ "" ] ++ List.map (\client -> customerDataToString client) (List.filter (\t -> t.client_active) clients)


rowToList : Model -> Contents -> List Clients -> List String
rowToList model row clients =
    [ row.contentKey
    ]
        ++ List.map
            (\customer -> rowHelper customer.code row)
            (filterColumns model clients)


columns : Model -> List Clients -> List (Table.Column Contents Msg)
columns model clients =
    [ { header = text ""
      , viewData = contentHelper
      , columnStyle = CustomStyle [ ( "width", "1%" ), ( "border-right", "1px solid black" ) ]
      , sorter = Table.IncOrDec (List.sortBy .contentKey)
      , columnId = "content"
      }
    ]
        ++ List.map
            (\customer ->
                { header = customerDataToHtml customer
                , viewData = \t -> text (rowHelper customer.code t)
                , columnStyle = CustomStyle [ ( "width", "1%" ), ( "text-align", "center" ) ]
                , sorter = Table.IncOrDec (List.sortBy (\t -> rowHelper customer.code t))
                , columnId = formatClients customer
                }
            )
            (filterColumns model clients)


sortMaybeString : String -> String
sortMaybeString t =
    if t == "" then
        "ZZZZZZZZZZZ"
    else
        t


gridConfig : Model -> List Contents -> List Clients -> Company -> Table.Config Contents Msg
gridConfig model rows clients company =
    { domTableId = "searchResultsTable"
    , toolbar =
        [ Table.viewPagination model.tableState SetTableState

        --div [ class "detailsEntitlementToolbarElementLeft" ]
        --  [ input [ type_ "checkbox", onClick ToggleShowInactive ] []
        --  , label [] [ text "Show Inactive Content" ]
        --  ]
        , div [ class "detailsEntitlementToolbarElementLeft" ]
            [ label [] [ text "Contact Search " ]
            , input [ type_ "text", onInput UpdateFilter ] []
            ]
        , div [ class "detailsEntitlementToolbarElementLeft" ]
            [ label [] [ text "" ]
            , if model.showExportBtnToggle then
                button [ onClick (ExcelExport (excelExportData model rows clients) company.company) ] [ text "Export To Excel" ]
              else
                text ""
            ]

        --, pagingView state totalRows filteredRows config.toMsg
        ]
    , toMsg = SetTableState
    , columns = columns model clients
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
    , showInactive = False
    , showExportBtnToggle = flags.showExportBtnToggle
    }
