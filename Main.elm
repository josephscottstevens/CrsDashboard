port module Main exposing (main)

import Common.Functions as Functions
import Common.Table as Table exposing (ColumnStyle(CustomStyle, Width))
import Html exposing (Html, a, br, button, div, h1, input, label, text)
import Html.Attributes exposing (class, href, type_)
import Html.Events exposing (onClick, onInput)


port loadData : (( List Row, List CustomerData ) -> msg) -> Sub msg


port openContactItem : String -> Cmd msg


port excelExport : List (List String) -> Cmd msg


type alias Flags =
    { displayLength : String
    , showExportBtnToggle : Bool
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    loadData LoadData


type alias CustomerData =
    { code : String
    , first_name : String
    , last_name : String
    , client_active : Bool
    }


type alias Row =
    { contentId : Int
    , contentKey : Maybe String
    , customerCode : List String
    , contentActive : String
    , relationshipType : List String
    , methodDesc : List String
    , contentTypeId : Int
    }


type alias Model =
    { rows : List Row
    , clients : List CustomerData
    , tableState : Table.State
    , filterStr : String
    , showInactive : Bool
    , showExportBtnToggle : Bool
    }


view : Model -> Html Msg
view model =
    let
        filteredRows =
            List.filter
                (\t ->
                    if model.showInactive then
                        t.contentActive == "Y"
                    else
                        t.contentActive == "Y"
                )
                model.rows
    in
        div []
            [ Table.view model.tableState filteredRows (gridConfig model)
            ]


type Msg
    = SetTableState Table.State
    | LoadData ( List Row, List CustomerData )
    | UpdateFilter String
    | ToggleShowInactive
    | OpenItem String
    | OpenContactItem String
    | ExcelExport (List (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        LoadData ( rows, clients ) ->
            ( { model | rows = rows, clients = clients }
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

        ExcelExport items ->
            ( model
            , excelExport items
            )


filterColumns : Model -> List CustomerData -> List CustomerData
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


formatCustomerData : CustomerData -> String
formatCustomerData customer =
    customer.first_name ++ " " ++ customer.last_name



--++ "<br />(" ++ "<a href=\"javascript:void(0)\" onclick=\"openItem('contact','" ++ customer.code ++ "')\">" ++ customer.code ++ "</a>" ++ ")"


customerDataToString : CustomerData -> String
customerDataToString customer =
    formatCustomerData customer ++ (" (" ++ customer.code ++ ")")


customerDataToHtml : CustomerData -> Html Msg
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


rowHelper : String -> Row -> String
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


contentHelper : Row -> Html Msg
contentHelper row =
    let
        contentKey =
            Maybe.withDefault "" row.contentKey
    in
        if row.contentTypeId /= 11 then
            a
                [ href "javascript:void(0)"
                , onClick (OpenItem contentKey)
                ]
                [ text contentKey ]
        else
            text contentKey


excelExportData : Model -> List (List String)
excelExportData model =
    [ modelToHeader model ]
        ++ List.map (\row -> rowToList model row) model.rows


modelToHeader : Model -> List String
modelToHeader model =
    [ "" ] ++ (List.map (\client -> customerDataToString client) (List.filter (\t -> t.client_active) model.clients))


rowToList : Model -> Row -> List String
rowToList model row =
    [ Maybe.withDefault "" row.contentKey
    ]
        ++ List.map
            (\customer -> rowHelper customer.code row)
            (filterColumns model model.clients)


columns : Model -> List (Table.Column Row Msg)
columns model =
    [ { header = text ""
      , viewData = contentHelper
      , columnStyle = CustomStyle [ ( "width", "1%" ), ( "border-right", "1px solid black" ) ]
      , sorter = Table.IncOrDec (List.sortBy (\t -> Functions.defaultString t.contentKey))
      , columnId = "content"
      }
    ]
        ++ List.map
            (\customer ->
                { header = customerDataToHtml customer
                , viewData = \t -> text (rowHelper customer.code t)
                , columnStyle = CustomStyle [ ( "width", "1%" ), ( "text-align", "center" ) ]
                , sorter = Table.IncOrDec (List.sortBy (\t -> sortMaybeString (rowHelper customer.code t)))
                , columnId = formatCustomerData customer
                }
            )
            (filterColumns model model.clients)


sortMaybeString : String -> String
sortMaybeString t =
    if t == "" then
        "ZZZZZZZZZZZ"
    else
        t


gridConfig : Model -> Table.Config Row Msg
gridConfig model =
    { domTableId = "AccountEntitlementsTable"
    , toolbar =
        [ --div [ class "detailsEntitlementToolbarElementLeft" ]
          --  [ input [ type_ "checkbox", onClick ToggleShowInactive ] []
          --  , label [] [ text "Show Inactive Content" ]
          --  ]
          div [ class "detailsEntitlementToolbarElementLeft" ]
            [ label [] [ text "Contact Search " ]
            , input [ type_ "text", onInput UpdateFilter ] []
            ]
        , div [ class "detailsEntitlementToolbarElementLeft" ]
            [ label [] [ text "" ]
            , if model.showExportBtnToggle then
                button [ onClick (ExcelExport (excelExportData model)) ] [ text "Export To Excel" ]
              else
                text ""
            ]
        ]
    , toMsg = SetTableState
    , columns = columns model
    , toRowId = .contentId
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { rows = []
      , clients = []
      , tableState = Table.init "Year" flags.displayLength
      , filterStr = ""
      , showInactive = False
      , showExportBtnToggle = flags.showExportBtnToggle
      }
    , Cmd.none
    )


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
