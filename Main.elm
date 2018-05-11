port module Main exposing (main)

import Common.Table as Table exposing (ColumnStyle(CustomStyle, Width))
import Html exposing (Html, div, h1, input, label, text, a)
import Html.Attributes exposing (class, type_, href)
import Html.Events exposing (onClick, onInput)


port loadData : (( List Row, List CustomerData ) -> msg) -> Sub msg


port openItem : String -> Cmd msg


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
    }


type alias Model =
    { rows : List Row
    , clients : List CustomerData
    , tableState : Table.State
    , filterStr : String
    , showInactive : Bool
    }


view : Model -> Html Msg
view model =
    let
        filteredRows =
            List.filter
                (\t ->
                    if model.showInactive then
                        True
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
            , openItem str
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
    customer.first_name ++ " " ++ customer.last_name ++ "<br />(" ++ "<a href=\"javascript:void(0)\" onclick=\"openItem('contact','" ++ customer.code ++ "')\">" ++ customer.code ++ "</a>" ++ ")"


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
contentHelper t =
    let
        contentKey =
            Maybe.withDefault "" t.contentKey
    in
        a
            [ href "javascript:void(0)"
            , onClick (OpenItem contentKey)
            ]
            [ text contentKey ]


gridConfig : Model -> Table.Config Row Msg
gridConfig model =
    { domTableId = "AccountEntitlementsTable"
    , toolbar =
        [ div [ class "detailsEntitlementToolbarElementLeft" ]
            [ input [ type_ "checkbox", onClick ToggleShowInactive ] []
            , label [] [ text "Show Inactive Content" ]
            ]
        , div [ class "detailsEntitlementToolbarElementLeft" ]
            [ label [] [ text "Contact Search " ]
            , input [ type_ "text", onInput UpdateFilter ] []
            ]
        ]
    , toMsg = SetTableState
    , columns =
        [ Table.htmlColumn "" contentHelper (CustomStyle [ ( "width", "1%" ), ( "border-right", "1px solid black" ) ]) (\t -> Maybe.withDefault "" t.contentKey)
        ]
            ++ List.map
                (\customer ->
                    Table.stringColumn (formatCustomerData customer) (\t -> Just (rowHelper customer.code t)) (CustomStyle [ ( "width", "1%" ), ( "text-align", "center" ) ])
                )
                (filterColumns model model.clients)
    , toRowId = .contentId
    }


init : ( Model, Cmd msg )
init =
    ( { rows = []
      , clients = []
      , tableState = Table.init "Year"
      , filterStr = ""
      , showInactive = False
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
