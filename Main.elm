port module Main exposing (main)

import Common.Table as Table exposing (ColumnStyle(CustomStyle, Width))
import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput, onClick)


port loadData : (( List Row, List CustomerData ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    loadData LoadData


type alias CustomerData =
    { code : String
    , client_active : Bool
    }


type alias Row =
    { contentId : Int
    , customerCodes : List String
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
    div []
        [ Table.view model.tableState model.rows (gridConfig model)
        ]


type Msg
    = SetTableState Table.State
    | LoadData ( List Row, List CustomerData )
    | UpdateFilter String
    | ToggleShowInactive


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


rowHelper : String -> Row -> Maybe String
rowHelper custCode row =
    if List.member custCode row.customerCodes then
        Just "X"
    else
        Nothing


gridConfig : Model -> Table.Config Row Msg
gridConfig model =
    let
        filteredColumns =
            if model.showInactive == True then
                List.filter (\t -> String.contains model.filterStr t.code) model.clients
            else
                List.filter (\t -> String.contains model.filterStr t.code && t.client_active == True) model.clients
    in
        { domTableId = "PresidentsTable"
        , toolbar =
            div []
                [ input [ type_ "text", onInput UpdateFilter ] []
                , input [ type_ "checkbox", onClick ToggleShowInactive ] []
                ]
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Content ID" (\t -> Just (toString t.contentId)) (Width 1)
            ]
                ++ List.map
                    (\customer ->
                        Table.stringColumn customer.code (\t -> rowHelper customer.code t) (Width 1)
                    )
                    filteredColumns
        , toRowId = .contentId
        }


init : ( Model, Cmd msg )
init =
    ( { rows = []
      , clients = []
      , tableState = Table.init "Year"
      , filterStr = ""
      , showInactive = True
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
