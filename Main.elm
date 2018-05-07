port module Main exposing (main)

import Common.Table as Table exposing (ColumnStyle(CustomStyle, Width))
import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput)


port loadData : (List Row -> msg) -> Sub msg


cust_codes : List String
cust_codes =
    [ "ABJ010", "DJA414", "GXD831", "ABJ002", "RANDO" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    loadData LoadData


type alias Row =
    { content_id : Int
    , cust_codes : List String
    }


type alias Model =
    { rows : List Row
    , tableState : Table.State
    , filterStr : String
    }


view : Model -> Html Msg
view model =
    div []
        [ Table.view model.tableState model.rows (gridConfig model.filterStr) Nothing
        ]


type Msg
    = SetTableState Table.State
    | LoadData (List Row)
    | UpdateFilter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        LoadData rows ->
            ( { model | rows = rows }
            , Cmd.none
            )

        UpdateFilter str ->
            ( { model | filterStr = str }
            , Cmd.none
            )


rowHelper : String -> Row -> Maybe String
rowHelper custCode row =
    if List.member custCode row.cust_codes then
        Just "X"
    else
        Nothing


gridConfig : String -> Table.Config Row Msg
gridConfig filterStr =
    { domTableId = "PresidentsTable"
    , toolbar = input [ type_ "text", onInput UpdateFilter ] []
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Content ID" (\t -> Just (toString t.content_id)) (Width 1)
        ]
            ++ List.map
                (\custCode ->
                    Table.stringColumn custCode (\t -> rowHelper custCode t) (Width 1)
                )
                (List.filter (\t -> String.contains filterStr t) cust_codes)
    , toRowId = .content_id
    }


init : ( Model, Cmd msg )
init =
    ( { rows = [ Row 1 [ "A", "ABJ002", "C" ] ]
      , tableState = Table.init "Year"
      , filterStr = ""
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
