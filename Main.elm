port module Main exposing (main)

import Array exposing (Array)
import Common.Functions as Functions
import Common.Table as Table exposing (ColumnStyle(CustomStyle, Width))
import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)


port loadData : (List JsonData -> msg) -> Sub msg


cust_codes : List String
cust_codes =
    [ "ABJ010", "DJA414", "GXD831", "ABJ002", "RANDO" ]


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.batch
        []


type alias JsonData =
    { content_type : String
    , content_id : Int
    , content_key : String
    , cust_code : String
    }


type alias Row =
    { content_id : Int
    , items : Array Bool
    }


type alias Model =
    { rows : List Row
    , tableState : Table.State
    }


view : Model -> Html Msg
view model =
    div []
        [ Table.view model.tableState model.rows gridConfig Nothing
        ]


type Msg
    = SetTableState Table.State
    | LoadData (List JsonData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        LoadData data ->
            ( { model | rows = getRows data }
            , Cmd.none
            )


boolHelper : Int -> Row -> Maybe String
boolHelper idx row =
    case Array.get idx row.items of
        Just t ->
            if t == True then
                Just "X"
            else
                Just ""

        Nothing ->
            Nothing


gridConfig : Table.Config Row Msg
gridConfig =
    { domTableId = "PresidentsTable"
    , toolbar = []
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Content ID" (\t -> Just (toString t.content_id)) (Width 1)
        ]
            ++ List.indexedMap
                (\idx custCode ->
                    Table.stringColumn custCode (\t -> boolHelper idx t) (Width 1)
                )
                cust_codes
    , toRowId = .content_id
    }


init : ( Model, Cmd msg )
init =
    ( emptyModel, Cmd.none )


getRows : List JsonData -> List Row
getRows items =
    items
        |> Functions.groupBy .content_id
        |> List.map mapper


mapper : ( Int, List JsonData ) -> Row
mapper ( content_id, items ) =
    let
        custCodeItems =
            List.map .cust_code items
    in
        Row content_id (Array.fromList (List.map (\t -> List.member t custCodeItems) cust_codes))


emptyModel : Model
emptyModel =
    { rows = []
    , tableState = Table.init "Year"
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
