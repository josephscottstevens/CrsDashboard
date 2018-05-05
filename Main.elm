port module Main exposing (main)

import Common.Table as Table exposing (ColumnStyle(CustomStyle, Width))
import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)


port loadData : (List Person -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loadData LoadData ]


type alias Model =
    { rows : List Person
    , tableState : Table.State
    , query : String
    }


type alias Person =
    { content_id : Int
    , custom : Maybe String
    , active : Maybe String
    , content_key : Maybe String
    , title : Maybe String
    , internal_user : Maybe String
    , priority : Maybe Int
    , market_date : Maybe Int
    , frequency : Maybe String
    , schedule : Maybe String
    , dynamic_product : Maybe String
    , unix_path : Maybe String
    , contact_team : Maybe String
    , contact_team_id : Maybe Int
    , tag : Maybe String
    , contenttype_id : Maybe Int
    , approval_method : Maybe String
    , default_format : Maybe String
    , run_date : Maybe Int
    }


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        rowToString t =
            String.toLower (toString t)

        filteredRows =
            model.rows
                |> List.filter (\t -> String.contains lowerQuery (rowToString t))
    in
        if model.rows == [] then
            text "Loading"
        else
            div []
                [ h1 [] [ text "Results" ]
                , input [ placeholder "Search", onInput SetQuery ] []
                , Table.view model.tableState filteredRows gridConfig Nothing
                ]


type Msg
    = SetQuery String
    | SetTableState Table.State
    | LoadData (List Person)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        LoadData newState ->
            ( { model | rows = newState }
            , Cmd.none
            )


gridConfig : Table.Config Person Msg
gridConfig =
    { domTableId = "ContentResultsTable"
    , toolbar = []
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Content Key" .content_key (Width 1)
        , Table.stringColumn "Title" .title (Width 1)
        , Table.stringColumn "Format" .default_format (Width 1)
        , Table.stringColumn "Active" .active (Width 1)
        ]
    }


init : ( Model, Cmd msg )
init =
    ( emptyModel, Cmd.none )


emptyModel : Model
emptyModel =
    { rows = []
    , tableState = Table.init "Year"
    , query = ""
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
