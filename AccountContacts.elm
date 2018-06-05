port module AccountContacts exposing (main)

import Common.Functions as Functions
import Common.Table as Table exposing (ColumnStyle(..))
import Html exposing (Html, a, br, button, div, h1, input, label, text)
import Html.Attributes exposing (class, href, type_)
import Html.Events exposing (onClick, onInput)


port loadAccountContactsData : (List Row -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    loadAccountContactsData LoadAccountContactsData


type alias Flags =
    { displayLength : String
    }


type alias Row =
    { contentId : Int
    , contentKey : Maybe String
    , contact : Maybe String
    , account : Maybe String
    , activeStatus : Bool
    , contactStatus : Maybe String
    , contentTypeId : Int
    }


type alias Model =
    { rows : List Row
    , tableState : Table.State
    , filterStr : String
    , showInactive : Bool
    }


view : Model -> Html Msg
view model =
    let
        filteredRows =
            List.filter
                (\t -> model.showInactive)
                model.rows
    in
    div []
        [ Table.view model.tableState filteredRows (gridConfig model)
        ]


type Msg
    = SetTableState Table.State
    | UpdateFilter String
    | ToggleShowInactive
    | OpenItem String
    | LoadAccountContactsData (List Row)


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

        LoadAccountContactsData rows ->
            ( { model | rows = rows }
            , Cmd.none
            )


codeHelper : Row -> Html Msg
codeHelper row =
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


codeColumn : Table.Column Row Msg
codeColumn =
    { header = text "Code"
    , viewData = codeHelper
    , columnStyle = CustomStyle [ ( "width", "1%" ), ( "border-right", "1px solid black" ) ]
    , sorter = Table.IncOrDec (List.sortBy (\t -> Functions.defaultString t.contentKey))
    , columnId = "code"
    }


statusHelper : Row -> Maybe String
statusHelper row =
    if row.activeStatus then
        Just "Active"
    else
        Just "Inactive"


columns : Model -> List (Table.Column Row Msg)
columns model =
    [ codeColumn
    , Table.stringColumn "Contact" .contact NoStyle
    , Table.stringColumn "Account" .account NoStyle
    , Table.stringColumn "Active Status" statusHelper NoStyle
    , Table.stringColumn "ContactStatus" .contactStatus NoStyle
    ]


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
        ]
    , toMsg = SetTableState
    , columns = columns model
    , toRowId = .contentId
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { rows = dummyData
      , tableState = Table.init "Year" flags.displayLength
      , filterStr = ""
      , showInactive = True
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


dummyData : List Row
dummyData =
    [ { contentId = 0
      , contentKey = Just "ABJ002"
      , contact = Just "Nick Lee"
      , account = Just "Ashburton Jersey, LTD."
      , activeStatus = True
      , contactStatus = Just "Client"
      , contentTypeId = 10
      }
    , { contentId = 6
      , contentKey = Just "ABJ006"
      , contact = Just "Richard Robinson"
      , account = Just "Ashburton Jersey, LTD."
      , activeStatus = True
      , contactStatus = Just "Client"
      , contentTypeId = 10
      }
    , { contentId = 10
      , contentKey = Just "ABJ010"
      , contact = Just "Luke Gale"
      , account = Just "Ashburton Jersey, LTD."
      , activeStatus = True
      , contactStatus = Just "Client"
      , contentTypeId = 10
      }
    , { contentId = 3
      , contentKey = Just "DJA414"
      , contact = Just "Shane de la Haye"
      , account = Just "Ashburton Jersey, LTD."
      , activeStatus = True
      , contactStatus = Just "Client"
      , contentTypeId = 10
      }
    , { contentId = 4
      , contentKey = Just "GXD831"
      , contact = Just "Craig Farley"
      , account = Just "Ashburton Jersey, LTD."
      , activeStatus = True
      , contactStatus = Just "Client"
      , contentTypeId = 10
      }
    ]
