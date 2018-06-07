port module Main exposing (..)

import AccountContacts exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountContents exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountEntitlements exposing (Model, emptyModel, init, subscriptions, update, view)
import Common.Types exposing (Flags)
import Html exposing (Html, a, br, button, div, h1, input, label, text)
import Html.Events exposing (onClick)


port openPage : (String -> msg) -> Sub msg


port loadDummyData : String -> Cmd msg


type alias Model =
    { page : Page
    , flags : Flags
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { page = NotLoaded
      , flags = flags
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ openPage OpenPage
        , pageSubscriptions model.page
        ]


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        NotLoaded ->
            Sub.none

        AccountContacts subModel ->
            Sub.map AccountContactsMsg (AccountContacts.subscriptions subModel)

        AccountEntitlements subModel ->
            Sub.map AccountEntitlementsMsg (AccountEntitlements.subscriptions subModel)

        AccountContents subModel ->
            Sub.map AccountContentsMsg (AccountContents.subscriptions subModel)

        Error _ ->
            Sub.none


type Page
    = NotLoaded
    | AccountContacts AccountContacts.Model
    | AccountEntitlements AccountEntitlements.Model
    | AccountContents AccountContents.Model
    | Error String


getPage : Flags -> String -> Page
getPage flags pageStr =
    case pageStr of
        "AccountContacts" ->
            AccountContacts (AccountContacts.emptyModel flags)

        "AccountEntitlements" ->
            AccountEntitlements (AccountEntitlements.emptyModel flags)

        "AccountContents" ->
            AccountContents (AccountContents.emptyModel flags)

        _ ->
            Error "Unknown Page"


view : Model -> Html Msg
view model =
    case model.page of
        NotLoaded ->
            div []
                [ label [] [ text "No Page Loaded yet" ]
                , div []
                    [ button [ onClick (OpenPage "AccountContacts") ] [ text "Open AccountContacts" ]
                    ]
                , div []
                    [ button [ onClick (OpenPage "AccountEntitlements") ] [ text "Open AccountEntitlements" ]
                    ]
                , div []
                    [ button [ onClick (OpenPage "AccountContents") ] [ text "Open AccountContents" ]
                    ]
                ]

        AccountContacts subModel ->
            Html.map AccountContactsMsg (AccountContacts.view subModel)

        AccountEntitlements subModel ->
            Html.map AccountEntitlementsMsg (AccountEntitlements.view subModel)

        AccountContents subModel ->
            Html.map AccountContentsMsg (AccountContents.view subModel)

        Error errorStr ->
            text errorStr


type Msg
    = OpenPage String
    | AccountContactsMsg AccountContacts.Msg
    | AccountEntitlementsMsg AccountEntitlements.Msg
    | AccountContentsMsg AccountContents.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.page msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            { model | page = toModel newModel } ! [ Cmd.map toMsg newCmd ]
    in
    case ( msg, page ) of
        ( OpenPage pageStr, _ ) ->
            ( { model | page = getPage model.flags pageStr }
            , loadDummyData pageStr
            )

        ( AccountContactsMsg subMsg, AccountContacts subModel ) ->
            toPage AccountContacts AccountContactsMsg AccountContacts.update subMsg subModel

        ( AccountEntitlementsMsg subMsg, AccountEntitlements subModel ) ->
            toPage AccountEntitlements AccountEntitlementsMsg AccountEntitlements.update subMsg subModel

        ( AccountContentsMsg subMsg, AccountContents subModel ) ->
            toPage AccountContents AccountContentsMsg AccountContents.update subMsg subModel

        _ ->
            ( model, Cmd.none )


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
