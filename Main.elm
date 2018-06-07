port module Main exposing (..)

import AccountContacts exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountEntitlements exposing (Model, emptyModel, init, subscriptions, update, view)
import Common.Types exposing (Flags)
import Html exposing (Html, a, br, button, div, h1, input, label, text)
import Html.Events exposing (onClick)


port openPage : (String -> msg) -> Sub msg


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
    openPage OpenPage


type Page
    = NotLoaded
    | AccountContacts AccountContacts.Model
    | AccountEntitlements AccountEntitlements.Model
    | Error String


getPage : Flags -> String -> Page
getPage flags pageStr =
    case pageStr of
        "AccountContacts" ->
            AccountContacts (AccountContacts.emptyModel flags)

        "AccountEntitlements" ->
            AccountEntitlements (AccountEntitlements.emptyModel flags)

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
                ]

        AccountContacts subModel ->
            Html.map AccountContactsMsg (AccountContacts.view subModel)

        AccountEntitlements subModel ->
            Html.map AccountEntitlementsMsg (AccountEntitlements.view subModel)

        Error errorStr ->
            text errorStr


type Msg
    = OpenPage String
    | AccountContactsMsg AccountContacts.Msg
    | AccountEntitlementsMsg AccountEntitlements.Msg


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
            , Cmd.none
            )

        ( AccountContactsMsg subMsg, AccountContacts subModel ) ->
            toPage AccountContacts AccountContactsMsg AccountContacts.update subMsg subModel

        ( AccountEntitlementsMsg subMsg, AccountEntitlements subModel ) ->
            toPage AccountEntitlements AccountEntitlementsMsg AccountEntitlements.update subMsg subModel

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
