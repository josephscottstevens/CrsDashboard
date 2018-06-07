port module Main exposing (..)

import AccountContacts exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountContents exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountEntitlements exposing (Model, emptyModel, init, subscriptions, update, view)
import Common.Html exposing (role)
import Common.Types exposing (Flags)
import Html exposing (Html, a, br, button, div, h1, input, label, li, span, table, tbody, td, text, tr, ul)
import Html.Attributes exposing (class, href, id, style, tabindex)
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
    div [ class "showAccountDetails" ]
        [ table [ id "account-tabs", style [ ( "width", "100%" ) ], class "ui-tabs ui-corner-all ui-widget ui-widget-content" ]
            [ tbody []
                [ div [ id "account-tabs" ]
                    [ ul [ id "detailsTabList", role "tablist", class "ui-tabs-nav ui-corner-all ui-helper-reset ui-helper-clearfix ui-widget-header" ]
                        (List.indexedMap (\idx t -> viewTab (getCurrentTab model.page) idx t) tabs)
                    , div
                        [ id ("#" ++ getTabContentId (getCurrentTab model.page) ++ "-fragment")
                        , style [ ( "font-family", "Arial" ), ( "font-size", "12pt" ), ( "height", "100%" ), ( "overflow", "auto" ) ]
                        , class "ui-tabs-panel ui-corner-bottom ui-widget-content"
                        ]
                        [ viewPage model.page ]
                    ]
                ]
            ]
        ]


viewPage : Page -> Html Msg
viewPage page =
    case page of
        NotLoaded ->
            text ""

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



-- Tab Stuff


getCurrentTab : Page -> Maybe Tab
getCurrentTab page =
    case page of
        NotLoaded ->
            Nothing

        AccountContacts _ ->
            Just accountContactsTab

        AccountEntitlements _ ->
            Just accountEntitlementsTab

        AccountContents _ ->
            Just accountContentsTab

        Error _ ->
            Nothing


getTabContentId : Maybe Tab -> String
getTabContentId maybeTab =
    case maybeTab of
        Just t ->
            t.name

        Nothing ->
            ""


type alias Tab =
    { name : String, displayText : String }


accountDetailsTab : Tab
accountDetailsTab =
    Tab "accountDetails" "Account Details"


accountContactsTab : Tab
accountContactsTab =
    Tab "accountContacts" "Account Contacts"


accountContentsTab : Tab
accountContentsTab =
    Tab "accountContents" "Account Contents"


accountEntitlementsTab : Tab
accountEntitlementsTab =
    Tab "accountEntitlements" "Account Entitlement Matrix"


accountProjectsTab : Tab
accountProjectsTab =
    Tab "accountProjects" "Account Projects"


closeDetailsTab : Tab
closeDetailsTab =
    Tab "closeDetails" "Close Details"


tabs : List Tab
tabs =
    [ accountDetailsTab
    , accountContactsTab
    , accountContentsTab
    , accountEntitlementsTab
    , accountProjectsTab
    , closeDetailsTab
    ]


viewTab : Maybe Tab -> Int -> Tab -> Html Msg
viewTab activeTab idx tab =
    let
        hrefText =
            "#" ++ tab.name ++ "-fragment"

        hrefId =
            "ui-id-" ++ toString (idx + 6)

        isActive =
            ""

        closeDetailsStyle =
            if tab.name == "accountDetails" then
                [ ( "float", "right" ) ]
            else
                []

        idStr =
            if tab.name == "accountDetails" then
                "accountDetailsTab"
            else
                ""

        activeStr =
            if activeTab == Just tab then
                "ui-tabs-active ui-state-active"
            else
                ""
    in
    li [ id idStr, role "tab", tabindex 0, class ("ui-tabs-tab ui-corner-top ui-state-default ui-tab " ++ activeStr), style closeDetailsStyle ]
        [ a [ href hrefText, tabindex -1, role "presentation", class "ui-tabs-anchor", id hrefId, onClick (OpenPage tab.name) ]
            [ span [] [ text tab.displayText ]
            ]
        ]



-- todo
-- onclick="closeDetails()"
