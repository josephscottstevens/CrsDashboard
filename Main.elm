port module Main exposing (..)

import AccountContacts exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountContents exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountDetails exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountEntitlements exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountProjects exposing (Model, emptyModel, init, subscriptions, update, view)
import Common.Html exposing (ariaControls, ariaExpanded, ariaHidden, ariaLabelledby, ariaSelected, role)
import Common.Types exposing (AllTheData, Flags)
import Html exposing (Html, a, br, button, div, h1, input, label, li, span, table, tbody, td, text, tr, ul)
import Html.Attributes exposing (class, href, id, style, tabindex)
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
    Sub.batch
        [ openPage OpenPage
        , pageSubscriptions model.page
        ]


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        NotLoaded ->
            Sub.none

        AccountDetails subModel ->
            Sub.map AccountDetailsMsg (AccountDetails.subscriptions subModel)

        AccountContacts subModel ->
            Sub.map AccountContactsMsg (AccountContacts.subscriptions subModel)

        AccountContents subModel ->
            Sub.map AccountContentsMsg (AccountContents.subscriptions subModel)

        AccountEntitlements subModel ->
            Sub.map AccountEntitlementsMsg (AccountEntitlements.subscriptions subModel)

        AccountProjects subModel ->
            Sub.map AccountProjectsMsg (AccountProjects.subscriptions subModel)

        Error _ ->
            Sub.none


type Page
    = NotLoaded
    | AccountDetails AccountDetails.Model
    | AccountContacts AccountContacts.Model
    | AccountContents AccountContents.Model
    | AccountEntitlements AccountEntitlements.Model
    | AccountProjects AccountProjects.Model
    | Error String


getPage : Flags -> String -> Page
getPage flags pageStr =
    case pageStr of
        "accountDetails" ->
            AccountDetails (AccountDetails.emptyModel flags.allTheData.company)

        "accountContacts" ->
            AccountContacts (AccountContacts.emptyModel flags flags.allTheData.clients)

        "accountContents" ->
            AccountContents (AccountContents.emptyModel flags flags.allTheData.contents)

        "accountEntitlements" ->
            AccountEntitlements (AccountEntitlements.emptyModel flags ( flags.allTheData.contents, flags.allTheData.clients ))

        "accountProjects" ->
            AccountProjects (AccountProjects.emptyModel flags flags.allTheData.projects)

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
                        , style [ ( "font-family", "Arial" ), ( "font-size", "12px" ), ( "height", "100%" ), ( "overflow", "auto" ) ]
                        , class "ui-tabs-panel ui-corner-bottom ui-widget-content"
                        , ariaHidden False
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

        AccountDetails subModel ->
            Html.map AccountDetailsMsg (AccountDetails.view subModel)

        AccountContacts subModel ->
            Html.map AccountContactsMsg (AccountContacts.view subModel)

        AccountContents subModel ->
            Html.map AccountContentsMsg (AccountContents.view subModel)

        AccountEntitlements subModel ->
            Html.map AccountEntitlementsMsg (AccountEntitlements.view subModel)

        AccountProjects subModel ->
            Html.map AccountProjectsMsg (AccountProjects.view subModel)

        Error errorStr ->
            text errorStr


type Msg
    = OpenPage String
    | AccountDetailsMsg AccountDetails.Msg
    | AccountContactsMsg AccountContacts.Msg
    | AccountContentsMsg AccountContents.Msg
    | AccountEntitlementsMsg AccountEntitlements.Msg
    | AccountProjectsMsg AccountProjects.Msg


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

        ( AccountDetailsMsg subMsg, AccountDetails subModel ) ->
            toPage AccountDetails AccountDetailsMsg AccountDetails.update subMsg subModel

        ( AccountContactsMsg subMsg, AccountContacts subModel ) ->
            toPage AccountContacts AccountContactsMsg AccountContacts.update subMsg subModel

        ( AccountContentsMsg subMsg, AccountContents subModel ) ->
            toPage AccountContents AccountContentsMsg AccountContents.update subMsg subModel

        ( AccountEntitlementsMsg subMsg, AccountEntitlements subModel ) ->
            toPage AccountEntitlements AccountEntitlementsMsg AccountEntitlements.update subMsg subModel

        ( AccountProjectsMsg subMsg, AccountProjects subModel ) ->
            toPage AccountProjects AccountProjectsMsg AccountProjects.update subMsg subModel

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

        AccountDetails _ ->
            Just accountDetailsTab

        AccountContacts _ ->
            Just accountContactsTab

        AccountContents _ ->
            Just accountContentsTab

        AccountEntitlements _ ->
            Just accountEntitlementsTab

        AccountProjects _ ->
            Just accountProjectsTab

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
            tab.name ++ "-fragment"

        hrefId =
            "ui-id-" ++ toString (idx + 6)

        closeDetailsStyle =
            if tab.name == "closeDetails" then
                [ ( "float", "right" ) ]
            else
                []

        idStr =
            if tab.name == "accountDetails" then
                "accountDetailsTab"
            else
                ""

        isActive =
            activeTab == Just tab

        activeClass =
            if isActive then
                "ui-tabs-active ui-state-active"
            else
                ""
    in
    li
        [ id idStr
        , role "tab"
        , tabindex 0
        , class ("ui-tabs-tab ui-corner-top ui-state-default ui-tab " ++ activeClass)
        , ariaControls hrefText
        , ariaLabelledby hrefId
        , ariaSelected isActive
        , ariaExpanded isActive
        , style closeDetailsStyle
        ]
        [ a [ href ("#" ++ hrefText), tabindex -1, role "presentation", class "ui-tabs-anchor", id hrefId, onClick (OpenPage tab.name) ]
            [ span [] [ text tab.displayText ]
            ]
        ]



-- todo
-- onclick="closeDetails()"
-- aria tags
