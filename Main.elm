port module Main exposing (..)

import AccountContacts exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountContents exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountDetails exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountEntitlements exposing (Model, emptyModel, init, subscriptions, update, view)
import AccountProjects exposing (Model, emptyModel, init, subscriptions, update, view)
import Common.Functions exposing (..)
import Common.Html exposing (ariaControls, ariaExpanded, ariaHidden, ariaLabelledby, ariaSelected, role)
import Common.Types exposing (..)
import Html exposing (Html, a, br, button, div, h1, input, label, li, span, table, tbody, td, text, tr, ul)
import Html.Attributes exposing (class, href, id, style, tabindex)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


port showError : String -> Cmd msg


port search : (Int -> msg) -> Sub msg


type alias Model =
    { page : Page
    , flags : Flags
    , company : Maybe Company
    , clients : Maybe (List Clients)
    , contents : Maybe (List Contents)
    , projects : Maybe (List Projects)
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { page = AccountDetails AccountDetails.emptyModel
      , flags = flags
      , company = Nothing
      , clients = Nothing
      , contents = Nothing
      , projects = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ search Search
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


view : Model -> Html Msg
view model =
    div [ class "showAccountDetails" ]
        [ table [ id "account-tabs", style [ ( "width", "100%" ) ], class "ui-tabs ui-corner-all ui-widget ui-widget-content" ]
            [ tbody []
                [ div [ id "account-tabs" ]
                    [ ul [ id "detailsTabList", role "tablist", class "ui-tabs-nav ui-corner-all ui-helper-reset ui-helper-clearfix ui-widget-header" ]
                        (List.indexedMap (\idx t -> viewTab model.page (getCurrentTab model.page) idx t) tabs)
                    , div
                        [ id ("#" ++ getTabContentId (getCurrentTab model.page) ++ "-fragment")
                        , style [ ( "font-family", "Arial" ), ( "font-size", "12px" ), ( "height", "100%" ), ( "overflow", "auto" ) ]
                        , class "ui-tabs-panel ui-corner-bottom ui-widget-content"
                        , ariaHidden False
                        ]
                        [ viewPage model ]
                    ]
                ]
            ]
        ]


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        NotLoaded ->
            text ""

        AccountDetails subModel ->
            case model.company of
                Just company ->
                    Html.map AccountDetailsMsg (AccountDetails.view subModel company)

                Nothing ->
                    text ""

        AccountContacts subModel ->
            case model.clients of
                Just clients ->
                    Html.map AccountContactsMsg (AccountContacts.view subModel clients)

                Nothing ->
                    text ""

        AccountContents subModel ->
            case model.contents of
                Just contents ->
                    Html.map AccountContentsMsg (AccountContents.view subModel contents)

                Nothing ->
                    text ""

        AccountEntitlements subModel ->
            case ( model.contents, model.clients ) of
                ( Just contents, Just clients ) ->
                    Html.map AccountEntitlementsMsg (AccountEntitlements.view subModel contents clients)

                _ ->
                    text ""

        AccountProjects subModel ->
            case model.projects of
                Just projects ->
                    Html.map AccountProjectsMsg (AccountProjects.view subModel projects)

                Nothing ->
                    text ""

        Error errorStr ->
            text errorStr


type Msg
    = OpenPage Page
    | Search Int
    | LoadCompany (Result Http.Error (List Company))
    | LoadClients (Result Http.Error (List Clients))
    | LoadContents (Result Http.Error (List Contents))
    | LoadProjects (Result Http.Error (List Projects))
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
        postRequest : String -> Decode.Decoder a -> Http.Request a
        postRequest =
            postRequestWithFlags model.flags

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            { model | page = toModel newModel } ! [ Cmd.map toMsg newCmd ]
    in
    case ( msg, page ) of
        ( OpenPage page, _ ) ->
            ( { model | page = page }, Cmd.none )

        ( Search accountId, _ ) ->
            ( model
            , Cmd.batch
                [ Http.send LoadCompany <|
                    postRequest ("lookupAccountDetails.do?accountId=" ++ toString accountId) (Decode.list decodeCompany)
                , Http.send LoadClients <|
                    postRequest ("lookupAccountContacts.do?accountId=" ++ toString accountId) (Decode.list decodeClients)
                , Http.send LoadContents <|
                    postRequest ("lookupAccountContents.do?accountId=" ++ toString accountId) (Decode.list decodeContents)
                , Http.send LoadProjects <|
                    postRequest ("lookupAccountProjects.do?accountId=" ++ toString accountId) (Decode.list decodeProjects)
                ]
            )

        ( LoadCompany response, _ ) ->
            case response of
                Ok companies ->
                    case List.head companies of
                        Just company ->
                            ( { model | company = Just company }, Cmd.none )

                        Nothing ->
                            ( model, showError "There was an error getting details for the company." )

                Err t ->
                    ( model, showError (toString t) )

        ( LoadClients response, _ ) ->
            case response of
                Ok clients ->
                    ( { model | clients = Just clients }, Cmd.none )

                Err t ->
                    ( model, showError (toString t) )

        ( LoadContents response, _ ) ->
            case response of
                Ok contents ->
                    ( { model | contents = Just contents }, Cmd.none )

                Err t ->
                    ( model, showError (toString t) )

        ( LoadProjects response, _ ) ->
            case response of
                Ok projects ->
                    ( { model | projects = Just projects }, Cmd.none )

                Err t ->
                    ( model, showError (toString t) )

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
    { name : String
    , displayText : String
    }


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


viewTab : Page -> Maybe Tab -> Int -> Tab -> Html Msg
viewTab page activeTab idx tab =
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
        [ a [ href ("#" ++ hrefText), tabindex -1, role "presentation", class "ui-tabs-anchor", id hrefId, onClick (OpenPage page) ]
            [ span [] [ text tab.displayText ]
            ]
        ]


postRequestWithFlags : Flags -> String -> Decode.Decoder a -> Http.Request a
postRequestWithFlags flags url decoder =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson decoder
        , headers = [ Http.header flags.header flags.token ]
        , method = "POST"
        , timeout = Nothing
        , url = url
        , withCredentials = False
        }



-- todo
-- onclick="closeDetails()"
-- aria tags
