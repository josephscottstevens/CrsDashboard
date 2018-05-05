module Common.Table
    exposing
        ( Column
        , ColumnStyle(..)
        , Config
        , State
        , checkColumn
        , dateColumn
        , dateTimeColumn
          -- , hrefColumn
          -- , htmlColumn
        , init
        , intColumn
        , stringColumn
        , view
        )

import Common.Functions as Functions
import Html exposing (Html, a, button, div, input, li, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (checked, class, classList, colspan, disabled, href, id, style, target, type_)
import Html.Events as Events
import Json.Encode as Encode


-- Data Types


blockSize : Int
blockSize =
    15


init : String -> State
init sortedColumnName =
    { selectedId = Nothing
    , openDropdownId = Nothing
    , pageIndex = 0
    , rowsPerPage = 20
    , sortField = sortedColumnName
    , sortAscending = True
    }


type alias State =
    { selectedId : Maybe Int
    , openDropdownId : Maybe Int
    , pageIndex : Int
    , rowsPerPage : Int
    , sortField : String
    , sortAscending : Bool
    }


type ColumnStyle
    = NoStyle
    | Width Int
    | CustomStyle (List ( String, String ))


type Page
    = First
    | Previous
    | PreviousBlock
    | Index Int
    | NextBlock
    | Next
    | Last


type alias Column data msg =
    { name : String
    , viewData : { data | id : Int } -> Html msg
    , columnStyle : ColumnStyle
    , sorter : Sorter data
    }


intColumn : String -> ({ data | id : Int } -> Int) -> ColumnStyle -> Column data msg
intColumn name data columnStyle =
    { name = name
    , viewData = data >> (\t -> text (toString t))
    , columnStyle = columnStyle
    , sorter = intSort data
    }


stringColumn : String -> ({ data | id : Int } -> Maybe String) -> ColumnStyle -> Column data msg
stringColumn name data columnStyle =
    { name = name
    , viewData = data >> (\t -> text (Maybe.withDefault "" t))
    , columnStyle = columnStyle
    , sorter = defaultSort data
    }


dateColumn : String -> ({ data | id : Int } -> Maybe String) -> ColumnStyle -> String -> Column data msg
dateColumn name data columnStyle dataField =
    { name = name
    , viewData = data >> (\t -> text (Functions.defaultDate t))
    , columnStyle = columnStyle
    , sorter = defaultSort data
    }


dateTimeColumn : String -> ({ data | id : Int } -> Maybe String) -> ColumnStyle -> String -> Column data msg
dateTimeColumn name data columnStyle dataField =
    { name = name
    , viewData = data >> (\t -> text (Functions.defaultDateTime t))
    , columnStyle = columnStyle
    , sorter = defaultSort data
    }



-- hrefColumn : String -> ({ data | id : Int } -> ( Maybe String, String )) -> ColumnStyle -> Sorter { data | id : Int } -> Column data msg
-- hrefColumn name data columnStyle sorter =
--     { name = name
--     , viewData = data >> viewHrefColumn
--     , columnStyle = columnStyle
--     , sorter = sorter
--     }


viewHrefColumn : ( Maybe String, String ) -> Html msg
viewHrefColumn ( urlData, textData ) =
    a [ href (Functions.defaultString urlData), target "_blank" ]
        [ text textData ]


checkColumn : String -> ({ data | id : Int } -> Bool) -> ColumnStyle -> Column data msg
checkColumn name data columnStyle =
    { name = name
    , viewData = data >> viewCheckColumn
    , columnStyle = columnStyle
    , sorter = defaultBoolSort data
    }


viewCheckColumn : Bool -> Html msg
viewCheckColumn isChecked =
    div [ class "e-checkcell" ]
        [ div [ class "e-checkcelldiv", style [ ( "text-align", "center" ) ] ]
            [ input [ type_ "checkbox", disabled True, checked isChecked ] []
            ]
        ]



-- htmlColumn : String -> ({ data | id : Int } -> Html msg) -> ColumnStyle -> Sorter { data | id : Int } -> Column data msg
-- htmlColumn name data columnStyle sorter =
--     { name = name
--     , viewData = data
--     , columnStyle = columnStyle
--     , sorter = sorter
--     }


type alias Config data msg =
    { domTableId : String
    , toolbar : List ( String, msg )
    , toMsg : State -> msg
    , columns : List (Column { data | id : Int } msg)
    }


type Sorter data
    = None
    | IncOrDec (List { data | id : Int } -> List { data | id : Int })



-- VIEW


view : State -> List { data | id : Int } -> Config { data | id : Int } msg -> Maybe (Html msg) -> Html msg
view state rows config maybeCustomRow =
    let
        sortedRows =
            sort state config.columns rows

        filteredRows =
            sortedRows
                |> List.drop (state.pageIndex * state.rowsPerPage)
                |> List.take state.rowsPerPage

        totalRows =
            List.length rows
    in
    div [ class "e-grid e-js e-waitingpopup" ]
        [ viewToolbar config.toolbar
        , table [ id config.domTableId, class "e-table", style [ ( "border-collapse", "collapse" ) ] ]
            [ thead [ class "e-gridheader e-columnheader e-hidelines" ]
                [ tr [] (List.map (viewTh state config) config.columns)

                -- This is for filters, this can come at a later time
                -- , tr [] (List.map (viewThFilter state config) config.columns)
                ]
            , tbody []
                (viewTr state filteredRows config maybeCustomRow)
            ]
        , pagingView state totalRows filteredRows config.toMsg
        ]


viewTr : State -> List { data | id : Int } -> Config { data | id : Int } msg -> Maybe (Html msg) -> List (Html msg)
viewTr state rows config maybeCustomRow =
    let
        selectedStyle row =
            style
                (if Just row.id == state.selectedId then
                    [ ( "background-color", "#66aaff" )
                    , ( "background", "#66aaff" )
                    ]
                 else
                    [ ( "", "" ) ]
                )

        rowClass ctr =
            classList
                [ ( "e-row", ctr % 2 == 0 )
                , ( "e-alt_row", ctr % 2 == 1 )
                ]

        standardTr ctr row =
            tr
                [ rowClass ctr
                , selectedStyle row
                ]
                (List.map (viewTd state row config) config.columns)

        customRowStyle =
            if List.length rows == 0 then
                style []
            else
                style
                    [ ( "border-bottom-color", "#cecece" )
                    , ( "border-bottom-width", "1px" )
                    , ( "border-bottom-style", "solid" )
                    ]

        customCellStyle =
            style
                [ ( "background-color", "white" )
                , ( "padding-top", "10px" )
                , ( "margin-left", "5px" )
                ]
    in
    case maybeCustomRow of
        Just customRow ->
            tr [ customRowStyle ]
                [ td [ colspan (List.length config.columns), customCellStyle ]
                    [ customRow
                    ]
                ]
                :: List.indexedMap standardTr rows

        Nothing ->
            if List.length rows == 0 then
                [ tr []
                    [ td [] [ text "No records to display" ]
                    ]
                ]
            else
                List.indexedMap standardTr rows


viewTh : State -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
viewTh state config column =
    let
        headerContent =
            if state.sortField == column.name then
                if not state.sortAscending then
                    [ text column.name, span [ class "e-icon e-ascending e-rarrowup-2x" ] [] ]
                else
                    [ text column.name, span [ class "e-icon e-ascending e-rarrowdown-2x" ] [] ]
            else
                [ text column.name ]

        newSortDirection =
            not state.sortAscending

        sortClick =
            Events.onClick (config.toMsg { state | sortAscending = newSortDirection, sortField = column.name })
    in
    th [ class ("e-headercell e-default " ++ column.name), sortClick ]
        [ div [ class "e-headercelldiv e-gridtooltip" ] headerContent
        ]


viewThFilter : State -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
viewThFilter state config column =
    th [ class "e-filterbarcell" ]
        [ div [ class "e-filterdiv e-fltrinputdiv" ]
            [ input [ class "e-ejinputtext e-filtertext" ] []
            , span [ class "e-cancel e-icon" ] []
            ]
        ]


viewTd : State -> { data | id : Int } -> Config { data | id : Int } msg -> Column { data | id : Int } msg -> Html msg
viewTd state row config column =
    let
        tdClass =
            classList
                [ ( "e-gridtooltip", True )
                , ( "e-active", Just row.id == state.selectedId )
                ]

        tdStyle =
            style [ ( "padding-left", "8.4px" ) ]

        tdClick =
            Events.onClick (config.toMsg { state | selectedId = Just row.id })
    in
    td [ tdClass, tdStyle, tdClick ]
        [ column.viewData row ]


textHtml : String -> Html msg
textHtml t =
    div
        [ Encode.string t
            |> Html.Attributes.property "innerHTML"
        ]
        []



-- Custom


rowDropDownDiv : State -> (State -> msg) -> { data | id : Int } -> List ( String, String, { data | id : Int } -> msg ) -> Html msg
rowDropDownDiv state toMsg row dropDownItems =
    let
        dropClickEvent event =
            Events.onClick (event row)

        dropDownMenuItem : ( String, String, { data | id : Int } -> msg ) -> Html msg
        dropDownMenuItem ( iconClass, displayText, event ) =
            li [ class "e-content e-list", dropClickEvent event ]
                [ a [ class "e-menulink", target "_blank" ]
                    [ text displayText
                    , span [ class ("e-gridcontext e-icon " ++ iconClass) ] []
                    ]
                ]

        dropDownMenuStyle : Html.Attribute msg
        dropDownMenuStyle =
            style
                [ ( "z-index", "5000" )
                , ( "position", "absolute" )
                , ( "display", "block" )
                , ( "left", "-173px" )
                , ( "width", "178.74px" )
                ]

        dropMenu =
            case state.openDropdownId of
                Just t ->
                    if row.id == t then
                        [ ul [ class "e-menu e-js e-widget e-box e-separator" ]
                            (List.map dropDownMenuItem dropDownItems)
                        ]
                    else
                        []

                Nothing ->
                    []

        btnClass =
            class "btn btn-sm btn-default fa fa-angle-down btn-context-menu editDropDown"

        btnStyle =
            style [ ( "position", "relative" ) ]

        clickEvent =
            case state.openDropdownId of
                Just _ ->
                    Events.onClick (toMsg { state | openDropdownId = Nothing })

                Nothing ->
                    Events.onClick (toMsg { state | openDropdownId = Just row.id })

        blurEvent =
            Events.onBlur (toMsg { state | openDropdownId = Nothing })
    in
    div []
        [ div [ style [ ( "text-align", "right" ) ] ]
            [ button [ id "contextMenuButton", type_ "button", btnClass, clickEvent, blurEvent, btnStyle ]
                [ div [ id "editButtonMenu", dropDownMenuStyle ]
                    dropMenu
                ]
            ]
        ]


viewToolbar : List ( String, msg ) -> Html msg
viewToolbar items =
    div [ class "e-gridtoolbar e-toolbar e-js e-widget e-box e-toolbarspan e-tooltip" ]
        [ ul [ class "e-ul e-horizontal" ]
            [ li [ class "e-tooltxt" ]
                (List.map toolbarHelper items)
            ]
        ]


toolbarHelper : ( String, msg ) -> Html msg
toolbarHelper ( iconStr, event ) =
    let
        iconStyle =
            if String.contains "e-disable" iconStr then
                style []
            else
                style [ ( "cursor", "pointer" ) ]

        iconClass =
            "e-addnewitem e-toolbaricons e-icon " ++ iconStr
    in
    a [ class iconClass, Events.onClick event, iconStyle, id "btnNewRecord" ] []



-- paging


setPagingState : State -> Int -> (State -> msg) -> Page -> Html.Attribute msg
setPagingState state totalRows toMsg page =
    let
        lastIndex =
            totalRows // state.rowsPerPage

        bounded t =
            if t > lastIndex then
                lastIndex
            else if t < 0 then
                0
            else
                t

        newIndex =
            case page of
                First ->
                    0

                Previous ->
                    bounded (state.pageIndex - 1)

                PreviousBlock ->
                    bounded (state.pageIndex - blockSize)

                Index t ->
                    bounded t

                NextBlock ->
                    bounded (state.pageIndex + blockSize)

                Next ->
                    bounded (state.pageIndex + 1)

                Last ->
                    lastIndex
    in
    Events.onClick (toMsg { state | pageIndex = newIndex })


pagingView : State -> Int -> List { data | id : Int } -> (State -> msg) -> Html msg
pagingView state totalRows rows toMsg =
    let
        lastIndex =
            totalRows // state.rowsPerPage

        pagingStateClick page =
            setPagingState state totalRows toMsg page

        activeOrNot pageIndex =
            let
                activeOrNotText =
                    if pageIndex == state.pageIndex then
                        "e-currentitem e-active"
                    else
                        "e-default"
            in
            div
                [ class ("e-link e-numericitem e-spacing " ++ activeOrNotText)
                , pagingStateClick (Index pageIndex)
                ]
                [ text (toString (pageIndex + 1)) ]

        rng =
            List.range 0 lastIndex
                |> List.drop ((state.pageIndex // blockSize) * blockSize)
                |> List.take blockSize
                |> List.map activeOrNot

        firstPageClass =
            if state.pageIndex > 1 then
                "e-icon e-mediaback e-firstpage e-default"
            else
                "e-icon e-mediaback e-firstpagedisabled e-disable"

        leftPageClass =
            if state.pageIndex > 0 then
                "e-icon e-arrowheadleft-2x e-prevpage e-default"
            else
                "e-icon e-arrowheadleft-2x e-prevpagedisabled e-disable"

        leftPageBlockClass =
            if state.pageIndex >= blockSize then
                "e-link e-spacing e-PP e-numericitem e-default"
            else
                "e-link e-nextprevitemdisabled e-disable e-spacing e-PP"

        rightPageBlockClass =
            if state.pageIndex < lastIndex - blockSize then
                "e-link e-NP e-spacing e-numericitem e-default"
            else
                "e-link e-NP e-spacing e-nextprevitemdisabled e-disable"

        rightPageClass =
            if state.pageIndex < lastIndex then
                "e-nextpage e-icon e-arrowheadright-2x e-default"
            else
                "e-icon e-arrowheadright-2x e-nextpagedisabled e-disable"

        lastPageClass =
            if state.pageIndex < lastIndex then
                "e-lastpage e-icon e-mediaforward e-default"
            else
                "e-icon e-mediaforward e-animate e-lastpagedisabled e-disable"

        pagerText =
            let
                currentPageText =
                    toString (state.pageIndex + 1)

                totalPagesText =
                    toString <|
                        if lastIndex < 1 then
                            1
                        else
                            lastIndex + 1

                totalItemsText =
                    toString totalRows
            in
            currentPageText ++ " of " ++ totalPagesText ++ " pages (" ++ totalItemsText ++ " items)"
    in
    div [ class "e-pager e-js e-pager" ]
        [ div [ class "e-pagercontainer" ]
            [ div [ class firstPageClass, pagingStateClick First ] []
            , div [ class leftPageClass, pagingStateClick Previous ] []
            , a [ class leftPageBlockClass, pagingStateClick PreviousBlock ] [ text "..." ]
            , div [ class "e-numericcontainer e-default" ] rng
            , a [ class rightPageBlockClass, pagingStateClick NextBlock ] [ text "..." ]
            , div [ class rightPageClass, pagingStateClick Next ] []
            , div [ class lastPageClass, pagingStateClick Last ] []
            ]
        , div [ class "e-parentmsgbar", style [ ( "text-align", "right" ) ] ]
            [ span [ class "e-pagermsg" ] [ text pagerText ]
            ]
        ]



-- Sorting


sort : State -> List (Column { data | id : Int } msg) -> List { data | id : Int } -> List { data | id : Int }
sort state columnData data =
    case findSorter state.sortField columnData of
        Nothing ->
            data

        Just sorter ->
            applySorter state.sortAscending sorter data


applySorter : Bool -> Sorter { data | id : Int } -> List { data | id : Int } -> List { data | id : Int }
applySorter isReversed sorter data =
    case sorter of
        None ->
            data

        IncOrDec sort ->
            if isReversed then
                List.reverse (sort data)
            else
                sort data


findSorter : String -> List (Column { data | id : Int } msg) -> Maybe (Sorter { data | id : Int })
findSorter selectedColumn columnData =
    case columnData of
        [] ->
            Nothing

        { name, sorter } :: remainingColumnData ->
            if name == selectedColumn then
                Just sorter
            else
                findSorter selectedColumn remainingColumnData


increasingOrDecreasingBy : ({ data | id : Int } -> comparable) -> Sorter data
increasingOrDecreasingBy toComparable =
    IncOrDec (List.sortBy toComparable)


defaultSort : ({ data | id : Int } -> Maybe String) -> Sorter data
defaultSort t =
    increasingOrDecreasingBy (Functions.defaultString << t)


intSort : ({ data | id : Int } -> Int) -> Sorter data
intSort t =
    increasingOrDecreasingBy (toString << t)


defaultBoolSort : ({ data | id : Int } -> Bool) -> Sorter data
defaultBoolSort t =
    increasingOrDecreasingBy (toString << t)
