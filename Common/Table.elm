module Common.Table
    exposing
        ( Column
        , ColumnStyle(..)
        , Config
        , State
        , checkColumn
        , dateColumn
        , dateTimeColumn
        , hrefColumn
        , htmlColumn
        , init
        , intColumn
        , stringColumn
        , view
        )

import Common.Functions as Functions
import Html exposing (Html, a, div, input, label, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, classList, colspan, disabled, href, id, rowspan, style, target, type_, value)
import Html.Events as Events
import Json.Encode as Encode
import Json.Decode as Decode


-- Data Types


blockSize : Int
blockSize =
    15


init : String -> State
init sortedColumnName =
    { selectedId = Nothing
    , openDropdownId = Nothing
    , pageIndex = 0
    , rowsPerPage = Exactly 50
    , sortField = sortedColumnName
    , sortAscending = True
    }


type RowsPerPage
    = Exactly Int
    | All


type alias State =
    { selectedId : Maybe Int
    , openDropdownId : Maybe Int
    , pageIndex : Int
    , rowsPerPage : RowsPerPage
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
    , viewData : data -> Html msg
    , columnStyle : ColumnStyle
    , sorter : Sorter data
    }


intColumn : String -> (data -> Maybe Int) -> ColumnStyle -> Column data msg
intColumn name data columnStyle =
    { name = name
    , viewData = data >> (\t -> text (Functions.defaultIntToString t))
    , columnStyle = columnStyle
    , sorter = intSort data
    }


stringColumn : String -> (data -> Maybe String) -> ColumnStyle -> Column data msg
stringColumn name data columnStyle =
    { name = name
    , viewData = data >> (\t -> text (Maybe.withDefault "" t))
    , columnStyle = columnStyle
    , sorter = defaultSort data
    }


dateColumn : String -> (data -> Maybe String) -> ColumnStyle -> Column data msg
dateColumn name data columnStyle =
    { name = name
    , viewData = data >> (\t -> text (Functions.defaultDate t))
    , columnStyle = columnStyle
    , sorter = defaultSort data
    }


dateTimeColumn : String -> (data -> Maybe String) -> ColumnStyle -> Column data msg
dateTimeColumn name data columnStyle =
    { name = name
    , viewData = data >> (\t -> text (Functions.defaultDateTime t))
    , columnStyle = columnStyle
    , sorter = defaultSort data
    }


hrefColumn : String -> (data -> ( Maybe String, String )) -> ColumnStyle -> (data -> comparable) -> Column data msg
hrefColumn name data columnStyle toComparable =
    { name = name
    , viewData = data >> viewHrefColumn
    , columnStyle = columnStyle
    , sorter = IncOrDec (List.sortBy toComparable)
    }


viewHrefColumn : ( Maybe String, String ) -> Html msg
viewHrefColumn ( urlData, textData ) =
    a [ href (Functions.defaultString urlData), target "_blank" ]
        [ text textData ]


checkColumn : String -> (data -> Bool) -> ColumnStyle -> Column data msg
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


htmlColumn : String -> (data -> Html msg) -> ColumnStyle -> (data -> comparable) -> Column data msg
htmlColumn name data columnStyle toComparable =
    { name = name
    , viewData = data
    , columnStyle = columnStyle
    , sorter = IncOrDec (List.sortBy toComparable)
    }


type alias Config data msg =
    { domTableId : String
    , toolbar : List (Html msg)
    , toMsg : State -> msg
    , columns : List (Column data msg)
    , toRowId : data -> Int
    }


type Sorter data
    = None
    | IncOrDec (List data -> List data)



-- VIEW


innerHtml : String -> Html.Attribute msg
innerHtml t =
    Encode.string t
        |> Html.Attributes.property "innerHTML"


cellspacing : String -> Html.Attribute msg
cellspacing t =
    attribute "cellspacing" t


cellpadding : String -> Html.Attribute msg
cellpadding t =
    attribute "cellspacing" t


border : String -> Html.Attribute msg
border t =
    attribute "border" t


pageSelect : String -> RowsPerPage
pageSelect str =
    case Functions.maybeStringToInt str of
        Just t ->
            if t > 0 then
                Exactly t
            else
                All

        Nothing ->
            All


view : State -> List data -> Config data msg -> Html msg
view state rows config =
    let
        sortedRows =
            sort state config.columns rows

        filteredRows =
            case state.rowsPerPage of
                Exactly rowsPerPage ->
                    sortedRows
                        |> List.drop (state.pageIndex * rowsPerPage)
                        |> List.take rowsPerPage

                All ->
                    sortedRows

        totalRows =
            List.length rows
    in
        div [ id "searchResultsTable_wrapper" ]
            [ div [ class "top" ]
                [ div [ class "detailsEntitlementToolbarElement", id "searchResultsTable_length" ]
                    ([ label []
                        [ text "Show "
                        , select [ id "pageLengthSelect", Events.onInput (\t -> config.toMsg { state | rowsPerPage = pageSelect t }) ]
                            [ option [ value "50" ] [ text "50" ]
                            , option [ value "100" ] [ text "100" ]
                            , option [ value "150" ] [ text "150" ]
                            , option [ value "200" ] [ text "200" ]
                            , option [ value "-1" ] [ text "All" ]
                            ]
                        ]
                     ]
                        ++ config.toolbar
                    )

                --, pagingView state totalRows filteredRows config.toMsg
                ]
            , table
                [ cellspacing "0"
                , cellpadding "0"
                , border "0"
                , class "display dataTable no-footer"
                , id config.domTableId
                , style [ ( "width", "100%" ) ]
                ]
                [ thead []
                    [ tr [] (List.map (viewTh state config) config.columns)
                    ]
                , tbody []
                    (viewTr state filteredRows config)
                ]
            , div [ class "bottom" ]
                [ div [ class "dataTables_info", id "searchResultsTable_info" ] [ text (pagerText state totalRows) ]
                , pagingView state totalRows filteredRows config.toMsg
                ]
            , div [ class "clear" ] []
            ]


viewTr : State -> List data -> Config data msg -> List (Html msg)
viewTr state rows config =
    let
        selectedStyle row =
            style
                (if Just (config.toRowId row) == state.selectedId then
                    [ ( "background-color", "#66aaff" )
                    , ( "background", "#66aaff" )
                    ]
                 else
                    [ ( "", "" ) ]
                )

        rowClass ctr =
            classList
                [ ( "even", ctr % 2 == 0 )
                , ( "odd", ctr % 2 == 1 )
                ]

        standardTr ctr row =
            tr
                [ rowClass ctr
                , selectedStyle row
                , classList
                    [ ( "selected", Just (config.toRowId row) == state.selectedId )
                    ]
                ]
                (List.map (viewTd state row config) config.columns)
    in
        if List.length rows == 0 then
            [ tr []
                [ td [] [ text "No records to display" ]
                ]
            ]
        else
            List.indexedMap standardTr rows


columnStyle : { data | columnStyle : ColumnStyle } -> Html.Attribute msg
columnStyle column =
    case column.columnStyle of
        NoStyle ->
            style []

        Width t ->
            style [ ( "width", toString t ++ "%" ) ]

        CustomStyle t ->
            style t


viewTh : State -> Config data msg -> Column data msg -> Html msg
viewTh state config column =
    let
        thClass =
            if state.sortField == column.name then
                if not state.sortAscending then
                    class "left sorting_desc"
                else
                    class "left sorting_asc"
            else
                class "left sorting"

        sortClick =
            Events.onClick (config.toMsg { state | sortAscending = not state.sortAscending, sortField = column.name })
    in
        th
            [ thClass
            , sortClick
            , columnStyle column
            , rowspan 1
            , colspan 1
            , innerHtml column.name
            ]
            []


viewTd : State -> data -> Config data msg -> Column data msg -> Html msg
viewTd state row config column =
    td
        [ Events.onClick (config.toMsg { state | selectedId = Just (config.toRowId row) })
        , class "left"
        , columnStyle column
        ]
        [ column.viewData row ]



-- paging


getLastIndex : Int -> RowsPerPage -> Int
getLastIndex totalRows rowsPerPage =
    case rowsPerPage of
        Exactly t ->
            totalRows // t

        All ->
            if totalRows - 1 <= 0 then
                0
            else
                totalRows - 1


setPagingState : State -> Int -> (State -> msg) -> Page -> Html.Attribute msg
setPagingState state totalRows toMsg page =
    let
        lastIndex =
            getLastIndex totalRows state.rowsPerPage

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


pagerText : State -> Int -> String
pagerText state totalRows =
    let
        lastIndex =
            getLastIndex totalRows state.rowsPerPage

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

        totalPageItems t =
            if (state.pageIndex + 1) * t > totalRows then
                totalRows
            else
                (state.pageIndex + 1) * t
    in
        case state.rowsPerPage of
            Exactly t ->
                "Showing " ++ toString (state.pageIndex * t + 1) ++ " to " ++ toString (totalPageItems t) ++ " of " ++ totalItemsText ++ " entries"

            All ->
                "Showing " ++ currentPageText ++ " to " ++ totalItemsText ++ " of " ++ totalPagesText ++ " entries"


pagingView : State -> Int -> List data -> (State -> msg) -> Html msg
pagingView state totalRows rows toMsg =
    let
        lastIndex =
            case state.rowsPerPage of
                Exactly t ->
                    totalRows // t

                All ->
                    0

        pagingStateClick page =
            setPagingState state totalRows toMsg page

        activeOrNot pageIndex =
            let
                activeOrNotText =
                    if pageIndex == state.pageIndex then
                        "paginate_button current"
                    else
                        "paginate_button"
            in
                a
                    [ class activeOrNotText
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
                "paginate_button first"
            else
                "paginate_button first disabled"

        leftPageClass =
            if state.pageIndex > 0 then
                "paginate_button previous"
            else
                "paginate_button previous disabled"

        rightPageClass =
            if state.pageIndex < lastIndex then
                "paginate_button next"
            else
                "paginate_button next disabled"

        lastPageClass =
            if state.pageIndex < lastIndex then
                "paginate_button last"
            else
                "paginate_button last disabled"
    in
        div [ class "dataTables_paginate paging_full_numbers" ]
            [ a [ class firstPageClass, pagingStateClick First ] [ text "First" ]
            , a [ class leftPageClass, pagingStateClick Previous ] [ text "Previous" ]
            , span [] rng
            , a [ class rightPageClass, pagingStateClick Next ] [ text "Next" ]
            , a [ class lastPageClass, pagingStateClick Last ] [ text "Last" ]
            ]



-- Sorting


sort : State -> List (Column data msg) -> List data -> List data
sort state columnData data =
    case findSorter state.sortField columnData of
        Nothing ->
            data

        Just sorter ->
            applySorter state.sortAscending sorter data


applySorter : Bool -> Sorter data -> List data -> List data
applySorter isReversed sorter data =
    case sorter of
        None ->
            data

        IncOrDec sort ->
            if isReversed then
                List.reverse (sort data)
            else
                sort data


findSorter : String -> List (Column data msg) -> Maybe (Sorter data)
findSorter selectedColumn columnData =
    case columnData of
        [] ->
            Nothing

        { name, sorter } :: remainingColumnData ->
            if name == selectedColumn then
                Just sorter
            else
                findSorter selectedColumn remainingColumnData


increasingOrDecreasingBy : (data -> comparable) -> Sorter data
increasingOrDecreasingBy toComparable =
    IncOrDec (List.sortBy toComparable)


defaultSort : (data -> Maybe String) -> Sorter data
defaultSort t =
    increasingOrDecreasingBy (Functions.defaultString << t)


intSort : (data -> Maybe Int) -> Sorter data
intSort t =
    increasingOrDecreasingBy (Functions.defaultIntToString << t)


defaultBoolSort : (data -> Bool) -> Sorter data
defaultBoolSort t =
    increasingOrDecreasingBy (toString << t)
