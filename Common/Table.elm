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
    , viewData : { data | content_id : Int } -> Html msg
    , columnStyle : ColumnStyle
    , sorter : Sorter data
    }


intColumn : String -> ({ data | content_id : Int } -> Int) -> ColumnStyle -> Column data msg
intColumn name data columnStyle =
    { name = name
    , viewData = data >> (\t -> text (toString t))
    , columnStyle = columnStyle
    , sorter = intSort data
    }


stringColumn : String -> ({ data | content_id : Int } -> Maybe String) -> ColumnStyle -> Column data msg
stringColumn name data columnStyle =
    { name = name
    , viewData = data >> (\t -> text (Maybe.withDefault "" t))
    , columnStyle = columnStyle
    , sorter = defaultSort data
    }


dateColumn : String -> ({ data | content_id : Int } -> Maybe String) -> ColumnStyle -> Column data msg
dateColumn name data columnStyle =
    { name = name
    , viewData = data >> (\t -> text (Functions.defaultDate t))
    , columnStyle = columnStyle
    , sorter = defaultSort data
    }


dateTimeColumn : String -> ({ data | content_id : Int } -> Maybe String) -> ColumnStyle -> Column data msg
dateTimeColumn name data columnStyle =
    { name = name
    , viewData = data >> (\t -> text (Functions.defaultDateTime t))
    , columnStyle = columnStyle
    , sorter = defaultSort data
    }


hrefColumn : String -> ({ data | content_id : Int } -> ( Maybe String, String )) -> ColumnStyle -> ({ data | content_id : Int } -> comparable) -> Column data msg
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


checkColumn : String -> ({ data | content_id : Int } -> Bool) -> ColumnStyle -> Column data msg
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


htmlColumn : String -> ({ data | content_id : Int } -> Html msg) -> ColumnStyle -> ({ data | content_id : Int } -> comparable) -> Column data msg
htmlColumn name data columnStyle toComparable =
    { name = name
    , viewData = data
    , columnStyle = columnStyle
    , sorter = IncOrDec (List.sortBy toComparable)
    }


type alias Config data msg =
    { domTableId : String
    , toolbar : List ( String, msg )
    , toMsg : State -> msg
    , columns : List (Column { data | content_id : Int } msg)
    }


type Sorter data
    = None
    | IncOrDec (List { data | content_id : Int } -> List { data | content_id : Int })



-- VIEW


cellspacing : String -> Html.Attribute msg
cellspacing t =
    attribute "cellspacing" t


cellpadding : String -> Html.Attribute msg
cellpadding t =
    attribute "cellspacing" t


border : String -> Html.Attribute msg
border t =
    attribute "border" t


view : State -> List { data | content_id : Int } -> Config { data | content_id : Int } msg -> Maybe (Html msg) -> Html msg
view state rows config maybeCustomRow =
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
            [ div [ class "dataTables_length", id "searchResultsTable_length" ]
                [ label []
                    [ text "Show"
                    , select [ id "pageLengthSelect" ]
                        [ option [ value "50", Events.onClick (config.toMsg { state | rowsPerPage = Exactly 50 }) ] [ text "50" ]
                        , option [ value "100", Events.onClick (config.toMsg { state | rowsPerPage = Exactly 100 }) ] [ text "100" ]
                        , option [ value "150", Events.onClick (config.toMsg { state | rowsPerPage = Exactly 150 }) ] [ text "150" ]
                        , option [ value "200", Events.onClick (config.toMsg { state | rowsPerPage = Exactly 200 }) ] [ text "200" ]
                        , option [ value "-1", Events.onClick (config.toMsg { state | rowsPerPage = All }) ] [ text "All" ]
                        ]
                    ]
                ]
            , pagingView state totalRows filteredRows config.toMsg
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
                (viewTr state filteredRows config maybeCustomRow)
            ]
        , div [ class "bottom" ]
            [ div [ class "dataTables_info", id "searchResultsTable_info" ] [ text (pagerText state totalRows) ]
            , pagingView state totalRows filteredRows config.toMsg
            ]
        , div [ class "clear" ] []
        ]


viewTr : State -> List { data | content_id : Int } -> Config { data | content_id : Int } msg -> Maybe (Html msg) -> List (Html msg)
viewTr state rows config maybeCustomRow =
    let
        selectedStyle row =
            style
                (if Just row.content_id == state.selectedId then
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
                    [ ( "selected", Just row.content_id == state.selectedId )
                    ]
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


columnStyle : { data | columnStyle : ColumnStyle } -> Html.Attribute msg
columnStyle column =
    case column.columnStyle of
        NoStyle ->
            style []

        Width t ->
            style [ ( "width", toString t ++ "%" ) ]

        CustomStyle t ->
            style t


viewTh : State -> Config { data | content_id : Int } msg -> Column { data | content_id : Int } msg -> Html msg
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
        ]
        [ text column.name
        ]


viewTd : State -> { data | content_id : Int } -> Config { data | content_id : Int } msg -> Column { data | content_id : Int } msg -> Html msg
viewTd state row config column =
    td
        [ Events.onClick (config.toMsg { state | selectedId = Just row.content_id })
        , class "left"
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
    in
    "Showing " ++ currentPageText ++ " to " ++ totalItemsText ++ " of " ++ totalPagesText ++ " entries"


pagingView : State -> Int -> List { data | content_id : Int } -> (State -> msg) -> Html msg
pagingView state totalRows rows toMsg =
    let
        lastIndex =
            getLastIndex totalRows state.rowsPerPage

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
        [ a [ class firstPageClass, pagingStateClick First ] []
        , a [ class leftPageClass, pagingStateClick Previous ] []
        , span [] rng
        , a [ class rightPageClass, pagingStateClick Next ] []
        , a [ class lastPageClass, pagingStateClick Last ] []
        ]



-- Sorting


sort : State -> List (Column { data | content_id : Int } msg) -> List { data | content_id : Int } -> List { data | content_id : Int }
sort state columnData data =
    case findSorter state.sortField columnData of
        Nothing ->
            data

        Just sorter ->
            applySorter state.sortAscending sorter data


applySorter : Bool -> Sorter { data | content_id : Int } -> List { data | content_id : Int } -> List { data | content_id : Int }
applySorter isReversed sorter data =
    case sorter of
        None ->
            data

        IncOrDec sort ->
            if isReversed then
                List.reverse (sort data)
            else
                sort data


findSorter : String -> List (Column { data | content_id : Int } msg) -> Maybe (Sorter { data | content_id : Int })
findSorter selectedColumn columnData =
    case columnData of
        [] ->
            Nothing

        { name, sorter } :: remainingColumnData ->
            if name == selectedColumn then
                Just sorter
            else
                findSorter selectedColumn remainingColumnData


increasingOrDecreasingBy : ({ data | content_id : Int } -> comparable) -> Sorter data
increasingOrDecreasingBy toComparable =
    IncOrDec (List.sortBy toComparable)


defaultSort : ({ data | content_id : Int } -> Maybe String) -> Sorter data
defaultSort t =
    increasingOrDecreasingBy (Functions.defaultString << t)


intSort : ({ data | content_id : Int } -> Int) -> Sorter data
intSort t =
    increasingOrDecreasingBy (toString << t)


defaultBoolSort : ({ data | content_id : Int } -> Bool) -> Sorter data
defaultBoolSort t =
    increasingOrDecreasingBy (toString << t)
