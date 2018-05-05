module Main exposing (main)

import Common.Table as Table
import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)


main =
    Html.program
        { init = init presidents
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { rows : List Person
    , tableState : Table.State
    , query : String
    }


type alias Person =
    { id : Int
    , name : Maybe String
    , year : Int
    , city : Maybe String
    , state : Maybe String
    }


init people =
    let
        model =
            { rows = people
            , tableState = Table.init "Year"
            , query = ""
            }
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = SetQuery String
    | SetTableState Table.State


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



-- VIEW


view : Model -> Html Msg
view model =
    -- let
    --     lowerQuery =
    --         String.toLower query
    --     acceptablePeople =
    --         List.filter (String.contains lowerQuery << String.toLower << .name) people
    -- in
    div []
        [ h1 [] [ text "Birthplaces of U.S. Presidents" ]

        -- , input [ placeholder "Search by Name", onInput SetQuery ] []
        , Table.view model.tableState model.rows gridConfig Nothing
        ]


gridConfig : Table.Config Person Msg
gridConfig =
    { domTableId = "PresidentsTable"
    , toolbar = []
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Name" .name
        , Table.intColumn "Year" .year
        , Table.stringColumn "City" .city
        , Table.stringColumn "State" .state
        ]
    }


presidents : List Person
presidents =
    [ Person 0 (Just "George Washington") 1732 (Just "Westmoreland County") (Just "Virginia")
    , Person 1 (Just "John Adams") 1735 (Just "Braintree") (Just "Massachusetts")
    , Person 2 (Just "Thomas Jefferson") 1743 (Just "Shadwell") (Just "Virginia")
    , Person 3 (Just "James Madison") 1751 (Just "Port Conway") (Just "Virginia")
    , Person 4 (Just "James Monroe") 1758 (Just "Monroe Hall") (Just "Virginia")
    , Person 5 (Just "Andrew Jackson") 1767 (Just "Waxhaws Region") (Just "South/North Carolina")
    , Person 6 (Just "John Quincy Adams") 1767 (Just "Braintree") (Just "Massachusetts")
    , Person 7 (Just "William Henry Harrison") 1773 (Just "Charles City County") (Just "Virginia")
    , Person 8 (Just "Martin Van Buren") 1782 (Just "Kinderhook") (Just "New York")
    , Person 9 (Just "Zachary Taylor") 1784 (Just "Barboursville") (Just "Virginia")
    , Person 10 (Just "John Tyler") 1790 (Just "Charles City County") (Just "Virginia")
    , Person 11 (Just "James Buchanan") 1791 (Just "Cove Gap") (Just "Pennsylvania")
    , Person 12 (Just "James K. Polk") 1795 (Just "Pineville") (Just "North (Just  Carolina")
    , Person 13 (Just "Millard Fillmore") 1800 (Just "Summerhill") (Just "New York")
    , Person 14 (Just "Franklin Pierce") 1804 (Just "Hillsborough") (Just "New Hampshire")
    , Person 15 (Just "Andrew Johnson") 1808 (Just "Raleigh") (Just "North Carolina")
    , Person 16 (Just "Abraham Lincoln") 1809 (Just "Sinking spring") (Just "Kentucky")
    , Person 17 (Just "Ulysses S. Grant") 1822 (Just "Point Pleasant") (Just "Ohio")
    , Person 18 (Just "Rutherford B. Hayes") 1822 (Just "Delaware") (Just "Ohio")
    , Person 19 (Just "Chester A. Arthur") 1829 (Just "Fairfield") (Just "Vermont")
    , Person 20 (Just "James A. Garfield") 1831 (Just "Moreland Hills") (Just "Ohio")
    , Person 21 (Just "Benjamin Harrison") 1833 (Just "North Bend") (Just "Ohio")
    , Person 22 (Just "Grover Cleveland") 1837 (Just "Caldwell") (Just "New Jersey")
    , Person 23 (Just "William McKinley") 1843 (Just "Niles") (Just "Ohio")
    , Person 24 (Just "Woodrow Wilson") 1856 (Just "Staunton") (Just "Virginia")
    , Person 25 (Just "William Howard Taft") 1857 (Just "Cincinnati") (Just "Ohio")
    , Person 26 (Just "Theodore Roosevelt") 1858 (Just "New York City") (Just "New (Just  York")
    , Person 27 (Just "Warren G. Harding") 1865 (Just "Blooming Grove") (Just "Ohio")
    , Person 28 (Just "Calvin Coolidge") 1872 (Just "Plymouth") (Just "Vermont")
    , Person 29 (Just "Herbert Hoover") 1874 (Just "West Branch") (Just "Iowa")
    , Person 30 (Just "Franklin D. Roosevelt") 1882 (Just "Hyde Park") (Just "New York")
    , Person 31 (Just "Harry S. Truman") 1884 (Just "Lamar") (Just "Missouri")
    , Person 32 (Just "Dwight D. Eisenhower") 1890 (Just "Denison") (Just "Texas")
    , Person 33 (Just "Lyndon B. Johnson") 1908 (Just "Stonewall") (Just "Texas")
    , Person 34 (Just "Ronald Reagan") 1911 (Just "Tampico") (Just "Illinois")
    , Person 35 (Just "Richard M. Nixon") 1913 (Just "Yorba Linda") (Just "California")
    , Person 36 (Just "Gerald R. Ford") 1913 (Just "Omaha") (Just "Nebraska")
    , Person 37 (Just "John F. Kennedy") 1917 (Just "Brookline") (Just "Massachusetts")
    , Person 38 (Just "George H. W. Bush") 1924 (Just "Milton") (Just "Massachusetts")
    , Person 39 (Just "Jimmy Carter") 1924 (Just "Plains") (Just "Georgia")
    , Person 40 (Just "George W. Bush") 1946 (Just "New Haven") (Just "Connecticut")
    , Person 41 (Just "Bill Clinton") 1946 (Just "Hope") (Just "Arkansas")
    , Person 42 (Just "Barack Obama") 1961 (Just "Honolulu") (Just "Hawaii")
    , Person 43 (Just "Donald Trump") 1946 (Just "New York City") (Just "New York")
    ]
