port module Common.Functions exposing (..)

--import Common.Types exposing (Flags)

import Date
import Date.Extra
import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


port openItem : String -> Cmd msg


groupBy : (a -> comparable) -> List a -> List ( comparable, List a )
groupBy fun items =
    Dict.toList (groupByDict fun items)


groupByDict : (a -> comparable) -> List a -> Dict comparable (List a)
groupByDict fun =
    let
        add2Maybe x m =
            case m of
                Nothing ->
                    Just [ x ]

                Just xs ->
                    Just (xs ++ [ x ])

        foldF e =
            Dict.update (fun e) (add2Maybe e)
    in
    List.foldl foldF Dict.empty


defaultString : Maybe String -> String
defaultString str =
    Maybe.withDefault "" str


defaultLower : Maybe String -> String
defaultLower str =
    String.toLower (defaultString str)


maybeIntToString : Maybe Int -> String
maybeIntToString maybeInt =
    case maybeInt of
        Just int ->
            toString int

        Nothing ->
            ""


getMonthIndex : Date.Date -> Int
getMonthIndex dt =
    case Date.month dt of
        Date.Jan ->
            0

        Date.Feb ->
            1

        Date.Mar ->
            2

        Date.Apr ->
            3

        Date.May ->
            4

        Date.Jun ->
            5

        Date.Jul ->
            6

        Date.Aug ->
            7

        Date.Sep ->
            8

        Date.Oct ->
            9

        Date.Nov ->
            10

        Date.Dec ->
            11


formatDateTime : String -> Maybe String -> Maybe String
formatDateTime format maybeStr =
    case maybeStr of
        Just str ->
            case Date.fromString str of
                Ok date ->
                    Just (Date.Extra.toUtcFormattedString format date)

                Err _ ->
                    Nothing

        Nothing ->
            Nothing


dateTimeToString : Date.Date -> String
dateTimeToString date =
    Date.Extra.toUtcFormattedString "MM/dd/yyyy hh:mm:ss a" date


dateToString : Date.Date -> String
dateToString date =
    Date.Extra.toUtcFormattedString "MM/dd/yyyy" date


dateFromString : String -> Maybe Date.Date
dateFromString str =
    case Date.fromString str of
        Ok t ->
            Just t

        Err _ ->
            Nothing


dateTime : String -> String
dateTime str =
    case Date.fromString str of
        Ok date ->
            dateTimeToString date

        Err _ ->
            ""


date : String -> String
date str =
    case Date.fromString str of
        Ok t ->
            dateToString t

        Err _ ->
            ""


defaultDateTime : Maybe String -> String
defaultDateTime str =
    dateTime (defaultString str)


defaultDate : Maybe String -> String
defaultDate str =
    date (defaultString str)


defaultLowerDate : Maybe String -> String
defaultLowerDate str =
    String.toLower (date (defaultString str))


defaultLowerDateTime : Maybe String -> String
defaultLowerDateTime str =
    String.toLower (dateTime (defaultString str))


maybeStringToInt : String -> Maybe Int
maybeStringToInt str =
    case String.toInt str of
        Ok t ->
            Just t

        Err _ ->
            Nothing


decodeMaybeString : Decode.Decoder (Maybe String)
decodeMaybeString =
    Decode.maybe Decode.string


decodeMaybeStringToString : Decode.Decoder (Maybe String) -> Decode.Decoder String
decodeMaybeStringToString =
    Decode.map (Maybe.withDefault "")


decodeString : Decode.Decoder String
decodeString =
    decodeMaybeString |> decodeMaybeStringToString


decodeMaybeInt : Decode.Decoder (Maybe Int)
decodeMaybeInt =
    Decode.maybe Decode.int


decodeYnBoolHelp : String -> Decode.Decoder Bool
decodeYnBoolHelp t =
    if t == "Y" then
        Decode.succeed True
    else if t == "N" then
        Decode.succeed False
    else
        Decode.fail "Invalid bool"


decodeYnBool : Decode.Decoder Bool
decodeYnBool =
    Decode.string
        |> Decode.andThen decodeYnBoolHelp



-- postRequestCustom : Flags -> Encode.Value -> String -> Decode.Decoder a -> Http.Request a
-- postRequestCustom flags body url decoder =
--     Http.request
--         { body = body |> Http.jsonBody
--         , expect = Http.expectJson decoder
--         , headers = [ Http.header flags.header flags.token ]
--         , method = "POST"
--         , timeout = Nothing
--         , url = url
--         , withCredentials = False
--         }
