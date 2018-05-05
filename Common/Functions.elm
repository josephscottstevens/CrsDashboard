module Common.Functions exposing (..)

import Date
import Date.Extra


defaultString : Maybe String -> String
defaultString str =
    Maybe.withDefault "" str


defaultLower : Maybe String -> String
defaultLower str =
    String.toLower (defaultString str)


defaultIntToString : Maybe Int -> String
defaultIntToString int =
    case int of
        Just t ->
            toString t

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
