module Common.Types exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


type alias Flags =
    { displayLength : String
    , showExportBtnToggle : Bool
    , allTheData : AllTheData
    }


type alias AllTheData =
    { company : Decode.Value
    , clients : List Clients
    , contents : List Contents
    , projects : List Projects
    }


type alias Company =
    { company : String
    , company_id : Int
    , status : String
    , salesperson : String
    , type_ : String
    , subscription : String
    , vote : String
    , totalPayments : Int
    , activeProductsPurchased : List String
    }


type alias Clients =
    { id : Int
    , code : String
    , first_name : String
    , last_name : String
    , company : String
    , client_active : Bool
    , status : String
    }


type alias Contents =
    { contentId : Int
    , contentKey : Maybe String
    , title : Maybe String
    , customerCode : List String
    , defaultFormat : Maybe String
    , methodDesc : List String
    , relationshipType : List String
    , schedule : Maybe String
    , contentActive : String
    , contentTypeId : Int
    }


type alias Projects =
    { id : Int
    , proj_num : Int
    , start_date : Int
    , completion_date : Int
    , first_name : Maybe String
    , last_name : Maybe String
    , proj_desc : Maybe String
    }


decodeCompany : Decode.Decoder Company
decodeCompany =
    Pipeline.decode Company
        |> Pipeline.required "company" Decode.string
        |> Pipeline.required "company_id" Decode.int
        |> Pipeline.required "status" Decode.string
        |> Pipeline.required "salesperson" Decode.string
        |> Pipeline.required "Type" Decode.string
        |> Pipeline.required "Customer_Class__c" Decode.string
        |> Pipeline.required "Vote_Schedule__c" Decode.string
        |> Pipeline.required "Annual_Billed__C" Decode.int
        |> Pipeline.required "Active_Licensed_Products__c" (Decode.list Decode.string)
