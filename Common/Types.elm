module Common.Types exposing (..)

import Common.Functions exposing (decodeMaybeInt, decodeString, decodeYnBool)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


type alias Flags =
    { displayLength : String
    , showExportBtnToggle : Bool
    , header : String
    , token : String
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
    , activeProductsPurchased : String
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
    , contentKey : String
    , title : String
    , customerCode : List String
    , defaultFormat : String
    , methodDesc : List String
    , relationshipType : List String
    , schedule : String
    , contentActive : Bool
    , contentTypeId : Int
    }


type alias Projects =
    { id : Int
    , proj_num : Int
    , start_date : Int
    , completion_date : Maybe Int
    , first_name : String
    , last_name : String
    , proj_desc : String
    }


decodeCompanyListItem : Decode.Decoder Company
decodeCompanyListItem =
    Pipeline.decode Company
        |> Pipeline.required "company" decodeString
        |> Pipeline.required "company_id" Decode.int
        |> Pipeline.required "status" decodeString
        |> Pipeline.required "salesperson" decodeString
        |> Pipeline.required "Type" decodeString
        |> Pipeline.required "Customer_Class__c" decodeString
        |> Pipeline.required "Vote_Schedule__c" decodeString
        |> Pipeline.required "Annual_Billed__C" Decode.int
        |> Pipeline.required "Active_Licensed_Products__c" decodeString


decodeCompanyList : Decode.Decoder (List Company)
decodeCompanyList =
    Decode.list decodeCompanyListItem


decodeCompany : Decode.Decoder (List Company)
decodeCompany =
    Decode.field "company" decodeCompanyList


decodeClientsListItem : Decode.Decoder Clients
decodeClientsListItem =
    Pipeline.decode Clients
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "code" decodeString
        |> Pipeline.required "first_name" decodeString
        |> Pipeline.required "last_name" decodeString
        |> Pipeline.required "company" decodeString
        |> Pipeline.required "client_active" Decode.bool
        |> Pipeline.required "status" decodeString


decodeClientsList : Decode.Decoder (List Clients)
decodeClientsList =
    Decode.list decodeClientsListItem


decodeClients : Decode.Decoder (List Clients)
decodeClients =
    Decode.field "clients" decodeClientsList


decodeContentsListItem : Decode.Decoder Contents
decodeContentsListItem =
    Pipeline.decode Contents
        |> Pipeline.required "contentId" Decode.int
        |> Pipeline.required "contentKey" decodeString
        |> Pipeline.required "title" decodeString
        |> Pipeline.required "customerCode" (Decode.list decodeString)
        |> Pipeline.required "defaultFormat" decodeString
        |> Pipeline.required "methodDesc" (Decode.list decodeString)
        |> Pipeline.required "relationshipType" (Decode.list decodeString)
        |> Pipeline.required "schedule" decodeString
        |> Pipeline.required "contentActive" decodeYnBool
        |> Pipeline.required "contentTypeId" Decode.int


decodeContentsList : Decode.Decoder (List Contents)
decodeContentsList =
    Decode.list decodeContentsListItem


decodeContents : Decode.Decoder (List Contents)
decodeContents =
    Decode.field "contents" decodeContentsList


decodeProjectsListItem : Decode.Decoder Projects
decodeProjectsListItem =
    Pipeline.decode Projects
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "proj_num" Decode.int
        |> Pipeline.required "start_date" Decode.int
        |> Pipeline.required "completion_date" decodeMaybeInt
        |> Pipeline.required "first_name" decodeString
        |> Pipeline.required "last_name" decodeString
        |> Pipeline.required "proj_desc" decodeString


decodeProjectsList : Decode.Decoder (List Projects)
decodeProjectsList =
    Decode.list decodeProjectsListItem


decodeProjects : Decode.Decoder (List Projects)
decodeProjects =
    Decode.field "projects" decodeProjectsList
