module Common.Types exposing (..)


type alias Flags =
    { displayLength : String
    , showExportBtnToggle : Bool
    , allTheData : AllTheData
    }


type alias AllTheData =
    { accountDetails : AccountDetails
    , accountContactsRows : List AccountContactsRow
    , accountContentsRows : List AccountContentsRow
    , customerDatas : List CustomerData
    , accountEntitlementsRow : List AccountEntitlementsRow
    , accountProjectsRows : List AccountProjectsRow
    }


type alias AccountDetails =
    { accountId : Int
    , status : String
    , salesRep : String
    , type_ : String
    , subscription : String
    , vote : String
    , totalPayments : Int
    , activeProductsPurchased : List String
    }


type alias AccountContactsRow =
    { contentId : Int
    , contentKey : Maybe String
    , contact : Maybe String
    , account : Maybe String
    , activeStatus : Bool
    , contactStatus : Maybe String
    , contentTypeId : Int
    }


type alias AccountContentsRow =
    { contentId : Int
    , contentKey : Maybe String
    , title : Maybe String
    , format : Maybe String
    , schedule : Maybe String
    , activeStatus : Bool
    , contentTypeId : Int
    }


type alias CustomerData =
    { code : String
    , first_name : String
    , last_name : String
    , client_active : Bool
    }


type alias AccountEntitlementsRow =
    { contentId : Int
    , contentKey : Maybe String
    , customerCode : List String
    , contentActive : String
    , relationshipType : List String
    , methodDesc : List String
    , contentTypeId : Int
    }


type alias AccountProjectsRow =
    { projectId : Int
    , startDate : Maybe String
    , completionDate : Maybe String
    , contactName : Maybe String
    , projectDescription : Maybe String
    }
