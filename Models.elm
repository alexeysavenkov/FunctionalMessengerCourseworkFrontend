module Models exposing (..)

import Json.Decode exposing (..)


type alias User =
  { id : Int
  , phone : String
  , name : String
  , description : String
  , authToken : String
  , avatarId : Maybe Int
  }

emptyUser : User
emptyUser = User 0 "" "" "" "" Nothing

userDecoder : Json.Decode.Decoder User
userDecoder =
  Json.Decode.map6 User
    (field "id" int)
    (field "phone" string)
    (oneOf[field "name" string, succeed ""])
    (field "description" string)
    (field "authToken" string)
    (oneOf[field "avatarId" (nullable int), succeed Nothing])
