module Models exposing (..)

import Json.Decode exposing (..)


type alias User =
  { id : Int
  , phone : String
  , name : String
  , description : String
  , authToken : String
  , avatarId : Maybe Int
  , isModerator : Bool
  }

type alias UserInfo =
  { userId : Int
  , isFriend : Bool
  , friendRequestSent: Bool
  , friendRequestReceived: Bool
  , isBlacklisted: Bool
  , youAreBlacklisted: Bool
  }

emptyUser : User
emptyUser = User 0 "" "" "" "" Nothing False

userDecoder : Json.Decode.Decoder User
userDecoder =
  Json.Decode.map7 User
    (field "id" int)
    (field "phone" string)
    (oneOf[field "name" string, succeed ""])
    (field "description" string)
    (field "authToken" string)
    (oneOf[field "avatarid" (nullable int), succeed Nothing])
    (field "ismoderator" bool)

userInfoDecoder : Json.Decode.Decoder UserInfo
userInfoDecoder =
   Json.Decode.map6 UserInfo
     (field "userId" int)
     (field "isFriend" bool)
     (field "friendRequestSent" bool)
     (field "friendRequestReceived" bool)
     (field "isBlacklisted" bool)
     (field "youAreBlacklisted" bool)

userWithInfoDecoder : Json.Decode.Decoder (User, UserInfo)
userWithInfoDecoder =
   Json.Decode.map2 (\x y -> (x,y))
     (field "user" userDecoder)
     (field "userInfo" userInfoDecoder)
