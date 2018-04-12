module GlobalProfile exposing (..)

import Models exposing (..)
import DropZone exposing (DropZoneMessage(Drop), dropZoneEventHandlers, isHovering)
import FileReader exposing (Error(..), FileRef, NativeFile, readAsTextFile)
import Http

type alias ProfileModel =
  { savedUser : User
  , notSavedUser : User
  , dropZone : DropZone.Model
  , userInfo : Maybe UserInfo

    -- store the DropZone model in your apps Model
    , files : List NativeFile
    , contents : List String
  }

profileInitModel : ProfileModel
profileInitModel = ProfileModel emptyUser emptyUser DropZone.init Nothing [] []

type ProfileMsg
  = ChangeProfileInfo User
  | SaveProfileChanges
  | ProfileChangesSaved (Result Http.Error User)
  | DnD (DropZone.DropZoneMessage (List NativeFile))
      -- add an Message that takes care of hovering, dropping etc
  | FileReadSucceeded String
  | FileReadFailed FileReader.Error
  | FriendRequest Int Bool
  | Blacklist Int Bool
  | Report Int String
  | ProfileForceUpdate
  | ProfileForceUpdated (Result Http.Error UserInfo)




