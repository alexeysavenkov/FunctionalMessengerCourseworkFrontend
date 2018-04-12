module GlobalFriends exposing (..)

import Models exposing (..)
import Http exposing (..)

type alias FriendsModel =
  { friends: Maybe (List (User, UserInfo))
  , friendRequests: Maybe(List (User, UserInfo))
  }

friendsInitModel : FriendsModel
friendsInitModel = FriendsModel Nothing Nothing

type FriendsMsg =
  RemoveFriend Int
  | ApproveRequest Int
  | ForceFriendsUpdate
  | FriendsUpdated (Result Http.Error (List (User, UserInfo)))
  | FriendReqsUpdated (Result Http.Error (List (User, UserInfo)))
