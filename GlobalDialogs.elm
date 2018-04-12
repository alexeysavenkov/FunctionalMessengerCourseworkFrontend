module GlobalDialogs exposing (..)

import Models exposing (..)
import Http exposing (..)

type alias DialogsModel =
  { nameInput : String
  , selectedFriends : List (User, Bool)
  , dialogs : List (Int, String)
  }

dialogsInitModel : DialogsModel
dialogsInitModel = DialogsModel "" [] []

type DialogsMsg
  = GoToDialog (Int, String)
  | NameInput String
  | ToggleFriend User
  | LoadDialogs
  | DialogsLoaded (Result Http.Error (List(Int, String)))
  | FriendsLoaded (Result Http.Error (List(User, UserInfo)))
  | CreateDialog
