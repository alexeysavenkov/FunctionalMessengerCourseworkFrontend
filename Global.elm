module Global exposing (..)

import Http
import GlobalAuth exposing (..)
import GlobalProfile exposing (..)
import GlobalUserSearch exposing (..)
import GlobalDialogs exposing (..)
import Models exposing (..)

type alias Model =
  { topic : String
  , gifUrl : String
  , screen : Screen
  , authModel : AuthModel
  , profileModel : ProfileModel
  , userSearchModel : UserSearchModel
  , dialogsModel : DialogsModel
  , currentUser : Maybe User
  }

type Screen
  = AuthorizationScreen
  | ProfileScreen
  | UserSearchScreen
  | DialogsScreen
  | FriendsScreen

type Msg
  = Auth AuthMsg
  | Profile ProfileMsg
  | UserSearch UserSearchMsg
  | Dialogs DialogsMsg
  | Logout
  | ChangeScreen Screen

  -- AUTHORIZATION
