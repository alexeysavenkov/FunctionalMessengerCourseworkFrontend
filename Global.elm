module Global exposing (..)

import Http
import GlobalAuth exposing (..)
import GlobalProfile exposing (..)
import GlobalUserSearch exposing (..)
import GlobalDialogs exposing (..)
import GlobalFriends exposing (..)
import GlobalMessages exposing (..)
import Models exposing (..)
import GlobalModerator exposing (..)

type alias Model =
  { screen : Screen
  , authModel : AuthModel
  , profileModel : ProfileModel
  , userSearchModel : UserSearchModel
  , dialogsModel : DialogsModel
  , friendsModel : FriendsModel
  , messagesModel : MessagesModel
  , moderatorModel : ModeratorModel
  , currentUser : Maybe User
  }

type Screen
  = AuthorizationScreen
  | ProfileScreen
  | UserSearchScreen
  | DialogsScreen
  | FriendsScreen
  | BlacklistScreen
  | MessagesScreen
  | ModeratorScreen

type Msg
  = Auth AuthMsg
  | Profile ProfileMsg
  | UserSearch UserSearchMsg
  | Dialogs DialogsMsg
  | Friends FriendsMsg
  | Messages MessagesMsg
  | Moderator ModeratorMsg
  | Logout
  | ChangeScreen Screen
  | GoToProfile User
  | GoToFriends
  | GoToDialogs

currentUser : Model -> User
currentUser model = Maybe.withDefault emptyUser model.currentUser

