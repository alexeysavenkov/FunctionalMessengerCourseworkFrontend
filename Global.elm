module Global exposing (..)

import Http

type alias Model =
  { topic : String
  , gifUrl : String
  , screen : Screen
  , authModel : AuthModel
  , currentUser : Maybe User
  }

type Screen
  = AuthorizationScreen
  | ProfileScreen

type Msg
  = Auth AuthMsg
  | Logout

  -- AUTHORIZATION

type alias AuthModel =
  { phone : String
  , password : String
  , message : String
  }

authInitModel : AuthModel
authInitModel = AuthModel "" "" ""


type AuthMsg
  = Register
  | Login
  | UpdateAuthModel AuthModel
  | LoggedIn (Result Http.Error User)

type alias User =
  { id : Int
  , phone : String
  , name : String
  , description : String
  , authToken : String
  }
