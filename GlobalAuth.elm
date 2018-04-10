module GlobalAuth exposing (..)

import Models exposing (..)
import Http

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
