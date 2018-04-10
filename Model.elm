module Model exposing (..)

type alias Model =
  { topic : String
  , gifUrl : String
  , screen : Screen
  , authModel : AuthModel
  , profileModel : ProfileModel
  }

type Screen =
  AuthorizationScreen

  type Msg
  = MorePlease
  | NewGif (Result Http.Error String)
  | AuthMsg AuthMsg
