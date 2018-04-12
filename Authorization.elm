module Authorization exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Debug
import GlobalAuth exposing (..)
import GlobalProfile exposing (..)
import GlobalModerator exposing (..)
import Global exposing (..)

import Backend



authUpdate : AuthMsg -> Model -> (Model, Cmd Msg)
authUpdate msg model =
  case msg of
    UpdateAuthModel newModel ->
      ({model|authModel={newModel|message=""}}, Cmd.none)
    Register ->
      (model, Http.send (\x -> Auth (LoggedIn x)) (Backend.registerRequest model.authModel.phone model.authModel.password))
    Login ->
      (model, Http.send (\x -> Auth (LoggedIn x)) (Backend.loginRequest model.authModel.phone model.authModel.password))
    LoggedIn (Ok user) ->
      let profileModel = model.profileModel
          (screen, cmd) =
            (if user.isModerator
            then (ModeratorScreen, Http.send (\x -> Moderator (LoadedComplaints x)) (Backend.unresolvedComplaints user))
            else (ProfileScreen, Cmd.none)) in
      ({model| screen=screen, currentUser=Just user, authModel=authInitModel
             , profileModel = {profileModel|notSavedUser=user, savedUser=user}}, cmd)
    LoggedIn (Err (Http.BadStatus response)) ->
      let authModel = model.authModel
      in ({model|authModel={authModel|message=response.body}}, Cmd.none)
    LoggedIn (Err e) ->
      let authModel = model.authModel
      in ({model|authModel={authModel|message=(toString e)}}, Cmd.none)

authView : AuthModel -> Html AuthMsg
authView authModel =
  div []
    [ h1 [] [text "Authorization"]
    , input [onInput (\x -> UpdateAuthModel {authModel|phone=x}), placeholder "Phone"] []
    , br [] []
    , input [onInput (\x -> UpdateAuthModel {authModel|password=x}), placeholder "Password", type_ "password"] []
    , br [] []
    , button [onClick Register] [text "Register"]
    , button [onClick Login] [text "Login"]
    , div [style [("color", "red")]] [text authModel.message]
    ]
