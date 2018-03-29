module Authorization exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Debug
import Global exposing (..)

import Backend



authUpdate : AuthMsg -> Model -> (Model, Cmd AuthMsg)
authUpdate msg model =
  case msg of
    UpdateAuthModel newModel ->
      ({model|authModel={newModel|message=""}}, Cmd.none)
    Register ->
      (model, Http.send LoggedIn (Backend.registerRequest model.authModel.phone model.authModel.password))
    Login ->
      (model, Http.send LoggedIn (Backend.loginRequest model.authModel.phone model.authModel.password))
    LoggedIn (Ok user) ->
      ({model|screen=ProfileScreen, currentUser=Just user, authModel=authInitModel}, Cmd.none)
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
    , input [onInput (\x -> UpdateAuthModel {authModel|password=x}), placeholder "Password"] []
    , br [] []
    , button [onClick Register] [text "Register"]
    , button [onClick Login] [text "Login"]
    , div [] [text authModel.message]
    ]
