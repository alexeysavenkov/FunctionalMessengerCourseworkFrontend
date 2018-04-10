module UserSearch exposing (..)

import GlobalUserSearch exposing (..)
import Global exposing (..)
import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


userSearchUpdate : UserSearchMsg -> Model -> (Model, Cmd UserSearchMsg)
userSearchUpdate msg model =
  case msg of
    _ -> (model, Cmd.none)

userSearchView : User -> UserSearchModel -> Html UserSearchMsg
userSearchView currentUser model = div [] []
