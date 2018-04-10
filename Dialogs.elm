module Dialogs exposing (..)

import Global exposing (..)
import GlobalDialogs exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)


dialogsUpdate : DialogsMsg -> Model -> (Model, Cmd DialogsMsg)
dialogsUpdate msg model =
  case msg of
    _ -> (model, Cmd.none)

dialogsView : User -> DialogsModel -> Html DialogsMsg
dialogsView currentUser model = div [] []
