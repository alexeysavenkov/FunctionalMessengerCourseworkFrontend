module Dialogs exposing (..)

import Global exposing (..)
import GlobalDialogs exposing (..)
import GlobalMessages exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)

import Http exposing (..)
import Backend
import Debug


dialogsUpdate : DialogsMsg -> Model -> (Model, Cmd Msg)
dialogsUpdate msg model =
  case msg of
    GoToDialog (id, name) ->
      let messagesModel = model.messagesModel
      in ({model | screen = MessagesScreen, messagesModel = { messagesModel | dialog = (id,name), messages = Nothing }},
        Http.send (\x -> Messages (MessagesLoaded x)) (Backend.loadMessages (currentUser model) id)
      )
    ToggleFriend user ->
      let
        dialogsModel = model.dialogsModel
        oldState = dialogsModel.selectedFriends
        newState = List.map (\(x,f) -> if x == user then (x,not f) else (x, f)) oldState
      in ({model | dialogsModel = { dialogsModel | selectedFriends = newState }}, Cmd.none)
    LoadDialogs ->
      (model, Http.send (\x -> Dialogs (DialogsLoaded x)) (Backend.loadDialogs (currentUser model)))
    DialogsLoaded (Ok dialogs) ->
      let
        _ = Debug.log "dialogs" dialogs
        dialogsModel = model.dialogsModel
      in ({model | dialogsModel = { dialogsModel | dialogs = dialogs }}, Cmd.none)
    FriendsLoaded (Ok friends) ->
      let
        _ = Debug.log "friends" (toString friends)
        dialogsModel = model.dialogsModel
        friendsTrigger = List.map (\(user, _) -> (user, False)) friends
      in ({model | dialogsModel = { dialogsModel | selectedFriends = friendsTrigger }}, Cmd.none)
    NameInput name ->
      let
        dialogsModel = model.dialogsModel
      in({model | dialogsModel = { dialogsModel | nameInput = name }}, Cmd.none)
    CreateDialog ->
      let
        members = List.map (\(user,_) -> user.id) (List.filter (\(_, b) -> b) model.dialogsModel.selectedFriends)
      in (model, Http.send (\x -> Dialogs LoadDialogs) (Backend.createDialog (currentUser model) model.dialogsModel.nameInput members))
    err ->
      Debug.log (toString err)
      Debug.crash (toString err)
      (model, Cmd.none)

dialogsView : User -> DialogsModel -> Html DialogsMsg
dialogsView currentUser model =
    div []
      [ h2 [class "display-4"] [text "Create New Dialog"]
      , input [onInput NameInput, placeholder "Name of new dialog", class "form-control"] [text model.nameInput]
      , b [] [text "Select users for new dialog"]
      , div []
          (List.map (\(user, flag) ->
            div []
              [ input [type_ "checkbox", checked flag, onClick (ToggleFriend user)] []
              , span [class "name"] [text user.name]
              ]
          ) model.selectedFriends)
      , button [onClick CreateDialog, disabled (List.isEmpty (List.filter (\(_, b) -> b) model.selectedFriends) || String.isEmpty model.nameInput)] [text "Create Dialog"]
      , br [] []
      , h2 [class "display-4"] [text "Dialogs"]
      , div [class "list-group"]
          (List.map (\(id, name) ->
            div [onClick (GoToDialog (id,name)), class "list-group-item"]
              [ text ("Dialog " ++ name)

              ]
            ) model.dialogs)
      ]
