module Messages exposing (..)

import Global exposing (..)
import GlobalDialogs exposing (..)
import GlobalMessages exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import GlobalFriends exposing (..)
import Http exposing (..)

import Backend exposing (..)


messagesUpdate : MessagesMsg -> Model -> (Model, Cmd Msg)
messagesUpdate msg model =
  case msg of
    LoadMessages ->
      (model, Http.send (\x -> Messages (MessagesLoaded x)) (Backend.loadMessages (currentUser model) (Tuple.first model.messagesModel.dialog)))
    MessagesLoaded (Ok messages) ->
      let messagesModel = model.messagesModel
      in ({model|messagesModel={messagesModel|messages=Just messages}}, Cmd.none)
    UpdateMessageText txt ->
      let messagesModel = model.messagesModel
      in ({model|messagesModel={messagesModel|messageText=txt}}, Cmd.none)
    SendMessage ->
      let messagesModel = model.messagesModel
          messageText = messagesModel.messageText
          dialogId = Tuple.first model.messagesModel.dialog
      in ({model|messagesModel={messagesModel|messageText=""}}, Http.send (\x -> Messages LoadMessages) (sendMessage (currentUser model) dialogId messageText))
    _ -> (model, Cmd.none)


messagesView : User -> MessagesModel -> Html Msg
messagesView currentUser model =
    div []
      [ h2 [] [text "Dialog"]
      , h3 [] [text (Tuple.second model.dialog)]
      , div []
        (case model.messages of
          Just messages -> List.map (\(user, msg) ->
            div [class "profilePreview", onClick (GoToProfile user)] [
                img [src (avatarUrl currentUser user)] [],
                div [class "name"] [text (user.name ++ ":")],
                div [class "msg"] [text msg]
              ]
          ) messages
          Nothing -> [text "Loading..."]
        )
      , textarea [onInput (\x -> Messages (UpdateMessageText x))] [text model.messageText]
      , button [onClick (Messages SendMessage)] [text "Send message"]
      ]
