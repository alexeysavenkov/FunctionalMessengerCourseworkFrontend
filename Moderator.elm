module Moderator exposing (..)

import GlobalModerator exposing (..)
import Global exposing (..)
import Models exposing (..)
import Backend exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http



moderatorView : User -> ModeratorModel -> Html ModeratorMsg
moderatorView currentUser model =
  case model.screen of
    Home ->
      div [] [
        h1 [] [text "Moderator dashboard"],
        button [onClick LoadComplaints] [text "Refresh"],
        div []
          (List.map
            (\(user, reasons, complaints, messages) ->
                div [class "profilePreview", onClick (GoToScreen (UserMessages user))] [
                  img [src (avatarUrl currentUser user)] [],
                  div [class "name"] [text user.name],
                  div [] [text ("Complaints: " ++ (toString complaints))],
                  div [] [text "Click to see details"]
                ]
          ) model.complaintsByUser)
      ]
    UserMessages user ->
      let
        (user, reasons, complaints, messages) = Maybe.withDefault (emptyUser, "", 0, []) (List.head (List.filter (\(u, _, _, _) -> u.id == user.id) model.complaintsByUser))
      in div [] [
          button [onClick (GoToScreen Home)] [text "Go Back"],
          h2 [] [text "Details"],
          h3 [] [text ("Reasons: " ++ reasons)],
          button [onClick (BanUser user)] [text "Ban User"],
          button [onClick (ForgiveUser user)] [text "Forgive User"],
          h3 [] [text "Messages:"],
          ul [] (
            List.map (\msg -> li [] [text msg]) messages
          )
        ]

moderatorUpdate : ModeratorMsg -> Model -> (Model, Cmd Msg)
moderatorUpdate moderatorMsg model =
  let moderatorModel = model.moderatorModel in
    case moderatorMsg of
      BanUser user ->
        (model, Http.send (\x -> Moderator LoadComplaints) (Backend.banUser (currentUser model) user.id))
      ForgiveUser user ->
        (model, Http.send (\x -> Moderator LoadComplaints) (Backend.forgiveUser (currentUser model) user.id))
      LoadComplaints ->
        (model, Http.send (\x -> Moderator (LoadedComplaints x)) (Backend.unresolvedComplaints (currentUser model)))
      LoadedComplaints (Ok complaints) ->
        ({model|moderatorModel={moderatorModel|complaintsByUser=complaints}}, Cmd.none)
      LoadedComplaints _ ->
        (model, Cmd.none)
      GoToScreen screen ->
        ({model|moderatorModel={moderatorModel|screen=screen}}, Http.send (\x -> Moderator (LoadedComplaints x)) (Backend.unresolvedComplaints (currentUser model)))

