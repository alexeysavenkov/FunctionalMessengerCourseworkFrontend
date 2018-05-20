module Friends exposing (..)

import Global exposing (..)
import GlobalDialogs exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import GlobalFriends exposing (..)
import Http exposing (..)

import Backend exposing (..)


friendsUpdate : FriendsMsg -> Model -> (Model, Cmd Msg)
friendsUpdate msg model =
  case msg of
    RemoveFriend userId ->
      (model, Http.send (\_ -> Friends ForceFriendsUpdate)
          (Backend.friendRequest (currentUser model) userId False)
        )
    ApproveRequest userId ->
      (model, Http.send (\_ -> Friends ForceFriendsUpdate)
          (Backend.friendRequest (currentUser model) userId True)
        )
    ForceFriendsUpdate ->
      (model, Cmd.batch
          [ Http.send (\x -> Friends (FriendsUpdated x))
            (Backend.loadFriends (currentUser model))
          , Http.send (\x -> Friends (FriendReqsUpdated x))
            (Backend.loadFriendReqs (currentUser model))
          ]
        )
    FriendsUpdated (Ok friends) ->
      let friendsModel = model.friendsModel
      in ({model|friendsModel={friendsModel|friends = Just friends}}, Cmd.none)
    FriendReqsUpdated (Ok friendReqs) ->
      let friendsModel = model.friendsModel
      in ({model|friendsModel={friendsModel|friendRequests = Just friendReqs}}, Cmd.none)
    _ -> (model, Cmd.none)


friendsView : User -> FriendsModel -> Html Msg
friendsView currentUser model =
    div []
      [ h2 [] [text "Friend Requests"]
      , case model.friendRequests of
          Just reqs -> div [] (List.map (renderFriendReqPreview currentUser) reqs)
          Nothing -> text "Loading..."
      , h2 [] [text "Friends"]
      , case model.friends of
          Just fr -> div [] (List.map (renderFriendPreview currentUser) fr)
          Nothing -> text "Loading..."
      ]

renderFriendPreview : User -> (User, UserInfo) -> Html Msg
renderFriendPreview currentUser (user, userInfo) =
  div [onClick (GoToProfile user)] [
    img [src (avatarUrl currentUser user)] [],
    div [class "name"] [text user.name],
    div [class "description"] [text user.description],
    button [onClick (Friends (RemoveFriend user.id))] [text "Remove From Friends"]
  ]

renderFriendReqPreview : User -> (User, UserInfo) -> Html Msg
renderFriendReqPreview currentUser (user, userInfo) =
  div [onClick (GoToProfile user)] [
    img [src (avatarUrl currentUser user)] [],
    div [class "name"] [text user.name],
    div [class "description"] [text user.description],
    button [onClick (Friends (RemoveFriend user.id))] [text "Decline"],
    button [onClick (Friends (ApproveRequest user.id))] [text "Approve"]
  ]

