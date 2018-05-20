module Main exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Authorization exposing (..)
import Profile exposing (..)
import GlobalProfile exposing (..)
import GlobalAuth exposing (..)
import GlobalUserSearch exposing (..)
import UserSearch exposing (..)
import GlobalDialogs exposing (..)
import GlobalFriends exposing (..)
import GlobalMessages exposing (..)
import GlobalModerator exposing (..)
import Dialogs exposing (..)
import Friends exposing (..)
import Messages exposing (..)
import Moderator exposing (..)
import Global exposing (..)
import Models exposing (..)
import Backend exposing (..)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL



init : (Model, Cmd Msg)
init =
  ( Model
      AuthorizationScreen
      authInitModel profileInitModel
      userSearchInitModel
      dialogsInitModel
      friendsInitModel
      messagesInitModel
      moderatorInitModel
      Maybe.Nothing
  , Cmd.none
  )



-- UPDATE



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Logout ->
      ( { model | currentUser = Nothing, screen = AuthorizationScreen }, Cmd.none )

    Auth authMsg ->
      let (newModel, cmd) = authUpdate authMsg model
      in (newModel, cmd)

    Profile profileMsg ->
      let (newModel, cmd) = profileUpdate profileMsg model
      in (newModel, Cmd.map Profile cmd)

    -- перелік подій та реакції на них продовжується далі
    GoToProfile user ->
      let
        profileModel = model.profileModel
        newProfileModel = {profileModel|notSavedUser=user, savedUser=user}
        newModel = {model|profileModel = newProfileModel, screen = ProfileScreen}
      in (newModel, Http.send (\x -> Profile (ProfileForceUpdated x)) (Backend.userInfoRequest (currentUser model) newModel.profileModel.notSavedUser.id))

    GoToFriends ->
      let friendsModel = model.friendsModel
          newFriendsModel = friendsInitModel in
            ( { model | screen = FriendsScreen, friendsModel = newFriendsModel },
                Cmd.batch
                [ Http.send (\x -> Friends (FriendsUpdated x))
                  (Backend.loadFriends (currentUser model))
                , Http.send (\x -> Friends (FriendReqsUpdated x))
                  (Backend.loadFriendReqs (currentUser model))
                ]
            )

    GoToDialogs ->
      let dialogsModel = model.dialogsModel
          newDialogsModel = dialogsInitModel in
            ( { model | screen = DialogsScreen, dialogsModel = newDialogsModel },
                  Cmd.batch
                  [ Http.send (\x -> Dialogs (DialogsLoaded x)) (Backend.loadDialogs (currentUser model))
                  , Http.send (\x -> Dialogs (FriendsLoaded x)) (Backend.loadFriends (currentUser model))
                  ])

    ChangeScreen newScreen ->
      let user = (if model.screen == ProfileScreen then model.profileModel.savedUser else currentUser model)
          oldProfile = model.profileModel
          newProfile = (if newScreen == ProfileScreen then { oldProfile | notSavedUser = user, savedUser = user } else oldProfile) in
      ( { model | screen = newScreen, profileModel = newProfile, currentUser = Just user }, Cmd.none )

    UserSearch userSearchMsg ->
      let (newModel, cmd) = userSearchUpdate userSearchMsg model
      in (newModel, Cmd.map UserSearch cmd)

    Dialogs dialogsMsg ->
      let (newModel, cmd) = dialogsUpdate dialogsMsg model
      in (newModel, cmd)

    Friends friendsMsg ->
      let (newModel, cmd) = friendsUpdate friendsMsg model
      in (newModel, cmd)

    Messages messagesMsg ->
      messagesUpdate messagesMsg model

    Moderator moderatorMsg ->
      let (newModel, cmd) = moderatorUpdate moderatorMsg model
      in (newModel, cmd)




-- VIEW


view : Model -> Html Msg
view model =
  let body =
    case model.screen of
      AuthorizationScreen -> Html.map Auth (authView model.authModel)
      ModeratorScreen -> Html.map Moderator (moderatorView (currentUser model) model.moderatorModel)
      _ ->
        div []
          [ button [onClick Logout, class "form-control"] [text "Logout"]
          , button [onClick (ChangeScreen UserSearchScreen), class "form-control"] [text "Search Users"]
          , button [onClick (GoToDialogs), class "form-control"] [text "Go to dialogs"]
          , button [onClick (ChangeScreen ProfileScreen), class "form-control"] [text "My Profile"]
          , button [onClick GoToFriends, class "form-control"] [text "Friends"]
          , case model.screen of
              ProfileScreen -> Html.map Profile (profileView (currentUser model) model.profileModel)
              UserSearchScreen -> (userSearchView (currentUser model)  model.userSearchModel)
              FriendsScreen -> (friendsView (currentUser model) model.friendsModel)
              DialogsScreen -> Html.map Dialogs (dialogsView (currentUser model) model.dialogsModel)
              MessagesScreen -> messagesView (currentUser model) model.messagesModel
              ModeratorScreen -> Html.map Moderator (moderatorView (currentUser model) model.moderatorModel)
              _ -> p [] []
          ]
  in div [] [
      node "link" [ rel "stylesheet", href "/site.css" ] [],
      node "link" [ rel "stylesheet", href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" ] [],
      node "script" [ src "https://code.jquery.com/jquery-3.2.1.slim.min.js" ] [],
      node "script" [ src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js" ] [],
      node "script" [ src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" ] [],
      body]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



