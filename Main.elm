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
import Dialogs exposing (..)
import Global exposing (..)
import Models exposing (..)


main : Program Never Model Msg
main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL



init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "waiting.gif" AuthorizationScreen authInitModel profileInitModel userSearchInitModel dialogsInitModel Maybe.Nothing
  , Cmd.none
  )



-- UPDATE



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Auth authMsg ->
      let (newModel, cmd) = authUpdate authMsg model
      in (newModel, Cmd.map Auth cmd)

    Profile profileMsg ->
      let (newModel, cmd) = profileUpdate profileMsg model
      in (newModel, Cmd.map Profile cmd)

    Logout ->
      ( { model | currentUser = Nothing, screen = AuthorizationScreen }, Cmd.none )

    ChangeScreen newScreen ->
      let user = (if model.screen == ProfileScreen then model.profileModel.savedUser else currentUser model)
          oldProfile = model.profileModel
          newProfile = (if newScreen == ProfileScreen then { oldProfile | notSavedUser = user, savedUser = user } else oldProfile) in
      ( { model | screen = newScreen, profileModel = newProfile, currentUser = Just user }, Cmd.none )

    UserSearch userSearchMsg ->
      ( model, Cmd.none )

    Dialogs dialogsMsg ->
      ( model, Cmd.none )




-- VIEW


view : Model -> Html Msg
view model =
  case model.screen of
    AuthorizationScreen -> Html.map Auth (authView model.authModel)
    _ ->
      div []
        [ button [onClick Logout] [text "Logout"]
        , button [onClick (ChangeScreen UserSearchScreen)] [text "Search Users"]
        , button [onClick (ChangeScreen DialogsScreen)] [text "Go to dialogs"]
        , button [onClick (ChangeScreen ProfileScreen)] [text "My Profile"]
        , button [onClick (ChangeScreen FriendsScreen)] [text "Friends"]
        , case model.screen of
            ProfileScreen -> Html.map Profile (profileView (currentUser model) model.profileModel)
            UserSearchScreen -> Html.map UserSearch (userSearchView (currentUser model)  model.userSearchModel)
            _ -> p [] []
        ]

currentUser : Model -> User
currentUser model = Maybe.withDefault emptyUser model.currentUser


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string
