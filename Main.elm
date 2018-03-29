module Main exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Authorization exposing (..)
import Global exposing (..)


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
  ( Model topic "waiting.gif" AuthorizationScreen authInitModel Maybe.Nothing
  , Cmd.none
  )



-- UPDATE



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Auth authMsg ->
      let (newModel, cmd) = authUpdate authMsg model
      in (newModel, Cmd.map Auth cmd)

    Logout ->
      ( { model | currentUser = Nothing, screen = AuthorizationScreen }, Cmd.none )




-- VIEW


view : Model -> Html Msg
view model =
  case model.screen of
    AuthorizationScreen -> Html.map Auth (authView model.authModel)
    ProfileScreen -> button [onClick Logout] [text "Logout"]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string
