module Backend exposing (..)

import Http
import Json.Decode exposing (..)
import Json.Encode
import Global exposing (..)

baseUrl : String
baseUrl = "http://localhost:9000"

endpointUrl : String -> String
endpointUrl endpt = baseUrl ++ endpt

registerRequest : String -> String -> Http.Request User
registerRequest phone password =
  let
    url = endpointUrl "/register"
    payload = encodeCredentials phone password
  in
    Http.post url (Http.jsonBody payload) userDecoder

loginRequest : String -> String -> Http.Request User
loginRequest phone password =
  let
    url = endpointUrl "/login"
    payload = encodeCredentials phone password
  in
    Http.post url (Http.jsonBody payload) userDecoder

encodeCredentials : String -> String -> Json.Encode.Value
encodeCredentials phone password =
  (Json.Encode.object
        [ ("phone", Json.Encode.string phone)
        , ("password", Json.Encode.string password)
        ]
      )

userDecoder : Json.Decode.Decoder User
userDecoder =
  Json.Decode.map5 User
    (field "id" int)
    (field "phone" string)
    (oneOf[field "name" string, succeed ""])
    (field "description" string)
    (field "authToken" string)
