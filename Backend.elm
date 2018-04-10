module Backend exposing (..)

import Http
import Json.Decode exposing (..)
import Json.Encode
import Global exposing (..)

import Models exposing (..)

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

profileSaveRequest : String -> String -> String -> Http.Request User
profileSaveRequest name description avatarUrl =
  let
    url = endpointUrl "/profile"
    payload = encodeProfileSave name description avatarUrl
  in
    Http.post url (Http.jsonBody payload) userDecoder

encodeCredentials : String -> String -> Json.Encode.Value
encodeCredentials phone password =
  (Json.Encode.object
        [ ("phone", Json.Encode.string phone)
        , ("password", Json.Encode.string password)
        ]
      )

encodeProfileSave : String -> String -> String -> Json.Encode.Value
encodeProfileSave name description avatarUrl =
  (Json.Encode.object
        [ ("name", Json.Encode.string name)
        , ("description", Json.Encode.string description)
        , ("avatarUrl", Json.Encode.string avatarUrl)
        ]
      )

httpSendAuthorized : String -> Json.Encode.Value -> Json.Decode.Decoder T -> Http.Request T
httpSendAuthorized
