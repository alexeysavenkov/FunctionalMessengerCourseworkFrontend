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

profileSaveRequest : User -> String -> String -> String -> Http.Request User
profileSaveRequest currentUser name description avatarUrl =
  let
    url = endpointUrl "/profile"
    payload = encodeProfileSave name description avatarUrl
  in
    httpSendAuthorized url "POST" currentUser (Http.jsonBody payload) userDecoder

searchUsersRequest : User -> String -> Http.Request (List User)
searchUsersRequest user query =
  let
    url = endpointUrl ("/user/search?q=" ++ query)
  in
    httpSendAuthorized url "GET" user Http.emptyBody (list userDecoder)

userInfoRequest : User -> Int -> Http.Request UserInfo
userInfoRequest user userId =
  let
    url = endpointUrl ("/user/" ++ (toString userId) ++ "/info")
  in
    httpSendAuthorized url "GET" user Http.emptyBody userInfoDecoder

friendRequest : User -> Int -> Bool -> Http.Request ()
friendRequest user userId flag =
  let
    url = endpointUrl ("/user/" ++ (toString userId) ++ "/friendRequest?cancel=" ++(if flag then "0" else "1"))
  in
    httpSendAuthorizedNoResponse url "POST" user Http.emptyBody

blacklistRequest : User -> Int -> Bool -> Http.Request ()
blacklistRequest user userId flag =
  let
    url = endpointUrl ("/user/" ++ (toString userId) ++ "/blacklist?cancel=" ++(if flag then "0" else "1"))
  in
    httpSendAuthorizedNoResponse url "POST" user Http.emptyBody

reportRequest : User -> Int -> String -> Http.Request ()
reportRequest user userId reason =
  let
    url = endpointUrl ("/user/" ++ (toString userId) ++ "/report?reason=" ++ reason)
  in
    httpSendAuthorizedNoResponse url "POST" user Http.emptyBody


loadFriends : User -> Http.Request (List (User, UserInfo))
loadFriends user =
  let
    url = endpointUrl "/user/friends"
  in
    httpSendAuthorized url "GET" user Http.emptyBody (list userWithInfoDecoder)

loadFriendReqs : User -> Http.Request (List (User, UserInfo))
loadFriendReqs user =
  let
    url = endpointUrl "/user/friendRequests"
  in
    httpSendAuthorized url "GET" user Http.emptyBody (list userWithInfoDecoder)


loadDialogs : User -> Http.Request (List (Int, String))
loadDialogs user =
  let
    url = endpointUrl "/dialogs"
  in
    httpSendAuthorized url "GET" user Http.emptyBody (list (Json.Decode.map2 (\a b -> (a,b)) (field "id" int) (field "name" string)))

createDialog : User -> String -> List Int -> Http.Request ()
createDialog user name members =
  let
    url = endpointUrl ("/dialog/create?name=" ++ name)
    payload = Json.Encode.list (List.map Json.Encode.int members)
  in
    httpSendAuthorizedNoResponse url "POST" user (Http.jsonBody payload)


loadMessages : User -> Int -> Http.Request (List (User,String))
loadMessages user dialogId =
  let
    url = endpointUrl ("/dialog/" ++ (toString dialogId) ++ "/messages")
  in
    httpSendAuthorized url "GET" user Http.emptyBody (list (Json.Decode.map2 (\a b -> (a,b)) (field "user" userDecoder) (field "message" string)))

sendMessage : User -> Int -> String -> Http.Request ()
sendMessage user dialogId messageText =
  let
    url = endpointUrl "/dialog/" ++ (toString dialogId) ++ "/sendMessage?message=" ++ messageText
    payload = Json.Encode.list []
  in
    httpSendAuthorizedNoResponse url "POST" user (Http.jsonBody payload)

unresolvedComplaints : User -> Http.Request (List (User, String, Int, List String))
unresolvedComplaints user =
  let
    url = endpointUrl ("/moderator/unresolvedComplaints")
    decoder = list
      (Json.Decode.map4 (\a b c d -> (a,b,c,d))
        (field "user" userDecoder)
        (field "reasons" string)
        (field "count" int)
        (field "messages" (list string))
      )

  in
    httpSendAuthorized url "GET" user Http.emptyBody decoder

banUser : User -> Int -> Http.Request ()
banUser user userId =
  let
    url = endpointUrl ("/moderator/banUser?id=" ++ (toString userId))
  in
    httpSendAuthorizedNoResponse url "POST" user Http.emptyBody

forgiveUser : User -> Int -> Http.Request ()
forgiveUser user userId =
  let
    url = endpointUrl ("/moderator/forgiveUser?id=" ++ (toString userId))
  in
    httpSendAuthorizedNoResponse url "POST" user Http.emptyBody



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

httpSendAuthorizedNoResponse : String -> String -> User -> Http.Body -> Http.Request ()
httpSendAuthorizedNoResponse url method user body =
  httpSendAuthorized url method user body (Json.Decode.map (\x -> ()) (nullable string))

httpSendAuthorized : String -> String -> User -> Http.Body -> Json.Decode.Decoder t -> Http.Request t
httpSendAuthorized url method user body decoder =
  Http.request
    { method = method
    , headers = [Http.header "Auth" user.authToken]
    , url = url
    , body = body
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

avatarUrl : User -> User -> String
avatarUrl currentUser user =
  case user.avatarId of
    Just id -> baseUrl ++ "/image/" ++ (toString id) ++ "?auth=" ++ currentUser.authToken
    Nothing -> ""
