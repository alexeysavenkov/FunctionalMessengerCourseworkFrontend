module GlobalUserSearch exposing (..)

import Models exposing (..)
import Http

type alias UserSearchModel =
  { query : String
  , results : List User
  }

userSearchInitModel : UserSearchModel
userSearchInitModel = UserSearchModel "" []

type UserSearchMsg
  = SearchUsers
  | SearchUsersCompleted (Result Http.Error (List User))

