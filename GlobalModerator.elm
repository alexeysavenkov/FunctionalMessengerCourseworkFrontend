module GlobalModerator exposing (..)

import Models exposing (..)
import Http exposing (..)

type alias ModeratorModel =
  { complaintsByUser : List (User, String, Int, List String)
  , screen : ModScreen
  }

moderatorInitModel : ModeratorModel
moderatorInitModel = ModeratorModel [] Home

type ModScreen = Home | UserMessages User

type ModeratorMsg =
  BanUser User
  | ForgiveUser User
  | LoadComplaints
  | LoadedComplaints (Result Http.Error (List (User, String, Int, List String)))
  | GoToScreen ModScreen
