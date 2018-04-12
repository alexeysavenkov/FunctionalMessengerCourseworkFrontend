module GlobalMessages exposing (..)

import Models exposing (..)
import Http

type alias MessagesModel =
  { dialog : (Int, String)
  , messages : Maybe (List (User, String))
  , messageText : String
  }

messagesInitModel : MessagesModel
messagesInitModel = MessagesModel (-1, "") Nothing ""

type MessagesMsg
  = LoadMessages
  | MessagesLoaded (Result Http.Error (List (User, String)))
  | UpdateMessageText String
  | SendMessage


