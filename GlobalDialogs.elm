module GlobalDialogs exposing (..)

type alias DialogsModel =
  { }

dialogsInitModel : DialogsModel
dialogsInitModel = {}

type DialogsMsg
  = GoToDialog Int
