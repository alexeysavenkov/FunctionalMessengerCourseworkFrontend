module Profile exposing (..)

import Global exposing (..)
import GlobalProfile exposing (..)
import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import DropZone exposing (DropZoneMessage(Drop), dropZoneEventHandlers, isHovering)
import FileReader exposing (Error(..), FileRef, NativeFile, readAsTextFile, readAsDataUrl)
import Task
import Debug
import Backend
import Http

profileUpdate : ProfileMsg -> Model -> (Model, Cmd ProfileMsg)
profileUpdate msg model =
  case msg of
    ChangeProfileInfo newInfo ->
      let profileModel = model.profileModel
          notSavedUser = profileModel.notSavedUser in
        ({ model | profileModel =
            { profileModel | notSavedUser = newInfo
            }
        }, Cmd.none)
    SaveProfileChanges ->
      let user = model.profileModel.notSavedUser in
        (model, Http.send ProfileChangesSaved
          (Backend.profileSaveRequest user.name user.description (Maybe.withDefault "" (List.head model.profileModel.contents)))
        )

    ProfileChangesSaved (Ok user) ->
      let profileModel = model.profileModel in
        ({ model | profileModel =
          { profileModel |
            notSavedUser = user
          , savedUser = user
          , files = []
          , contents = []
          }
         , currentUser = Just user
        }, Cmd.none)

    ProfileChangesSaved (Err e) ->
      (model, Cmd.none)

    DnD (Drop files) ->
        -- this happens when the user dropped something into the dropzone
        let profileModel = model.profileModel in
          ({ model | profileModel =
            { profileModel | dropZone =
            DropZone.update (Drop files) model.profileModel.dropZone

            -- update the DropZone model
            , files =
            files

            -- and store the dropped files
            }
          } , Cmd.batch <|
          -- also create a bunch of effects to read the files as text, one effect for each file
          List.map (readTextFile << .blob) files
          )

    DnD a ->
        -- these are opaque DropZone messages, just hand them to DropZone to deal with them
        let profileModel = model.profileModel in
          ( { model | profileModel =
              { profileModel | dropZone = DropZone.update a model.profileModel.dropZone }
            }
          , Cmd.none
          )

    FileReadSucceeded str ->
        -- this happens when an effect has finished and the file has successfully been loaded

        let profileModel = model.profileModel
            _ = Debug.log "success" 1 in
          ( { model | profileModel =
              { profileModel | contents = str :: model.profileModel.contents
              }
            }
          , Cmd.none
          )

    FileReadFailed err ->
        -- this happens when an effect has finished and there was an error loading hte file
        let _ = Debug.log ("fail: " ++ (toString err)) 1 in
          ( model
          , Cmd.none
          )

profileView : User -> ProfileModel -> Html ProfileMsg
profileView currentUser model =
  let isYourProfile = (model.savedUser.id == currentUser.id)
      userInfo = model.notSavedUser
  in
    div []
      [ h1 [] [text ("Profile of " ++ (if String.isEmpty userInfo.name then "*No name*" else userInfo.name))]
      , img [src (
          case (model.notSavedUser.avatarId, model.contents) of
            (Just id, _) -> "/image/" ++ (toString id)
            (_, x::_) -> x
            (_, _) -> ""
          )] []
      , renderDropZone model.dropZone
      , input [onInput (\x -> ChangeProfileInfo {userInfo|name=x}), placeholder "Name", value userInfo.name] []
      , br [] []
      , input [onInput (\x -> ChangeProfileInfo {userInfo|description=x}), placeholder "Description", value userInfo.description] []
      , br [] []
      , button [onClick SaveProfileChanges] [text "Save changes"]
      ]

renderDropZone : DropZone.Model -> Html ProfileMsg
renderDropZone dropZoneModel =
    Html.map DnD
        (div (renderZoneAttributes dropZoneModel) [div [style [("margin-top", "25px"), ("text-align","center")]] [text "Drop new avatar here"]])

renderZoneAttributes :
    DropZone.Model
    -> List (Html.Attribute (DropZoneMessage (List NativeFile)))
renderZoneAttributes dropZoneModel =
    (if DropZone.isHovering dropZoneModel then
        dropZoneHover
        -- style the dropzone differently depending on whether the user is hovering
     else
        dropZoneDefault
    )
        :: -- add the necessary DropZone event wiring
           dropZoneEventHandlers FileReader.parseDroppedFiles


containerStyles : Html.Attribute a
containerStyles =
    style [ ( "padding", "20px" ) ]


dropZoneDefault : Html.Attribute a
dropZoneDefault =
    style
        [ ( "height", "120px" )
        , ( "width" , "120px" )
        , ( "border-radius", "10px" )
        , ( "border", "3px dashed steelblue" )
        ]


dropZoneHover : Html.Attribute a
dropZoneHover =
    style
        [ ( "height", "120px" )
        , ( "width" , "120px" )
        , ( "border-radius", "10px" )
        , ( "border", "3px dashed red" )
        ]

readTextFile : FileRef -> Cmd ProfileMsg
readTextFile fileValue =
    readAsDataUrl fileValue
        |> Task.attempt
            (\res ->
                case res of
                    Ok val ->
                      let url = String.slice 1 -1 (toString val) in
                       let _ = Debug.log "url" url in
                        FileReadSucceeded url

                    Err error ->
                        FileReadFailed error
            )

