module Confirm where

import Html        exposing (..)
import Html.Events exposing (..)
import Signal      exposing (..)

type Action = NoOp
            | Prompt
            | Add String

actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp

type alias Model =
  { name : String }

initialModel : Model
initialModel =
  { name = "" }

-- PORTS --------------------------------

-- port is used to push the name stirng in from javascript
port addPlayer : Signal String

-- port is used to force the display of a prompt
port displayPrompt : Signal ()
port displayPrompt =
  actions.signal
    |> filter (\s -> s == Prompt) NoOp
    |> map (always ())

externalActions: Signal Action
externalActions =
  mergeMany
    [ (Add) <~ addPlayer ]

update: Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Prompt ->
      model
    Add name ->
      { model | name <- name }

model : Signal Model
model =
  let
    allActions = mergeMany
      [ actions.signal,
        externalActions ]
  in
    foldp update initialModel allActions

-- VIEWS --------------------------------
view address model =
  div [ ]
    [ text model.name,
      button [ onClick address Prompt ]
        [ text "Set Name" ]]

main : Signal Html
main = map (view actions.address) model
