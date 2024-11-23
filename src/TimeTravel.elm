module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set
import Dict exposing (update)

controlBarHeight = 64

initialStateWithTimeTravel rawGame =
    {rawModel = rawGame.initialState, paused = False}

viewWithTimeTravel rawGame computer model =
  let
    helpMessage =
      if model.paused then
        "Press R to resume"
      else
        "Press T to time travel"
  in
    (rawGame.view computer model.rawModel) ++
      [ words white helpMessage
          |> move 0 (computer.screen.top - controlBarHeight / 2)
      ]

updateWithTimeTravel rawGame computer model =
  if keyPressed "T" computer then
    { model | paused = True}
  else if keyPressed "R" computer then
    { model | paused = False, rawModel = rawGame.updateState computer model.rawModel}
  else if model.paused then
    model
  else
    { model | rawModel = rawGame.updateState computer model.rawModel }

addTimeTravel rawGame =
  { initialState = initialStateWithTimeTravel rawGame
  , updateState = updateWithTimeTravel rawGame
  , view = viewWithTimeTravel rawGame
  }

keyPressed keyName computer = 
  [ String.toLower keyName
  , String.toUpper keyName
  ]
    |> List.any (\key -> Set.member key computer.keyboard.keys)