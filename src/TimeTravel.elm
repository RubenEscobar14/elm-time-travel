module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set
import Dict exposing (update)

initialStateWithTimeTravel rawGame =
    {rawModel = rawGame.initialState, paused = False}

viewWithTimeTravel rawGame computer model =
  rawGame.view computer model.rawModel

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