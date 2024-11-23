module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set
import Dict exposing (update)

controlBarHeight : number
controlBarHeight = 64
maxVisibleHistory = 2000

initialStateWithTimeTravel rawGame =
    {rawModel = rawGame.initialState, paused = False, history = []}

viewWithTimeTravel rawGame computer model =
  let
    histLength = List.length model.history
    -- get % of list completed (histLength / maxVisibleHistory * 100) and multiply it by 2.56
    colorLength = (toFloat histLength) / (toFloat maxVisibleHistory) * 256
    myRed = 256.0 - colorLength
    myBlue = colorLength
    -- Creates a rectangle at the top of the screen, stretching from the
    -- left edge up to a specific position within the history timeline
    historyBar color opacity index =
      let
        width = historyIndexToX computer index
      in
        rectangle color width controlBarHeight
          |> move (computer.screen.left + width/2)
                  (computer.screen.top - controlBarHeight/2)
          |> fade opacity
    helpMessage =
      if model.paused then
        "Press R to resume"
      else
        "Press T to time travel"
  in
    (rawGame.view computer model.rawModel) ++
      [ historyBar black 0.3 maxVisibleHistory
      , historyBar (rgb myRed 5.0 myBlue) 0.6 histLength
      , words white helpMessage
          |> move 0 (computer.screen.top - controlBarHeight / 2)
      ]

updateWithTimeTravel rawGame computer model =
  if keyPressed "T" computer then
    { model | paused = True}
  else if keyPressed "R" computer then
    { model | paused = False, rawModel = rawGame.updateState computer model.rawModel
    , history = model.history ++ [computer]
    }
  else if model.paused then
    model
  else
    { model | rawModel = rawGame.updateState computer model.rawModel
    , history = model.history ++ [computer]
    }

addTimeTravel rawGame =
  { initialState = initialStateWithTimeTravel rawGame
  , updateState = updateWithTimeTravel rawGame
  , view = viewWithTimeTravel rawGame
  }

-- Helper functions 
keyPressed keyName computer = 
  [ String.toLower keyName
  , String.toUpper keyName
  ]
    |> List.any (\key -> Set.member key computer.keyboard.keys)

-- Converts an index in the history list to an x coordinate on the screen
historyIndexToX computer index =
  (toFloat index) / maxVisibleHistory * computer.screen.width