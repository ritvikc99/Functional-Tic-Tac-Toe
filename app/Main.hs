module Main where

import Data.Maybe (isNothing)
import TicTacToe

promptPlayer :: IO Player
promptPlayer = do
  putStrLn "Which player do you want to be? (X plays first.)"
  putStrLn "Enter X or O:"
  line <- getLine
  case line of
    "X" -> pure X
    "x" -> pure X
    "O" -> pure O
    "o" -> pure O
    _ -> do
      putStrLn "Invalid input, try again."
      promptPlayer

charToLineIndex :: Char -> Maybe LineIndex
charToLineIndex '0' = Just C0
charToLineIndex '1' = Just C1
charToLineIndex '2' = Just C2
charToLineIndex _ = Nothing

promptBoardIndex :: Board (Maybe a) -> IO BoardIndex
promptBoardIndex b = do
  putStrLn "Where do you want to move?"
  putStrLn "Enter two digits (0/1/2) separated by a space."
  line <- getLine
  case line of
    [row, ' ', col] ->
      case (charToLineIndex row, charToLineIndex col) of
        (Just i, Just j) ->
          if isNothing (index (i, j) b) then
            pure (i, j)
          else do
            putStrLn "That space is already occupied, try again."
            promptBoardIndex b
        _ -> do
          putStrLn "Invalid input, try again."
          promptBoardIndex b
    _ -> do
      putStrLn "Invalid input, try again."
      promptBoardIndex b

boardSpaceString :: Maybe Player -> String
boardSpaceString Nothing = " "
boardSpaceString (Just X) = "X"
boardSpaceString (Just O) = "O"

printBoard :: Board (Maybe Player) -> IO ()
printBoard b = do
  putStrLn " 0 0 | 0 1 | 0 2"
  putStrLn
    ( "  "    ++ boardSpaceString (index (C0, C0) b) ++
      "  |  " ++ boardSpaceString (index (C0, C1) b) ++
      "  |  " ++ boardSpaceString (index (C0, C2) b)
    )
  putStrLn "     |     |"
  putStrLn "-----------------"
  putStrLn " 1 0 | 1 1 | 1 2"
  putStrLn
    ( "  "    ++ boardSpaceString (index (C1, C0) b) ++
      "  |  " ++ boardSpaceString (index (C1, C1) b) ++
      "  |  " ++ boardSpaceString (index (C1, C2) b)
    )
  putStrLn "     |     |"
  putStrLn "-----------------"
  putStrLn " 2 0 | 2 1 | 2 2"
  putStrLn
    ( "  "    ++ boardSpaceString (index (C2, C0) b) ++
      "  |  " ++ boardSpaceString (index (C2, C1) b) ++
      "  |  " ++ boardSpaceString (index (C2, C2) b)
    )
  putStrLn "     |     |"

playGame :: Player -> Player -> Board (Maybe Player) -> IO ()
playGame humanPlayer currentPlayer b = do
  printBoard b
  if checkIfWon humanPlayer b then
    putStrLn "You win!"
  else if checkIfWon (opponent humanPlayer) b then
    putStrLn "You lose."
  else if allEmptyIndices b == [] then
    putStrLn "Tie game."
  else do
    i <-
      if currentPlayer == humanPlayer then
        promptBoardIndex b
      else do
        putStrLn "The AI is thinking..."
        case findBestMove currentPlayer b of
          Just (i, _) -> do
            putStrLn "The AI makes a move."
            pure i
          Nothing -> error "AI error: impossible state"
    playGame humanPlayer (opponent currentPlayer) (update i (Just currentPlayer) b)

main :: IO ()
main = do
  p <- promptPlayer
  playGame p X (pure Nothing)
