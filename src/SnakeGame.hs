module SnakeGame
  ( Position
  , Direction(..)
  , Game
  , newGame
  , step
  , turn
  , gLost
  , gBody
  , gHead
  , gApple
  , score
  )
  where

import System.Random
import Data.Maybe

type Position = (Int, Int)

data Direction = North | East | South | West
data Game = Game { gDir :: Direction
                 , gHead :: Position
                 , gBody :: [Position]
                 , gBodyLength :: Int
                 , gApples :: [Position]
                 , gLost :: Bool
                 }

newGame :: IO Game
newGame = do
  xGen <- newStdGen
  yGen <- newStdGen

  let appleXs = randomRs (0, 59) xGen
      appleYs = randomRs (0, 59) yGen
      apples = zip appleXs appleYs

  return (Game East (0,0) [] 5 apples False)


step :: Game -> Game
step g | gLost g = g
step g@(Game { gDir = dir
             , gHead = head
             , gBody = body
             , gBodyLength = bodyLength
             , gApples = apples
             , gLost = lost
          }) =
    g { gHead = newHead
      , gBody = take bodyLength $ head : body
      , gApples = remainingApples
      , gBodyLength = if ate then bodyLength + 5 else bodyLength
      , gLost = lost || (newHead `elem` body)
      }

  where newHead = (wrapAround (move dir head))
        (ate, remainingApples) = tryEat head apples

move :: Direction -> Position -> Position
move North (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move South (x, y) = (x, y + 1)
move West (x, y) = (x - 1, y)

gApple :: Game -> Maybe Position
gApple = listToMaybe . gApples

tryEat :: Position -> [Position] -> (Bool, [Position])
tryEat head (apple:rest) | head == apple = (True, rest)
tryEat _ apples = (False, apples)

score :: Game -> Int
score = length . gBody

turn :: Direction -> Game -> Game
turn dir g = g { gDir = dir }

wrapAround :: Position -> Position
wrapAround (x, y) = (x `mod` 60, y `mod` 60)

