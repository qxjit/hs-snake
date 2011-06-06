module SnakeGUI where

import SnakeGame

import Graphics.UI.WX
import Control.Monad as Monad

snakeGUI :: IO ()
snakeGUI = start $ do
  game <- newGame
  state <- variable [value := game]
  f <- frame [ text := "Snake"
             , layout := space 600 600
             , on paint := drawGame state
             , on downKey := updateGame (turn South) state
             , on upKey := updateGame (turn North) state
             , on leftKey := updateGame (turn West) state
             , on rightKey := updateGame (turn East) state
             ]

  timer f [ interval := 100
          , on command := advance state f
          ]

  return ()


updateGame :: (Game -> Game) -> Var Game -> IO ()
updateGame f state = do
  varUpdate state f
  return ()

headColor :: Color
headColor = rgb 100 220 100

bodyColor :: Color
bodyColor = rgb 0 128 0

appleColor :: Color
appleColor = rgb 240 30 30

bgColor :: Game -> Color
bgColor g | gLost g = rgb 255 200 200
bgColor _ = rgb 220 220 255

drawGame :: Var Game -> DC () -> Rect -> IO ()
drawGame state ctx _ = do
  game <- varGet state
  fillRect ctx (rect (pt 0 0) (sz 600 600)) (bgColor game)
  mapM_ (drawCell ctx bodyColor) (gBody game)
  drawCell ctx headColor (gHead game)

  maybe (return ()) (drawCell ctx appleColor) (gApple game)

drawCell :: DC () -> Color -> Position -> IO ()
drawCell ctx color (x, y) = do
  let (x', y') = (x*10, y*10)
  fillRect ctx (rect (pt x' y') (sz 10 10)) color

fillRect :: DC () -> Rect -> Color -> IO ()
fillRect ctx r color =
  drawRect ctx r [ brushColor := color
                 , brushKind := BrushSolid
                 , penKind := PenTransparent
                 ]

advance :: Var Game -> Frame () -> IO ()
advance state frame = do
  game <- varUpdate state step
  Monad.when (gLost game) $ do
    game' <- newGame
    infoDialog frame
               "You have lost"
               ("You ran the snake into itself and lost. Total score: " ++ (show $ score game))
    varSet state game'
  repaint frame
