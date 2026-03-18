
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw

import Drawing
import Drawing.Activity
import Drawing.Vector

import qualified Data.Text as T
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as BL

data GridMode = NoGrid | LivesGrid | ViewGrid
  deriving (Eq, Show)

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        , gmGridMode :: GridMode 
        , gmZoom :: Double, gmShift :: Point
        , gmShowHelp :: Bool
        }

-----------------------------------------------------
-- Initialization

viewWidth, viewHeight :: Double
viewWidth = 60.0
viewHeight = 30.0

main =
    activityOfIO 3708 viewWidth viewHeight initial update view

board0Cells =
    [(-5, 0), (-4, 0), (-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]

initial = Game
    { gmBoard = foldr (setCell True) initBoard board0Cells
    , gmGridMode = NoGrid
    , gmZoom = 1.0, gmShift = (0.0, 0.0)
    , gmShowHelp = True
    }

-----------------------------------------------------
-- Update state

data Action =
          NextStep
        | SetCell Point
        | ChangeGridMode
        | ZoomOut
        | ZoomIn
        | AddShift (Double, Double)
        | ToggleHelp
        | StartLoad
        | EndLoad Board
        | Save
        deriving (Eq, Show)

update :: Action -> UpdateM Game Action ()
update NextStep =               -- Next generation
    modify $
        \game -> game{ gmBoard = nextGeneration (gmBoard game) }

update (SetCell point) =  do    -- Set live/dead cells
    game <- get
    let pos = pointToPos game point
        brd = gmBoard game
    put game{ gmBoard = setCell (not $ cellIsLive pos brd) pos brd }

update ChangeGridMode = do      -- Canvia el mode de graella
    game <- get
    let newMode = case gmGridMode game of
            NoGrid    -> LivesGrid
            LivesGrid -> ViewGrid
            ViewGrid  -> NoGrid
    put game{ gmGridMode = newMode }

update ZoomIn = do
    game <- get
    if gmZoom game < 2.0
        then put game{ gmZoom = gmZoom game * 2.0 }
        else return ()

update ZoomOut = do
    game <- get
    if gmZoom game > 0.125
        then put game{ gmZoom = gmZoom game / 2.0 }
        else return ()

update (AddShift v) = do        -- Afegir desplaçament (pan)
    game <- get
    put game{ gmShift = gmShift game ^+^ v }

update ToggleHelp =
    modify $
        \game -> game{ gmShowHelp = not $ gmShowHelp game }


update Save = do
    game <- get
    deferIO_ $ BL.writeFile "board.json" (encode (gmBoard game))

update StartLoad =
    deferSub $ \sink -> do
        contents <- BL.readFile "board.json"
        case eitherDecode contents of
            Right board -> sink (EndLoad board)
            Left _      -> return ()

update (EndLoad board) =
    modify $ \game -> game{ gmBoard = board}



pointToPos :: Game -> Point -> Pos
pointToPos game p =
    let (gx, gy) = (1.0 / gmZoom game) *^ p ^-^ gmShift game
    in (round gx, round gy)

-----------------------------------------------------
-- View state

view :: Game -> View Action
view game =
    drawingView
        [ onKeyDown' keyMap
        , onMouseDown SetCell
        ]
        $ draw game

keyMap :: T.Text -> Maybe Action
keyMap "N" = Just NextStep
keyMap "G" = Just ChangeGridMode
keyMap "O"           = Just ZoomOut
keyMap "I"           = Just ZoomIn
keyMap "ARROWUP"     = Just $ AddShift (0, 1)
keyMap "ARROWDOWN"   = Just $ AddShift (0,  -1)
keyMap "ARROWLEFT"   = Just $ AddShift (-1,  0)
keyMap "ARROWRIGHT"  = Just $ AddShift (1, 0)
keyMap "H" = Just ToggleHelp
keyMap "S" = Just Save
keyMap "L" = Just StartLoad
keyMap _   = Nothing

draw :: Game -> Drawing
draw game =
    let boardPart = drawBoard (gmBoard game)
        gridPart = case gmGridMode game of
                     NoGrid    -> blank
                     LivesGrid -> drawGrid (minLiveCell (gmBoard game)) (maxLiveCell (gmBoard game))
                     ViewGrid  -> drawGrid (pointToPos game (-viewWidth/2, -viewHeight/2))
                                           (pointToPos game ( viewWidth/2,  viewHeight/2))
        combined = boardPart <> gridPart
        (dx, dy) = gmShift game
        scaledDrawing = scaled (gmZoom game) (gmZoom game) combined

        helpLine n t = translated (-viewWidth/2 + 6) (viewHeight/2 - 2 - n * 1.5)
                       $ colored blue
                       $ scaled 1 1
                       $ text (T.pack t)

        helpText = if gmShowHelp game
                   then helpLine 0 "H:          Toggle help"
                     <> helpLine 1 "N:          Next step"
                     <> helpLine 2 "G:          Change grid mode"
                     <> helpLine 3 "O:          Zoom out"
                     <> helpLine 4 "I:          Zoom in"
                     <> helpLine 5 "ARROWUP:    Shift down"
                     <> helpLine 6 "ARROWDOWN:  Shift up"
                     <> helpLine 7 "ARROWRIGHT: Shift left"
                     <> helpLine 8 "ARROWLEFT:  Shift right"
                     <> helpLine 9 "Mouse: Set  live/dead cell"
                     <> helpLine 10 "S:          Save board"
                     <> helpLine 11 "L:          Load board"
                   else blank

    in translated dx dy scaledDrawing <> helpText