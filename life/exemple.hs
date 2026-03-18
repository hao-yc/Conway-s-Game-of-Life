
module Main where
import Drawing
import Drawing.Activity

main :: IO ()
main = activityOf 3708 60 30 initial handle view

initial = 0

handle :: () -> State Double ()
handle () =
    modify $ \angle -> angle + pi / 4

view :: Double -> View ()
view angle =
    drawingView [ onKeyDown (const ()) ] draw
    where
        draw = rotated angle $
            polyline [(0, 0), (2, 0)]
              <> translated 2 0 (colored red $ solidCircle 0.5)

