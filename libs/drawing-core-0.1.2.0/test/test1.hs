
module Main where
import Drawing
import Drawing.IO

main :: IO ()
main = do
    let drw' = coordinatePlane <> myDrawing
    writeSvgFile "test1.svg" drw'


lightBulb :: Color -> Double ->  Drawing
lightBulb c dy = colored c (translated 0 dy (solidCircle 1))

frame :: Drawing
frame = colored gray (solidRectangle 2.5 7.5) <> rectangle 2.5 7.5

trafficLight :: Drawing
trafficLight =
  frame <>
  lightBulb red 2.5 <>
  lightBulb yellow 0   <>
  lightBulb green (-2.5)

trafficLights :: [(Double, Double)] -> Drawing
trafficLights ps = foldMap (flip (uncurry translated) trafficLight) ps

myDrawing :: Drawing
myDrawing =
  trafficLights [(x, y) | x <- [-3,0,3], y <- [-8,0,8] ]

