
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where
import Drawing
import Drawing.IO
import Drawing.Image

import Data.Text (Text, pack)

import System.Environment

main :: IO ()
main = do
    [contentType, fileName] <- getArgs
    drw' <- imageDraw contentType fileName
    writeSvgFile "test2.svg" $
        translated (-8) 0 imageAC <> drw' <> translated 8 0 imageAC

ratio :: Double
ratio = 0.7

frame :: Drawing
frame = colored gray (solidRectangle w h) <> rectangle w h
    where h = 8
          w = ratio * h

imageDraw :: String -> String -> IO Drawing
imageDraw contentType fileName = do
    uri <- dataUriFromFile (pack contentType) fileName
    pure $ image uri w h <> colored red (rectangle w h)
    where w = 5.2
          h = 8

imageAC :: Drawing
imageAC = $(embedAsImage "image/png" "test/AC.png" 5.2 8)
