
module Drawing.IO where
import           Drawing.Types
import           Drawing.Render

import qualified Data.Text.Lazy.IO as TL
import qualified System.IO as IO

-- | Write a 'Drawing' to a handle.
hPutSvg :: IO.Handle -> Drawing -> IO ()
hPutSvg h draw = do
    IO.hPutStrLn h "<?xml version='1.0' encoding='UTF-8' standalone='no'?>"
    IO.hPutStrLn h "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
    TL.hPutStr h $ snd $ renderSvgText 300 300 draw

-- | Write a 'Drawing' to 'stdout'.
putSvg :: Drawing -> IO ()
putSvg = hPutSvg IO.stdout

{-# DEPRECATED svgOf "Use 'putSvg' instead" #-}
svgOf :: Drawing -> IO ()
svgOf = putSvg

-- | Write a 'Drawing' to a file.
writeSvgFile :: FilePath -> Drawing -> IO ()
writeSvgFile p = IO.withFile p IO.WriteMode . flip hPutSvg

