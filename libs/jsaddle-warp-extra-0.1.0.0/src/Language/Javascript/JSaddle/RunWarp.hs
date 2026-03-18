
{-# LANGUAGE OverloadedStrings          #-}

module Language.Javascript.JSaddle.RunWarp
    ( run
    )
where
import           Language.Javascript.JSaddle.Types (JSM, syncPoint)
import qualified Language.Javascript.JSaddle.WebSockets as JS
import qualified Network.WebSockets as W
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import qualified Network.HTTP.Types as W
import qualified Data.ByteString as B
import qualified Data.Binary.Builder as B
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IORef as IO
import           Control.Monad (when)
import qualified System.IO as IO


run :: Int -> JSM () -> IO ()
run port f = do
    app <- staticApp
    W.runSettings (W.setPort port (W.setTimeout 3600 W.defaultSettings)) =<<
        JS.jsaddleWithAppOr W.defaultConnectionOptions (f >> syncPoint) app

withCache :: Bool
withCache = False

staticApp :: IO W.Application
staticApp = do
    cacheRef <- IO.newIORef M.empty
    pure $ handleRequest cacheRef
    where
        handleRequest :: IO.IORef (M.Map String B.Builder) -> W.Application
        handleRequest cacheRef req sendResponse =
            case (W.requestMethod req, W.pathInfo req) of
                ("GET", segments) | Just (dir, name) <- toPath segments -> do
                    let filepath = T.unpack (dir <> name)
                    cache <- IO.readIORef cacheRef
                    content <- case M.lookup filepath cache of
                        Just content -> pure content
                        Nothing -> do
                            content <- B.putStringUtf8 <$> IO.readFile filepath
                            when withCache $
                                IO.writeIORef cacheRef $ M.insert filepath content cache
                            pure content
                    let mime = getMimeType name
                    sendResponse $ W.responseBuilder W.status200 [("Content-Type", mime)] content
                _ ->
                    sendResponse $ W.responseLBS W.status403 [("Content-Type", "text/plain")] "Forbidden"
        toPath :: [Text] -> Maybe (Text, Text)
        toPath [] = Nothing
        toPath [segment] = Just ("", segment)
        toPath (segment : segments) = do
            (dir, name) <- toPath segments
            pure (segment <> "/" <> dir, name)

getMimeType :: Text -> B.ByteString
getMimeType name =
    let suffix = T.dropWhile (/= '.') name
    in maybe "text/plain" id $ M.lookup suffix mimeTypes

mimeTypes :: M.Map Text B.ByteString
mimeTypes = M.fromList
    [ (".html", "text/html;charset=UTF-8")
    , (".css", "text/css;charset=UTF-8")
    , (".js", "application/javascript")
    ]

