
module Develop.DatFw.Internal.Handler
where
import Develop.DatFw.Internal.Types

import           Network.Wai
import           Network.HTTP.Types as H

import           Data.Monoid
import qualified Data.ByteString as B
import           Data.IORef
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader

lookupPostQuery :: MonadHandler m => m Query
lookupPostQuery = liftHandler $ do -- HandlerFor (HandlerSite m)
    st <- get
    case hsPostQ st of
        Just q -> pure q
        Nothing -> do
            hdata <- HandlerFor ask
            q <- liftIO $ getPostQuery (handlerReq hdata)
            put st{ hsPostQ = Just q }
            pure q
  where
        getPostQuery :: Request -> IO Query
        getPostQuery req =
            parseQuery <$> getBody req
        getBody req = do
            b <- getRequestBodyChunk req
            if B.null b then pure B.empty
            else do
                bs <- getBody req
                pure $ b <> bs

--------------------------------------------------------------
-- Internal

asksEnv :: MonadHandler m => (RunHandlerEnv (HandlerSite m) (HandlerSite m) -> a) -> m a
asksEnv f = liftHandler $ HandlerFor $ asks $ f . handlerEnv

get :: MonadHandler m => m HandlerState
get = liftHandler $ HandlerFor $ asks handlerStR >>= liftIO . readIORef

put :: MonadHandler m => HandlerState -> m ()
put x = liftHandler $ HandlerFor $ asks handlerStR >>= \str -> liftIO (writeIORef str x)

modify :: MonadHandler m => (HandlerState -> HandlerState) -> m ()
modify f = liftHandler $ HandlerFor $ asks handlerStR >>= \str -> liftIO (modifyIORef str f)


modSession :: (SessionMap -> SessionMap) -> HandlerState -> HandlerState
modSession f st = st{ hsSession = f (hsSession st) }


