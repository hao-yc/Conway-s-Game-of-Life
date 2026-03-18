
module Develop.DatFw.Internal.Routing
where
import Develop.DatFw.Internal.Types
import Develop.DatFw.Handler
import Develop.DatFw.Content

import Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (Method)
import Network.Wai


selectMethod :: MonadHandler m => [(Method, m r)] -> m r
selectMethod = selectMethod_aux getMethod

selectMethodOverride :: MonadHandler m => [(Method, m r)] -> m r
selectMethodOverride = selectMethod_aux getMethodOverride

selectMethod_aux :: MonadHandler m => m (Method, m r) -> [(Method, m r)] -> m r
selectMethod_aux getmeth hlist = do
    let mmap = M.fromList hlist
        methmbf m = case M.lookup m mmap of
            Nothing -> M.lookup anyMethod mmap
            Just h  -> Just h
    (meth, badmeth) <- getmeth
    maybe badmeth id $ methmbf meth

getMethod :: MonadHandler m => m (Method, m a)
getMethod = do
    method <- requestMethod <$> getRequest
    pure (method, badMethod)

getMethodOverride :: MonadHandler m => m (Method, m a)
getMethodOverride = do
    method <- requestMethod <$> getRequest
    if method == B8.pack "POST"
        then maybe (method, badMethod) (\m -> (m, badMethodOverride m)) <$> lookupMethParam
        else pure (method, badMethod)
    where
        methParamName = T.pack "_method"
        lookupMethParam = do
            mbvalue1 <- lookupGetParam methParamName
            mbvalue2 <- maybe (lookupPostParam methParamName) (pure . Just) mbvalue1
            pure (T.encodeUtf8 <$> mbvalue2)
        badMethodOverride m = invalidArgs [T.pack "Bad Overriden Method: " <> T.decodeUtf8 m]

onMethod :: (Functor f, ToTypedContent o) => Method -> f o -> (Method, f TypedContent)
onMethod m h = (m, fmap toTypedContent h)

onAnyMethod :: (Functor f, ToTypedContent o) => f o -> (Method, f TypedContent)
onAnyMethod h = (anyMethod, fmap toTypedContent h)

anyMethod :: Method
anyMethod = B8.pack ""

-- ****************************************************************

subhandler :: (site -> sub)
           -> SubHandlerFor sub site a
           -> HandlerFor site a
subhandler toSub handler =
    let dataToSub hd = hd{ handlerEnv = srhe }
            where
                rhe = handlerEnv hd
                srhe = rhe { rheSubSite = toSub (rheSite rhe)
                           }
    in HandlerFor $ withReaderT dataToSub $ unSubHandlerFor handler

