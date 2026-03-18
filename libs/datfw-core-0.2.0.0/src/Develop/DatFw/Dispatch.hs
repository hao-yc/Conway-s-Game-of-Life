
{-# LANGUAGE OverloadedStrings #-}
--
-- @author Jordi Forga
--
module Develop.DatFw.Dispatch
    ( DispatchEnv(..)
    , toApp, toAppWithEnv
    -- * Routing combinators
    , selectMethod, selectMethodOverride
    , onMethod, onAnyMethod
    -- * Subsites
    , subhandler
    )
where
import Develop.DatFw.Content
import Develop.DatFw.Handler (notFound)
import Develop.DatFw.Internal.Types
import Develop.DatFw.Internal.Dispatch
import Develop.DatFw.Internal.Routing
import Develop.DatFw.Internal.Classes

import           Network.Wai
import qualified Network.HTTP.Types as H

import           Data.Monoid
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL

--------------- Convert to WAI (Http Application)  ---------------------

-- | Convert the given argument into a WEB application, executable with any compatible server.
toApp :: (WebApp site, ToTypedContent a)
      => site -> (Route site -> HandlerFor site a)
      -> IO Application
toApp site handler = do
    sessionBackend <- makeSessionBackend site
    let env = DispatchEnv site sessionBackend
    pure $ toAppWithEnv env handler

-- | Pure low level function to construct WEB application.
--
-- Useful when you need not standard way to run your app, or want to embed it
-- inside another app.
toAppWithEnv :: (WebApp site, ToTypedContent a)
             => DispatchEnv site -> (Route site -> HandlerFor site a)
             -> Application
toAppWithEnv env handler request respond =
    case cleanPath site (pathInfo request) of
        Left path -> sendRedirect site path request respond
        Right path -> dispatchSite env handler (request { pathInfo = path }) respond
  where
        site = envSite env

        sendRedirect :: WebApp a => a -> [Text] -> Application
        sendRedirect app segments req respond = do
            let ar = appRoot app req
                -- Ensure that non-GET requests get redirected correctly.
                status = if requestMethod req == H.methodGet
                         then H.movedPermanently301 else H.temporaryRedirect307
                rqs = rawQueryString req
                dest = joinPath app ar segments []
                        <> (if B.null rqs then mempty else B.byteString rqs)
            respond (responseBuilder status [("Location", BL.toStrict (B.toLazyByteString dest))] mempty)

        dispatchSite :: (WebApp site, ToTypedContent r)
                     => DispatchEnv site -> (Route site -> HandlerFor site r)
                     -> Application
        dispatchSite env handle req = case parseRoute (pathInfo req, decodedQuery) of
                Just r  -> dispatchHandler (handle r) env (Just r) req
                Nothing -> dispatchHandler (notFound :: HandlerFor site ()) env Nothing req
            where
                decodedQuery :: [(Text, Text)]
                decodedQuery =
                    let query = queryString req
                        decode (n, mbv) = (T.decodeUtf8 n, maybe T.empty T.decodeUtf8 mbv)
                    in decode <$> query

