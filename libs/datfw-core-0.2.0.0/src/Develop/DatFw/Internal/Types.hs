
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

--
-- @author Jordi Forga
--
module Develop.DatFw.Internal.Types
where
import           Develop.DatFw.Content

import           Network.Wai
import           Network.HTTP.Types as H

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Control.Monad.Trans.Class
import qualified Data.ByteString as B
import           Data.IORef
import           Data.Kind
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable
import           Data.String              -- imports IsString
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import qualified Data.Text.Read
import           Text.Blaze.Html
import           Text.Read hiding (lift)

---------------------------------------------------------------------------------
-- Foundation types

class HasRoute site where
    data Route site :: Type
    parseRoute :: ([Text], [(Text, Text)]) -> Maybe (Route site)
    renderRoute :: Route site -> ([Text], [(Text, Text)])

type UrlRender route = route -> [(Text, Text)] -> Text


type SessionMap = M.Map Text B.ByteString

type SaveSession = SessionMap -> IO [Header]

-- ^ Return the session data and a function to save the session
type SessionBackend = Request -> IO (SessionMap, SaveSession)


-- | Authorization result type.
data AuthzResult = AuthenticationRequired | Authorized | Unauthorized Text

-----------------------------------------------------------------

class PathPiece a where
    toPathPiece :: a -> Text
    fromPathPiece :: Text -> Maybe a

-- | See the documentation for 'readFromPathPiece'.
showToPathPiece :: Show a => a -> Text
showToPathPiece = T.pack . show

-- | A function for helping generate free 'PathPiece'
--   instances for enumeration data types
--   that have derived 'Read' and 'Show' instances.
--   Intended to be used like this:
--
--   > data MyData = Foo | Bar | Baz
--   >   deriving (Read,Show)
--   > instance PathPiece MyData where
--   >   fromPathPiece = readFromPathPiece
--   >   toPathPiece = showToPathPiece
--
readFromPathPiece :: Read a => Text -> Maybe a
readFromPathPiece = readMaybe . T.unpack

parseIntegral :: (Integral a, Bounded a, Ord a) => T.Text -> Maybe a
parseIntegral s = n
    where
    n = case Data.Text.Read.signed Data.Text.Read.decimal s of
        Right (i, "") | i <= top && i >= bot -> Just (fromInteger i)
        _ -> Nothing
    Just witness = n
    top = toInteger (maxBound `asTypeOf` witness)
    bot = toInteger (minBound `asTypeOf` witness)

instance PathPiece Text where
    toPathPiece = id
    fromPathPiece = Just

instance PathPiece String where
    toPathPiece = T.pack
    fromPathPiece = Just . T.unpack

instance PathPiece Int where
    toPathPiece = T.pack . show
    fromPathPiece = parseIntegral

instance PathPiece Bool where
    toPathPiece False = "False"
    toPathPiece True = "True"
    fromPathPiece "False" = Just False
    fromPathPiece "True"  = Just True
    fromPathPiece _       = Nothing

instance (PathPiece a) => PathPiece (Maybe a) where
    fromPathPiece s = case T.stripPrefix "Just " s of
        Just r -> fmap Just $ fromPathPiece r
        _ -> case s of
            "Nothing" -> Just Nothing
            _ -> Nothing
    toPathPiece m = case m of
        Just s -> "Just " <> toPathPiece s
        _ -> "Nothing"


class PathMultiPiece a where
    toPathMultiPiece :: a -> [Text]
    fromPathMultiPiece :: [Text] -> Maybe a

instance PathPiece a => PathMultiPiece [a] where
    toPathMultiPiece = fmap toPathPiece
    fromPathMultiPiece = mapM fromPathPiece


---------------------------------------------------------------------------------
-- Dispatch types

data DispatchEnv site = DispatchEnv
        { envSite :: site                               -- the application
        , envSessionBackend :: Maybe SessionBackend     -- sessionBackend
        -- environment provided by the server ...
        }

---------------------------------------------------------------------------------
-- Handler types

newtype HandlerFor site a = HandlerFor { unHandlerFor :: ReaderT (HandlerData site site) IO a }
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

class MonadIO m => MonadHandler m where
    type HandlerSite (m :: Type -> Type) :: Type
    type SubHandlerSite (m :: Type -> Type) :: Type

    liftHandler :: HandlerFor (HandlerSite m) a -> m a
    liftSubHandler :: SubHandlerFor (SubHandlerSite m) (HandlerSite m) a -> m a

instance MonadHandler (HandlerFor site) where
    type HandlerSite (HandlerFor site) = site
    type SubHandlerSite (HandlerFor site) = site

    liftHandler = id
    liftSubHandler (SubHandlerFor h) = HandlerFor h


data HandlerData sub site = HandlerData
        { handlerEnv :: RunHandlerEnv sub site  -- handler's environment
        , handlerReq :: Request                 -- request
        , handlerStR :: IORef HandlerState      -- state reference
        }

data RunHandlerEnv sub site = RunHandlerEnv
        { rheSite :: site                                       -- master site
        , rheSubSite :: sub                                     -- subsite
        , rheUrlRender :: UrlRender (Route site)                -- urlRender
        , rheRoute :: Maybe (Route site)                        -- site's route
        , rheDefStatus :: Status                                -- defaultStatus
        , rheOnError :: ResponseError -> Request -> SessionMap -> IO HandlerResp -- onError
        }

data HandlerState = HandlerState
        { hsSession :: SessionMap       -- session
        , hsPostQ :: Maybe Query        -- POST parameters
        , hsId :: Int
        }

-- | Responses to indicate some form of an error occurred.
data ResponseError =
      NotFound
    | BadMethod H.Method
    | InvalidArgs [Text]
    | InternalError Text
    | NotAuthenticated
    | PermissionDenied Text

instance Show ResponseError where
    showsPrec _ NotFound  = showString "NotFound"
    showsPrec p (BadMethod x)        = showParen (p > 10) (showString "BadMethod " . showsPrec 11 x)
    showsPrec p (InvalidArgs x)      = showParen (p > 10) (showString "InvalidArgs " . showsPrec 11 x)
    showsPrec p (InternalError x)    = showParen (p > 10) (showString "InternalError " . showsPrec 11 x)
    showsPrec _ NotAuthenticated     = showString "NotAuthenticated"
    showsPrec p (PermissionDenied x) = showParen (p > 10) (showString "PermissionDenied " . showsPrec 11 x)


data HandlerResp =
    Normal
        Status                  -- status;
        [Header]                -- headers;
        ContentType             -- contentType;
        Content                 -- content;
        SessionMap              -- session;
    | Direct
        Response                --  response;

data HandlerContentException =
    HCError
        ResponseError   -- error;
    | HCRedirect
        Status          -- status;
        Text            -- url;
    | HCResponse
        Response        -- response;
    deriving (Typeable)

instance Show HandlerContentException where
    show (HCError e) = "HCError " <> show e
    show (HCRedirect s t) = "HCRedirect " <> show (s, t)
    show (HCResponse _) = "HCResponse"

instance Exception HandlerContentException


---------------------------------------------------------------------------------
-- Widget construction

-- | A function generating an 'Html' given a URL-rendering function.

data WidgetData route = WidgetData
        { wdTitle :: Maybe Html
        , wdHead :: UrlRender route -> Html
        , wdBody :: UrlRender route -> Html
        }

instance Semigroup (WidgetData route) where
    x <> y =
        WidgetData
            { wdTitle = if isJust (wdTitle y) then wdTitle y else wdTitle x
            , wdHead = wdHead x <> wdHead y
            , wdBody = wdBody x <> wdBody y
            }

instance Monoid (WidgetData route) where
    mempty =
        WidgetData { wdTitle = Nothing, wdHead = mempty, wdBody = mempty }

newtype WidgetFor site a = WidgetFor { unWidgetFor :: WriterT (WidgetData (Route site)) (HandlerFor site) a }
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

type Widget site = WidgetFor site ()

instance a ~ () => Semigroup (WidgetFor site a) where
    x <> y = x *> y

instance a ~ () => Monoid (WidgetFor site a) where
    mempty = pure ()

instance MonadHandler (WidgetFor site) where
    type HandlerSite (WidgetFor site) = site
    type SubHandlerSite (WidgetFor site) = site

    liftHandler h = WidgetFor $ lift h
    liftSubHandler (SubHandlerFor h) = WidgetFor $ lift $ HandlerFor h

tellWidgetData :: WidgetData (Route site) -> WidgetFor site ()
tellWidgetData = WidgetFor . tell

tellWidgetBody :: (UrlRender (Route site) -> Html) -> WidgetFor site ()
tellWidgetBody htmlf = tellWidgetData mempty{ wdBody = htmlf }


-- | A 'String' can be trivially promoted to a widget.
instance a ~ () => IsString (WidgetFor site a) where
    fromString = tellWidgetBody . const . toMarkup . T.pack

--------------------------------------------------------------
-- Subsites

newtype SubHandlerFor subsite site a = SubHandlerFor { unSubHandlerFor :: ReaderT (HandlerData subsite site) IO a }
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

instance MonadHandler (SubHandlerFor subsite site) where
    type HandlerSite (SubHandlerFor subsite site) = site
    type SubHandlerSite (SubHandlerFor subsite site) = subsite

    liftHandler (HandlerFor f) = SubHandlerFor $ ReaderT $ \ shd ->
        let srhe = handlerEnv shd
            rhe = srhe{ rheSubSite = rheSite srhe
                      }
        in runReaderT f shd{ handlerEnv = rhe }
    liftSubHandler = id

