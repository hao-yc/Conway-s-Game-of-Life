
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveFunctor              #-}

module Drawing.Activity.Internal.Event
where
import           Drawing

import           Miso

import qualified Data.Aeson as Json
import qualified Data.Map as M
import qualified Data.Text as T

-- ---------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------

data Msg action
    = MNoOp
    | MMouseIn (Maybe MouseInfo)  -- if mouse is in the drawing
                                  --   then it is 'Just (offsetCoords, clientCoords)'
    | MMouseDownUp MouseInfo (Point -> action)
                                  -- True/false iff mouse down/up, (offsetCoords, clientCoords)
    | MMouseMov !(Int, Int)       -- clientCoords
    | MAction action
    deriving Functor

instance Show (Msg action) where
    show MNoOp = "MNoOp"
    show (MMouseIn mbinfo)     = "(MMouseIn " ++ show mbinfo ++ ")"
    show (MMouseDownUp info _) = "(MMouseDownUp " ++ show info ++ " ?)"
    show (MMouseMov client)    = "(MMouseMov " ++ show client ++ ")"
    show (MAction _)           = "(MAction ?)"

data MouseInfo = MouseInfo{ offset, client :: !(Int, Int) }
    deriving (Show)


data ViewProp action
    = ViewProp (Attribute (Msg action))
    | PropOnMouseMove (Point -> action)

instance Functor ViewProp where
    fmap f (ViewProp attr) = ViewProp $ fmap (fmap f) attr

onKeyDown :: (T.Text -> action) -> ViewProp action
onKeyDown f = onKeyDown' (Just . f)

onKeyDown' :: (T.Text -> Maybe action) -> ViewProp action
onKeyDown' f = ViewProp $
    on "keydown" keyInfoDecoder $ maybe MNoOp MAction . f . keyText . keyCode

onKeyUp :: (T.Text -> action) -> ViewProp action
onKeyUp f = onKeyUp' (Just . f)

onKeyUp' :: (T.Text -> Maybe action) -> ViewProp action
onKeyUp' f = ViewProp $
    on "keyup" keyInfoDecoder $ maybe MNoOp MAction . f . keyText . keyCode

keyText :: KeyCode -> T.Text
keyText (KeyCode k) =
    case M.lookup k table of
        Just t -> t
        Nothing -> T.pack [ toEnum k ]
  where
    table = M.fromList
      [ ( 187, "+" )
      , ( 189, "-" )
      , ( 38, "ARROWUP" )
      , ( 40, "ARROWDOWN" )
      , ( 39, "ARROWRIGHT" )
      , ( 37, "ARROWLEFT" )
      ]

onMouseDown :: (Point -> action) -> ViewProp action
onMouseDown f = ViewProp $
    on "mousedown" mouseInfoDecoder $ \info -> MMouseDownUp info f

onMouseUp :: (Point -> action) -> ViewProp action
onMouseUp f = ViewProp $
    on "mouseup" mouseInfoDecoder $ \info -> MMouseDownUp info f

onMouseMove :: (Point -> action) -> ViewProp action
onMouseMove f = PropOnMouseMove f

-----------------------------------------------------------------

mouseInfoDecoder :: Decoder MouseInfo
mouseInfoDecoder = [] `at` decoder
    where
        pairXY o name = (,) <$> (o Json..: (name<>"X")) <*> (o Json..: (name<>"Y"))
        decoder = Json.withObject "MouseEvent" $ \ o -> do
            offset <- pairXY o "offset"
            client <- pairXY o "client"
            pure MouseInfo{ offset = offset, client = client }

