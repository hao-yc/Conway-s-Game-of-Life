
{-# LANGUAGE OverloadedStrings #-}
--
-- @author Jordi Forga
--
module Develop.DatFw.Content
    ( module Develop.DatFw.Content
    , Html
    )
where
import           Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8

import           Data.Text(Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.Aeson as J

--------------------- Types ------------------------

data Content =
      ContentBuilder
          Builder       -- byte-string builder
          (Maybe Int)   -- content length (possibly unknown)
    --- | ContentSource (OutputStream -> IO ()) -- writeTo

-- | Zero-length content.
emptyContent :: Content
emptyContent = ContentBuilder mempty (Just 0)


data TypedContent = TypedContent ContentType Content

type ContentType = B.ByteString

typePlain, typeHtml, typeJson :: ContentType
typePlain = "text/plain; charset=utf-8"
typeHtml = "text/html; charset=utf-8"
typeJson = "application/json"


--------------------- Type classes ------------------------

class ToContent a where
    toContent :: a -> Content

class ToContent a => ToTypedContent a where
    toTypedContent :: a -> TypedContent

    default toTypedContent :: HasContentType a => a -> TypedContent
    toTypedContent x = TypedContent (getContentType $ Just x) (toContent x)

class ToTypedContent a => HasContentType a where
    getContentType :: Monad m => m a -> ContentType


--------------------- ToContent instances ------------------------

instance ToContent Content where
    toContent c = c

instance ToContent TypedContent where
    toContent (TypedContent t c) = c

instance ToContent () where
    toContent () = ContentBuilder mempty (Just 0)

instance ToContent [Char] where
    toContent text =  ContentBuilder (stringUtf8 text) (Just (length text))

instance ToContent Text where
    toContent text =  ContentBuilder (encodeUtf8Builder text) (Just (T.length text))

instance ToContent Html where
    toContent bs = ContentBuilder (renderHtmlBuilder bs) Nothing

instance ToContent J.Value where
    toContent = toContent . J.toEncoding

instance ToContent J.Encoding where
    toContent j = ContentBuilder (J.fromEncoding j) Nothing

--------------------- ToTypedContent instances ------------------------

instance ToTypedContent TypedContent where
    toTypedContent tc = tc

instance ToTypedContent () where
    toTypedContent () = TypedContent typePlain (toContent ())

instance ToTypedContent [Char] where
    toTypedContent text = TypedContent typePlain (toContent text)

instance ToTypedContent Text where
instance HasContentType Text where
    getContentType _ = typePlain

instance ToTypedContent Html where
instance HasContentType Html where
    getContentType _ = typeHtml

instance ToTypedContent J.Value where
instance HasContentType J.Value where
    getContentType _ = typeJson

instance ToTypedContent J.Encoding where
instance HasContentType J.Encoding where
    getContentType _ = typeJson

