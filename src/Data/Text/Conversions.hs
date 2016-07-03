{-# LANGUAGE DeriveFunctor #-}

{-|
  Module: Data.Text.Conversions

  This module provides a set of typeclasses for safely converting between
  textual data. The built-in 'String' type, as well as strict 'Data.Text.Text'
  and lazy 'Data.Text.Lazy.Text', are safely convertible between one another.
  The 'Data.ByteString.ByteString' type is frequently treated in much the same
  manner, but this is unsafe for two reasons:

  * Since 'Data.ByteString.ByteString' encodes binary data, it does not specify
    a particular encoding, so assuming a particular encoding like UTF-8 would
    be incorrect.

  * Furthermore, decoding binary data into text given a particular encoding can
    fail. Most systems simply use 'Data.Text.Encoding.decodeUtf8' and similar
    functions, which will dangerously throw exceptions when given invalid data.

  This module addresses both problems by providing a 'DecodeText' typeclass for
  decoding binary data in a way that can fail and by providing a 'UTF8' wrapper
  type for selecting the desired encoding.

  Most of the time, you will not need to create your own instances or use the
  underlying functions that make the conversion machinery tick. Instead, just
  use the 'convertText' method to convert between two textual datatypes or the
  'decodeConvertText' method to perform a conversion that can fail.

  Examples:

  >>> convertText ("hello" :: String) :: Text
  "hello"
  >>> decodeConvertText (UTF8 ("hello" :: ByteString)) :: Maybe Text
  Just "hello"
  >>> decodeConvertText (UTF8 ("\xc3\x28" :: ByteString)) :: Maybe Text
  Nothing
-}
module Data.Text.Conversions
  ( Base16(..)
  , Base64(..)
  , DecodeText(..)
  , FromText(..)
  , ToText(..)
  , UTF8(..)
  , convertText
  , decodeConvertText
  ) where

import Control.Error.Util (hush)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base16.Lazy as Base16L
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.Lazy as Base64L

{-|
  Simple wrapper type that is used to select a desired encoding when encoding or
  decoding text from binary data, such as 'Data.ByteString.ByteString's.
-}
newtype UTF8 a = UTF8 { unUTF8 :: a }
  deriving (Eq, Show, Functor)

{-|
  Wrapper type used to select a base 16 encoding when encoding or decoding
  binary data. Safe because base 16 encoding will always produce ASCII output.
-}
newtype Base16 a = Base16 { unBase16 :: a }
  deriving (Eq, Show, Functor)

{-|
  Wrapper type used to select a base 64 encoding when encoding or decoding
  binary data. Safe because base 64 encoding will always produce ASCII output.
-}
newtype Base64 a = Base64 { unBase64 :: a }
  deriving (Eq, Show, Functor)

{-|
  A simple typeclass that handles converting arbitrary datatypes to
  'Data.Text.Text' when the operation cannot fail. If you have a type that
  satisfies that requirement, implement this typeclass, but if the operation can
  fail, use 'DecodeText' instead.
-}
class ToText a where
  toText :: a -> T.Text

{-|
  A simple typeclass that handles converting 'Data.Text.Text' to arbitrary
  datatypes. If you have a type that can be produced from text, implement this
  typeclass, /not/ 'ConvertText'. However, you probably do not want to call
  'fromText' directly; call 'convertText', instead.
-}
class FromText a where
  fromText :: T.Text -> a

{-|
  A simple typeclass that handles converting arbitrary datatypes to
  'Data.Text.Text' when the operation can fail. If you have a type that
  satisfies that requirement, implement this typeclass, but if the operation
  cannot fail, use 'ToText' instead.
-}
class Functor f => DecodeText f a where
  decodeText :: a -> f T.Text

{-|
  A function that provides a way to /safely/ convert between arbitrary textual
  datatypes where the conversion to text cannot fail.

  >>> convertText ("hello" :: String) :: Text
  "hello"
-}
convertText :: (ToText a, FromText b) => a -> b
convertText = fromText . toText


{-|
  A function that provides a way to /safely/ convert between arbitrary textual
  datatypes where the conversion to text can fail, such as decoding binary data
  to text. Since binary data can represent text in many different potential
  encodings, it is necessary to use a newtype that picks the particular
  encoding, like 'UTF8':

  >>> decodeConvertText (UTF8 ("hello" :: ByteString)) :: Maybe Text
  Just "hello"
-}
decodeConvertText :: (DecodeText f a, FromText b) => a -> f b
decodeConvertText = fmap fromText . decodeText

instance ToText   T.Text  where toText   = id
instance FromText T.Text  where fromText = id
instance ToText   String  where toText   = T.pack
instance FromText String  where fromText = T.unpack
instance ToText   TL.Text where toText   = TL.toStrict
instance FromText TL.Text where fromText = TL.fromStrict

instance DecodeText Maybe (UTF8 B.ByteString)  where decodeText = hush . T.decodeUtf8' . unUTF8
instance FromText         (UTF8 B.ByteString)  where fromText   = UTF8 . T.encodeUtf8
instance DecodeText Maybe (UTF8 BL.ByteString) where decodeText = hush . fmap TL.toStrict . TL.decodeUtf8' . unUTF8
instance FromText         (UTF8 BL.ByteString) where fromText   = UTF8 . TL.encodeUtf8 . TL.fromStrict

instance ToText (Base16 B.ByteString) where
  toText = T.decodeUtf8 . Base16.encode . unBase16
instance FromText (Maybe (Base16 B.ByteString)) where
  fromText txt = case Base16.decode (T.encodeUtf8 txt) of
    (bs, "") -> Just $ Base16 bs
    (_,  _)  -> Nothing

instance ToText (Base64 B.ByteString) where
  toText = T.decodeUtf8 . Base64.encode . unBase64
instance FromText (Maybe (Base64 B.ByteString)) where
  fromText = fmap Base64 . hush . Base64.decode . T.encodeUtf8

instance ToText (Base16 BL.ByteString) where
  toText = TL.toStrict . TL.decodeUtf8 . Base16L.encode . unBase16
instance FromText (Maybe (Base16 BL.ByteString)) where
  fromText txt = case Base16L.decode (TL.encodeUtf8 $ TL.fromStrict txt) of
    (bs, "") -> Just $ Base16 bs
    (_,  _)  -> Nothing

instance ToText (Base64 BL.ByteString) where
  toText = TL.toStrict . TL.decodeUtf8 . Base64L.encode . unBase64
instance FromText (Maybe (Base64 BL.ByteString)) where
  fromText = fmap Base64 . hush . Base64L.decode . TL.encodeUtf8 . TL.fromStrict
