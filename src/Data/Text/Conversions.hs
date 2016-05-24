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
  use the 'convertText' method to automatically convert between two textual
  datatypes automatically, whatever they may be.

  Examples:

  >>> convertText ("hello" :: String) :: Text
  "hello"
  >>> convertText (UTF8 ("hello" :: ByteString)) :: Maybe Text
  Just "hello"
  >>> convertText (UTF8 ("\xc3\x28" :: ByteString)) :: Maybe Text
  Nothing
-}
module Data.Text.Conversions
  ( ConvertText(..)
  , DecodeText(..)
  , FromText(..)
  , ToText(..)
  , UTF8(..)
  ) where

import Control.Error.Util (hush)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

{-|
  Simple wrapper type that is used to select a desired encoding when encoding or
  decoding text from binary data, such as 'Data.ByteString.ByteString's.
-}
newtype UTF8 a = UTF8 { unUTF8 :: a }
  deriving (Eq, Show, Functor)

{-|
  A simple typeclass that handles converting arbitrary datatypes to
  'Data.Text.Text' when the operation cannot fail. If you have a type that
  satisfies that requirement, implement this typeclass, /not/ 'ConvertText'.
  However, you probably do not want to call 'toText' directly; call
  'convertText', instead.
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
  'Data.Text.Text' when the operation can fail (the functor in question is
  expected to be 'Data.Maybe.Maybe' or 'Data.Either.Either'). If you have a type
  that satisfies that requirement, implement this typeclass, /not/
  'ConvertText'. However, you probably do not want to call 'decodeText'
  directly; call 'convertText', instead.
-}
class Functor f => DecodeText f a where
  decodeText :: a -> f T.Text

{-|
  A typeclass that provides a way to /safely/ convert between arbitrary textual
  datatypes, including conversions that can potentially fail.
  __Do not implement this typeclass directly__, implement 'ToText', 'FromText',
  or 'DecodeText', instead, which this typeclass defers to. Use the
  'convertText' function to actually perform conversions.

  At a basic level, 'convertText' can convert between textual types, like
  between 'String' and 'Data.Text.Text':

  >>> convertText ("hello" :: String) :: Text
  "hello"

  More interestingly, 'convertText' can also convert between binary data and
  textual data in the form of 'Data.ByteString.ByteString'. Since binary data
  can represent text in many different potential encodings, it is necessary to
  use a newtype that picks the particular encoding, like 'UTF8':

  >>> convertText (UTF8 ("hello" :: ByteString)) :: Maybe Text
  Just "hello"

  Note that the result of converting a 'Data.ByteString.ByteString' is a 'Maybe'
  'Data.Text.Text' since the decoding can fail.
-}
class ConvertText a b where
  convertText :: a -> b

instance (ToText a, FromText b) => ConvertText a b where
  convertText = fromText . toText

instance {-# OVERLAPPING #-} (DecodeText Maybe a, FromText b) => ConvertText a (Maybe b) where
  convertText = fmap fromText . decodeText

instance {-# OVERLAPPING #-} (DecodeText (Either e) a, FromText b) => ConvertText a (Either e b) where
  convertText = fmap fromText . decodeText

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
