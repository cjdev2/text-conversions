module Data.Text.ConversionsSpec (spec) where

import Test.Hspec
import Data.Text.Conversions

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

newtype Upper = Upper T.Text deriving (Eq, Show)
newtype Lower = Lower T.Text deriving (Eq, Show)

instance ToText Upper where toText (Upper txt) = txt
instance FromText Lower where fromText = Lower . T.toLower

data FailableString = FailableString
  deriving (Eq, Show)

instance DecodeText Maybe FailableString where
  decodeText _ = Just "failable"

data FailableRepresentation = FailableRepresentation
  deriving (Eq, Show)

instance FromText (Maybe FailableRepresentation) where
  fromText _ = Just FailableRepresentation

spec :: Spec
spec = do
  describe "convertText" $ do
    it "can convert strings to and from text" $ do
      convertText ("hello" :: String) `shouldBe` ("hello" :: T.Text)
      convertText ("hello" :: T.Text) `shouldBe` ("hello" :: String)

    it "converts between strict and lazy text" $ do
      convertText ("hello" :: T.Text) `shouldBe` ("hello" :: TL.Text)
      convertText ("hello" :: TL.Text) `shouldBe` ("hello" :: T.Text)

    it "can convert between things with ToText/FromText conversions" $
      convertText (Upper "HELLO") `shouldBe` Lower "hello"

    it "can convert between things with FromText instances that produce functors" $
      convertText ("hello" :: T.Text) `shouldBe` Just FailableRepresentation

    describe "UTF8" $
      it "properly encodes text as bytestrings" $ do
        convertText ("hello" :: T.Text) `shouldBe` UTF8 ("hello" :: B.ByteString)
        convertText ("hello" :: T.Text) `shouldBe` UTF8 ("hello" :: BL.ByteString)

    describe "Base16" $ do
      it "encodes bytestrings as base 16 encoded text" $ do
        convertText (Base16 ("hello" :: B.ByteString)) `shouldBe` ("68656c6c6f" :: T.Text)
        convertText (Base16 ("hello" :: BL.ByteString)) `shouldBe` ("68656c6c6f" :: T.Text)

      it "decodes properly encoded base 16 text as bytestrings" $ do
        convertText ("68656c6c6f" :: T.Text) `shouldBe` Just (Base16 ("hello" :: B.ByteString))
        convertText ("68656c6c6f" :: T.Text) `shouldBe` Just (Base16 ("hello" :: BL.ByteString))

      it "fails to decode improperly encoded base 16 text as bytestrings" $ do
        convertText ("not base 16" :: T.Text) `shouldBe` (Nothing :: Maybe (Base16 B.ByteString))
        convertText ("not base 16" :: T.Text) `shouldBe` (Nothing :: Maybe (Base16 BL.ByteString))

    describe "Base64" $ do
      it "encodes bytestrings as base 64 encoded text" $ do
        convertText (Base64 ("hello" :: B.ByteString)) `shouldBe` ("aGVsbG8=" :: T.Text)
        convertText (Base64 ("hello" :: BL.ByteString)) `shouldBe` ("aGVsbG8=" :: T.Text)

      it "decodes properly encoded base 64 text as bytestrings" $ do
        convertText ("aGVsbG8=" :: T.Text) `shouldBe` Just (Base64 ("hello" :: B.ByteString))
        convertText ("aGVsbG8=" :: T.Text) `shouldBe` Just (Base64 ("hello" :: BL.ByteString))

      it "fails to decode improperly encoded base 64 text as bytestrings" $ do
        convertText ("not base 16" :: T.Text) `shouldBe` (Nothing :: Maybe (Base64 B.ByteString))
        convertText ("not base 16" :: T.Text) `shouldBe` (Nothing :: Maybe (Base64 BL.ByteString))

  describe "decodeConvertText" $ do
    it "can convert between things in functors with a DecodeText instance" $
      decodeConvertText FailableString `shouldBe` Just ("failable" :: TL.Text)

    describe "UTF8" $ do
      it "successfully decodes properly encoded bytestrings" $ do
        decodeConvertText (UTF8 ("hello" :: B.ByteString)) `shouldBe` Just ("hello" :: T.Text)
        decodeConvertText (UTF8 ("hello" :: BL.ByteString)) `shouldBe` Just ("hello" :: T.Text)

      it "fails to decode improperly encoded bytestrings" $ do
        decodeConvertText (UTF8 ("invalid \xc3\x28" :: B.ByteString)) `shouldBe` (Nothing :: Maybe T.Text)
        decodeConvertText (UTF8 ("invalid \xc3\x28" :: BL.ByteString)) `shouldBe` (Nothing :: Maybe T.Text)
