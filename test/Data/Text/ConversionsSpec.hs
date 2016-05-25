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

spec :: Spec
spec = do
  describe "ConvertText" $ do
    it "can convert strings to and from text" $ do
      convertText ("hello" :: String) `shouldBe` ("hello" :: T.Text)
      convertText ("hello" :: T.Text) `shouldBe` ("hello" :: String)

    it "converts between strict and lazy text" $ do
      convertText ("hello" :: T.Text) `shouldBe` ("hello" :: TL.Text)
      convertText ("hello" :: TL.Text) `shouldBe` ("hello" :: T.Text)

    it "can convert between things with ToText/FromText conversions" $
      convertText (Upper "HELLO") `shouldBe` Lower "hello"

    it "can convert between things in functors with a DecodeText instance" $
      convertText FailableString `shouldBe` Just ("failable" :: TL.Text)

    describe "UTF8" $ do
      it "successfully decodes properly encoded bytestrings" $ do
        convertText (UTF8 ("hello" :: B.ByteString)) `shouldBe` Just ("hello" :: T.Text)
        convertText (UTF8 ("hello" :: BL.ByteString)) `shouldBe` Just ("hello" :: T.Text)

      it "fails to decode improperly encoded bytestrings" $ do
        convertText (UTF8 ("invalid \xc3\x28" :: B.ByteString)) `shouldBe` (Nothing :: Maybe T.Text)
        convertText (UTF8 ("invalid \xc3\x28" :: BL.ByteString)) `shouldBe` (Nothing :: Maybe T.Text)

      it "properly encodes text as bytestrings" $ do
        convertText ("hello" :: T.Text) `shouldBe` UTF8 ("hello" :: B.ByteString)
        convertText ("hello" :: T.Text) `shouldBe` UTF8 ("hello" :: BL.ByteString)
