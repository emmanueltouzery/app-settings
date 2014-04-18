{-# LANGUAGE ScopedTypeVariables #-}

import Data.AppSettings

import Test.Hspec
import Test.HUnit (assertBool)

import System.Directory (removeFile)
import Control.Exception (try, SomeException)

textSizeFromWidth :: Setting Double
textSizeFromWidth = Setting "textSizeFromWidth" 0.04

textFill :: Setting (Double,Double,Double,Double)
textFill = Setting "textFill" (1, 1, 0, 1)

textSizeFromHeight :: Setting Double
textSizeFromHeight = Setting "textSizeFromHeight" 12.4

getDefaultSettings :: Conf
getDefaultSettings = getDefaultConfig $ do
	setting textSizeFromWidth
	setting textSizeFromHeight
	setting textFill

main :: IO ()
main = hspec $ do
	describe "load config" $ do
		testEmptyFileDefaults
		testPartialFileDefaults
		testFileWithComments
		testInvalidFile
	describe "save config" $ do
		testSaveUserSetAndDefaults
	-- readSetting for which i have no value

testEmptyFileDefaults :: Spec
testEmptyFileDefaults = it "parses correctly an empty file with defaults" $ do
	readResult <- try $ readSettings getDefaultSettings (Path "tests/empty.config")
	case readResult of
		Right (_, GetSetting getSetting) -> do
			getSetting textSizeFromWidth `shouldBe` 0.04
			getSetting textSizeFromHeight `shouldBe` 12.4
			getSetting textFill `shouldBe` (1,1,0,1)
		Left (x :: SomeException) -> assertBool (show x) False

testPartialFileDefaults :: Spec
testPartialFileDefaults = it "parses correctly a partial file with defaults" $ do
	readResult <- try $ readSettings getDefaultSettings (Path "tests/partial.config")
	case readResult of
		Right (_, GetSetting getSetting) -> do
			getSetting textSizeFromWidth `shouldBe` 1.02
			getSetting textSizeFromHeight `shouldBe` 12.4
			getSetting textFill `shouldBe` (1,2,3,4)
		Left (x :: SomeException) -> assertBool (show x) False

testFileWithComments :: Spec
testFileWithComments = it "parses correctly a partial file with comments" $ do
	readResult <- try $ readSettings getDefaultSettings (Path "tests/test-save.txt")
	case readResult of
		Right (_, GetSetting getSetting) -> do
			getSetting textSizeFromWidth `shouldBe` 1.02
			getSetting textSizeFromHeight `shouldBe` 12.4
			getSetting textFill `shouldBe` (1,2,3,4)
		Left (x :: SomeException) -> assertBool (show x) False

testInvalidFile :: Spec
testInvalidFile = it "reports errors properly for invalid files" $ do
	readResult <- try $ readSettings getDefaultSettings (Path "tests/broken.config")
	case readResult of
		Left (x:: SomeException) -> assertBool "ok" True
		Right _ -> assertBool "did not get an error!" False

testSaveUserSetAndDefaults :: Spec
testSaveUserSetAndDefaults = it "saves a file with user-set and default settings" $ do
	readResult <- try $ readSettings getDefaultSettings (Path "tests/partial.config") 
	case readResult of 
		Right (conf, _) -> do
			saveSettings (Path "test.txt") conf
			actual <- readFile "test.txt"
			reference <- readFile "tests/test-save.txt"
			actual `shouldBe` reference
			removeFile "test.txt"
		Left (x :: SomeException) -> assertBool (show x) False
