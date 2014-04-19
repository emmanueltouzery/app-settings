{-# LANGUAGE ScopedTypeVariables #-}

import Data.AppSettings

import Test.Hspec
import Test.HUnit (assertBool)

import System.Directory (removeFile, copyFile, doesFileExist)
import Control.Exception (try, SomeException)

textSizeFromWidth :: Setting Double
textSizeFromWidth = Setting "textSizeFromWidth" 0.04

textFill :: Setting (Double,Double,Double,Double)
textFill = Setting "textFill" (1, 1, 0, 1)

textSizeFromHeight :: Setting Double
textSizeFromHeight = Setting "textSizeFromHeight" 12.4

getDefaultSettings :: DefaultConfig
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
		testInvalidValue
	describe "save config" $ do
		testSaveUserSetAndDefaults
		createBakBeforeSaving

testEmptyFileDefaults :: Spec
testEmptyFileDefaults = it "parses correctly an empty file with defaults" $ do
	readResult <- try $ readSettings (Path "tests/empty.config")
	case readResult of
		Right (_, GetSetting getSetting) -> do
			getSetting textSizeFromWidth `shouldBe` 0.04
			getSetting textSizeFromHeight `shouldBe` 12.4
			getSetting textFill `shouldBe` (1,1,0,1)
		Left (x :: SomeException) -> assertBool (show x) False

testPartialFileDefaults :: Spec
testPartialFileDefaults = it "parses correctly a partial file with defaults" $ do
	readResult <- try $ readSettings (Path "tests/partial.config")
	case readResult of
		Right (_, GetSetting getSetting) -> do
			getSetting textSizeFromWidth `shouldBe` 1.02
			getSetting textSizeFromHeight `shouldBe` 12.4
			getSetting textFill `shouldBe` (1,2,3,4)
		Left (x :: SomeException) -> assertBool (show x) False

testFileWithComments :: Spec
testFileWithComments = it "parses correctly a partial file with comments" $ do
	readResult <- try $ readSettings (Path "tests/test-save.txt")
	case readResult of
		Right (_, GetSetting getSetting) -> do
			getSetting textSizeFromWidth `shouldBe` 1.02
			getSetting textSizeFromHeight `shouldBe` 12.4
			getSetting textFill `shouldBe` (1,2,3,4)
		Left (x :: SomeException) -> assertBool (show x) False

testInvalidValue :: Spec
testInvalidValue = it "parses correctly a file with invalid values" $ do
	readResult <- try $ readSettings (Path "tests/brokenvalue.config")
	case readResult of
		Right (_, GetSetting getSetting) -> do
			-- give the default for the invalid value
			getSetting textSizeFromWidth `shouldBe` 0.04
			getSetting textSizeFromHeight `shouldBe` 12.4
			getSetting textFill `shouldBe` (1,2,3,4)
		Left (x :: SomeException) -> assertBool (show x) False

testInvalidFile :: Spec
testInvalidFile = it "reports errors properly for invalid files" $ do
	readResult <- try $ readSettings (Path "tests/broken.config")
	case readResult of
		Left (_ :: SomeException) -> assertBool "ok" True
		Right _ -> assertBool "did not get an error!" False

testSaveUserSetAndDefaults :: Spec
testSaveUserSetAndDefaults = it "saves a file with user-set and default settings" $ do
	readResult <- try $ readSettings (Path "tests/partial.config") 
	case readResult of 
		Right (conf, _) -> do
			saveSettings getDefaultSettings (Path "test.txt") conf
			actual <- readFile "test.txt"
			reference <- readFile "tests/test-save.txt"
			actual `shouldBe` reference
			removeFile "test.txt"
		Left (x :: SomeException) -> assertBool (show x) False

createBakBeforeSaving :: Spec
createBakBeforeSaving = it "creates a backup of the config file before overwriting it" $ do
	readResult <- try $ readSettings (Path "tests/partial.config") 
	case readResult of 
		Right (conf, _) -> do
			copyFile "tests/partial.config" "p.config"
			saveSettings getDefaultSettings (Path "p.config") conf
			doesFileExist "p.config.bak" >>= (flip shouldBe) True
			doesFileExist "p.config" >>= (flip shouldBe) True
			removeFile "p.config"
			removeFile "p.config.bak"
		Left (x :: SomeException) -> assertBool (show x) False
