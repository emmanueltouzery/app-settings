import Data.AppSettings

import Test.Hspec

import System.Directory (removeFile)

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
--		testInvalidFile
	describe "save config" $ do
		testUserSetAndDefaults
	-- readSetting for which i have no value

testEmptyFileDefaults :: Spec
testEmptyFileDefaults = it "parses correctly an empty file with defaults" $ do
	readSettings getDefaultSettings (Path "tests/empty.config") >>= \(_, GetSetting getSetting) -> do
		getSetting textSizeFromWidth `shouldBe` 0.04
		getSetting textSizeFromHeight `shouldBe` 12.4
		getSetting textFill `shouldBe` (1,1,0,1)

testPartialFileDefaults :: Spec
testPartialFileDefaults = it "parses correctly a partial file with defaults" $ do
	readSettings getDefaultSettings (Path "tests/partial.config") >>= \(_, GetSetting getSetting) -> do
		getSetting textSizeFromWidth `shouldBe` 1.02
		getSetting textSizeFromHeight `shouldBe` 12.4
		getSetting textFill `shouldBe` (1,2,3,4)

testFileWithComments :: Spec
testFileWithComments = it "parses correctly a partial file with comments" $ do
	readSettings getDefaultSettings (Path "tests/test-save.txt") >>= \(_, GetSetting getSetting) -> do
		getSetting textSizeFromWidth `shouldBe` 1.02
		getSetting textSizeFromHeight `shouldBe` 12.4
		getSetting textFill `shouldBe` (1,2,3,4)

--testInvalidFile :: Spec
--testInvalidFile = it "reports errors properly for invalid files" $ do
--	readSettings getDefaultSettings (Path "tests/broken.config") >>= \(_, GetSetting getSetting) -> do

testUserSetAndDefaults :: Spec
testUserSetAndDefaults = it "saves a file with user-set and default settings" $ do
	readSettings getDefaultSettings (Path "tests/partial.config") >>= \(conf, _) -> do
		saveSettings (Path "test.txt") conf
		actual <- readFile "test.txt"
		reference <- readFile "tests/test-save.txt"
		actual `shouldBe` reference
		removeFile "test.txt"
