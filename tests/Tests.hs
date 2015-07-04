{-# LANGUAGE ScopedTypeVariables #-}

import Data.AppSettings
import Data.AppSettingsInternal

import Test.Hspec
import Test.HUnit (assertBool, assertEqual)

import System.Directory (removeFile, copyFile, doesFileExist)
import Control.Exception (try, SomeException)

textSizeFromWidth :: Setting Double
textSizeFromWidth = Setting "textSizeFromWidth" 0.04

textFill :: Setting (Double,Double,Double,Double)
textFill = Setting "textFill" (1, 1, 0, 1)

textSizeFromHeight :: Setting Double
textSizeFromHeight = Setting "textSizeFromHeight" 12.4

testInlineList :: Setting [String]
testInlineList = Setting "testInlineList" ["inline1", "inline2", "inline3"]

testList :: Setting [String]
testList = ListSetting "testList" ["list1", "list2", "list3"]

defaultConfig :: DefaultConfig
defaultConfig = getDefaultConfig $ do
    setting textSizeFromWidth
    setting textSizeFromHeight
    setting textFill
    setting testInlineList
    setting testList

main :: IO ()
main = hspec $ do
    describe "unit tests" testIsKeyForListSetting
    describe "load config" $ do
        testEmptyFileDefaults
        testPartialFileDefaults
        testPartialFileDefaults2
        testFileWithComments
        testInvalidFile
        testInvalidValue
        testNonExistingFile
        testReadLongSetting
    describe "save config" $ do
        testSaveUserSetAndDefaults
        createBakBeforeSaving
        wrapLongSettings
    describe "set setting" testSetListSetting

testEmptyFileDefaults :: Spec
testEmptyFileDefaults = it "parses correctly an empty file with defaults" $ do
    readResult <- try $ readSettings (Path "tests/empty.config")
    case readResult of
        Right (_, GetSetting getSetting) -> do
            getSetting textSizeFromWidth `shouldBe` 0.04
            getSetting textSizeFromHeight `shouldBe` 12.4
            getSetting textFill `shouldBe` (1,1,0,1)
            getSetting testInlineList `shouldBe` ["inline1", "inline2", "inline3"]
            getSetting testList `shouldBe` ["list1", "list2", "list3"]
        Left (x :: SomeException) -> assertBool (show x) False

testPartialFileDefaults :: Spec
testPartialFileDefaults = it "parses correctly a partial file with defaults" $ do
    readResult <- try $ readSettings (Path "tests/partial.config")
    case readResult of
        Right (_, GetSetting getSetting) -> do
            getSetting textSizeFromWidth `shouldBe` 1.02
            getSetting textSizeFromHeight `shouldBe` 12.4
            getSetting textFill `shouldBe` (1,2,3,4)
            getSetting testInlineList `shouldBe` ["un", "deux"]
            getSetting testList `shouldBe` ["one", "two"]
        Left (x :: SomeException) -> assertBool (show x) False

testPartialFileDefaults2 :: Spec
testPartialFileDefaults2 = it "parses correctly a partial2 file with defaults" $ do
    readResult <- try $ readSettings (Path "tests/partial2.config")
    case readResult of
        Right (_, GetSetting getSetting) -> do
            getSetting textSizeFromWidth `shouldBe` 1.02
            getSetting textSizeFromHeight `shouldBe` 12.4
            getSetting textFill `shouldBe` (1,2,3,4)
            getSetting testInlineList `shouldBe` ["un", "deux"]
            getSetting testList `shouldBe` []
        Left (x :: SomeException) -> assertBool (show x) False

testFileWithComments :: Spec
testFileWithComments = it "parses correctly a partial file with comments" $ do
    readResult <- try $ readSettings (Path "tests/test-save.txt")
    case readResult of
        Right (_, GetSetting getSetting) -> do
            getSetting textSizeFromWidth `shouldBe` 1.02
            getSetting textSizeFromHeight `shouldBe` 12.4
            getSetting textFill `shouldBe` (1,2,3,4)
            getSetting testInlineList `shouldBe` ["un", "deux"]
            getSetting testList `shouldBe` ["one", "two"]
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
            getSetting testInlineList `shouldBe` ["inline1", "inline2", "inline3"]
            getSetting testList `shouldBe` ["list1", "list2", "list3"]
        Left (x :: SomeException) -> assertBool (show x) False

testNonExistingFile :: Spec
testNonExistingFile = it "returns empty config for a non-existing file" $ do
    readResult <- try $ readSettings (Path "tests/dontexist.conf")
    case readResult of
        Right (_, GetSetting getSetting) -> do
            getSetting textSizeFromWidth `shouldBe` 0.04
            getSetting textSizeFromHeight `shouldBe` 12.4
            getSetting textFill `shouldBe` (1,1,0,1)
            getSetting testInlineList `shouldBe` ["inline1", "inline2", "inline3"]
            getSetting testList `shouldBe` ["list1", "list2", "list3"]
        Left (x :: SomeException) -> assertBool (show x) False

testReadLongSetting :: Spec
testReadLongSetting = it "Reads setting files with setting values wrapped over several lines" $ do
    readResult <- try $ readSettings (Path "tests/longsetting.conf")
    case readResult of
        Right (_, GetSetting getVal) -> do
            let expected = replicate 20 "long      string"
            let actual = getVal testInlineList
            assertEqual "doesn't match" expected actual
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
            saveSettings defaultConfig (Path "test.txt") conf
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
            saveSettings defaultConfig (Path "p.config") conf
            doesFileExist "p.config.bak" >>= flip shouldBe True
            doesFileExist "p.config" >>= flip shouldBe True
            removeFile "p.config"
            removeFile "p.config.bak"
        Left (x :: SomeException) -> assertBool (show x) False

wrapLongSettings :: Spec
wrapLongSettings = it "wraps long settings when saving" $ do
    readResult <- try $ readSettings (Path "tests/partial.config")
    case readResult of
        Right (conf, _) -> do
            let conf1 = setSetting conf testInlineList $ replicate 20 "long      string"
            saveSettings defaultConfig (Path "xx.config") conf1
            expected <- readFile "tests/longsetting.conf"
            actual <- readFile "xx.config"
            assertEqual "doesn't match" expected actual
            removeFile "xx.config"
        Left (x :: SomeException) -> assertBool (show x) False


testSetListSetting :: Spec
testSetListSetting = it "overwrites correctly a list setting" $ do
    readResult <- try $ readSettings (Path "tests/empty.config")
    case readResult of
        Right (conf, GetSetting getSetting) -> do
            getSetting testList `shouldBe` ["list1", "list2", "list3"]
            getSetting testInlineList `shouldBe` ["inline1", "inline2", "inline3"]
            let conf0 = setSetting conf testList ["un"]
            let conf1 = setSetting conf0 testInlineList ["one"]
            getSetting' conf1 testList `shouldBe` ["un"]
            getSetting' conf1 testInlineList `shouldBe` ["one"]
            let conf2 = setSetting conf1 testList []
            let conf3 = setSetting conf2 testInlineList []
            getSetting' conf3 testList `shouldBe` []
            getSetting' conf3 testInlineList `shouldBe` []
        Left (x :: SomeException) -> assertBool (show x) False

testIsKeyForListSetting :: Spec
testIsKeyForListSetting = it "checks correctly whether a key is attached to a list setting" $ do
    isKeyForListSetting "listTest" "listTest_1" `shouldBe` True
    isKeyForListSetting "listTest" "listTest_13" `shouldBe` True
    isKeyForListSetting "listTest" "listTest_unrelated" `shouldBe` False
    isKeyForListSetting "listTest" "listTest_1notanumber" `shouldBe` False
    isKeyForListSetting "listTest" "listTestX_1" `shouldBe` False
