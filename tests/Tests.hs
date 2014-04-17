import Data.AppSettings

import Test.Hspec
import Data.Map as M

textSizeFromWidth :: Setting Double
textSizeFromWidth = Setting "textSizeFromWidth" 0.04

textFill :: Setting (Double,Double,Double,Double)
textFill = Setting "textFill" (1, 1, 0, 1)

getBlankSettings :: Conf
getBlankSettings = getBlankConfig $ do
	setting textSizeFromWidth
	setting textFill

main :: IO ()
main = hspec $ do
	describe "load config" $ testEmptyFile
	describe "load config empty file" $ testEmptyFileDefaults

testEmptyFile :: Spec
testEmptyFile = it "parses correctly an empty file" $ do
	readSettingsFrom "tests/empty.config" >>= \(conf, _) -> conf `shouldBe`
		M.fromList []

testEmptyFileDefaults :: Spec
testEmptyFileDefaults = it "parses correctly an empty file with defaults" $ do
	readSettingsFromDefaults "tests/empty.config" getBlankSettings >>= \(conf, _) -> conf `shouldBe`
		M.fromList [
			("textSizeFromWidth", show $ defaultValue textSizeFromWidth),
			("textFill", show $ defaultValue textFill)]
