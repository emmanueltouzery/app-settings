import Data.AppSettings

import Test.Hspec
import Data.Map as M

textSizeFromWidth :: Setting Double
textSizeFromWidth = Setting "textSizeFromWidth" 0.04

textFill :: Setting (Double,Double,Double,Double)
textFill = Setting "textFill" (1, 1, 0, 1)

textSizeFromHeight :: Setting Double
textSizeFromHeight = Setting "textSizeFromHeight" 12.4

getBlankSettings :: Conf
getBlankSettings = getBlankConfig $ do
	setting textSizeFromWidth
	setting textSizeFromHeight
	setting textFill

main :: IO ()
main = hspec $ do
	describe "load config" $ do
		testEmptyFile
		testEmptyFileDefaults
		testPartialFileDefaults

testEmptyFile :: Spec
testEmptyFile = it "parses correctly an empty file" $ do
	readSettingsFrom "tests/empty.config" >>= \(conf, _) -> conf `shouldBe`
		M.fromList []

testEmptyFileDefaults :: Spec
testEmptyFileDefaults = it "parses correctly an empty file with defaults" $ do
	readSettingsFromDefaults "tests/empty.config" getBlankSettings >>= \(conf, _) -> conf `shouldBe`
		M.fromList [
			("textSizeFromWidth", show $ defaultValue textSizeFromWidth),
			("textSizeFromHeight", show $ defaultValue textSizeFromHeight),
			("textFill", show $ defaultValue textFill)]

testPartialFileDefaults :: Spec
testPartialFileDefaults = it "parses correctly a partial file with defaults" $ do
	readSettingsFromDefaults "tests/partial.config" getBlankSettings >>= \(conf, _) -> conf `shouldBe`
		M.fromList [
			("textSizeFromWidth", "1.02"),
			("textSizeFromHeight", show $ defaultValue textSizeFromHeight),
			("textFill", "(1,2,3,4)")]
