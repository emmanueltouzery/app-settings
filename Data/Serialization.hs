{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Data.Serialization (readConfigFile, writeConfigFile) where

import Data.Text.IO as IO
import Data.Map as M
import Text.ParserCombinators.Parsec
import Text.Parsec.Text as T

readConfigFile :: FilePath -> IO (Map String String)
readConfigFile path = do
	contents <- IO.readFile path
	return $ case parse parseConfigFile "" contents of
		Left _ -> M.fromList []
		Right v -> M.fromList v

parseConfigFile :: T.GenParser st [(String, String)]
parseConfigFile = many parseConfigEntry

parseConfigEntry :: T.GenParser st (String,String)
parseConfigEntry = do
	key <- many $ noneOf "="
	char '='
	value <- many $ noneOf "\r\n"
	many $ oneOf "\r\n"
	return (key, value)

writeConfigFile :: FilePath -> Map String String -> IO ()
writeConfigFile path = undefined
