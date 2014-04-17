{-# LANGUAGE RankNTypes #-}

module Data.AppSettings where

import Data.Maybe (fromMaybe)
import System.Directory
import Control.Monad (liftM)
import Data.Map as M
import Control.Monad
import Control.Monad.State

import Data.Serialization

type Conf = M.Map String String

-- http://stackoverflow.com/questions/23117205/
newtype GetSetting = GetSetting (forall a. Read a => Setting a -> a)

data Setting a = Setting { name :: String, defaultValue :: a }

setting :: (Show a) => Setting a -> State Conf ()
setting (Setting name defaultV) = do
	soFar <- get
	put $ M.insert name (show defaultV) soFar

getBlankConfig :: State Conf () -> Conf
getBlankConfig actions = do
	execState actions M.empty

readSettings :: IO (Conf, GetSetting)
readSettings = getConfigFileName >>= readSettingsFrom

readSettingsDefaults :: Conf -> IO (Conf, GetSetting)
readSettingsDefaults defaults = do
	fname <- getConfigFileName
	readSettingsFromDefaults fname defaults

readSettingsFrom :: FilePath -> IO (Conf, GetSetting)
readSettingsFrom filename = liftM (\conf -> (conf, GetSetting $ readSetting conf))
				$ readConfigFile filename

readSettingsFromDefaults :: FilePath -> Conf -> IO (Conf, GetSetting)
readSettingsFromDefaults filename defaults = do
	(conf, get) <- readSettingsFrom filename
	return (M.union conf defaults, get)

saveSettings :: Conf -> IO ()
saveSettings conf = getConfigFileName >>= \fname -> writeConfigFile fname conf

readSetting :: (Read a) => Conf -> Setting a -> a
readSetting conf (Setting key defaultV) = maybe defaultV read (M.lookup key conf)
--readSetting conf (Setting key defaultV) = fromMaybe defaultV $ liftM read (M.lookup key conf)

setSetting :: (Show a) => Conf -> Setting a -> a -> Conf
setSetting conf (Setting key _) v = M.insert key (show v) conf

-- TODO besides font name also add the date format string

getSettingsFolder :: IO FilePath
getSettingsFolder = do
	home <- getHomeDirectory
	let result = home ++ "/.picdate/"
	createDirectoryIfMissing False result
	return result

getConfigFileName :: IO String
getConfigFileName = fmap (++"config.json") getSettingsFolder
