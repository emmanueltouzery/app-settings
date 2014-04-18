{-# LANGUAGE RankNTypes #-}

module Data.AppSettings (
	Conf,
	Setting(..),
	GetSetting(..),
	setting,
	getDefaultConfig,
	-- TODO merge all the readSettings* using a data
	-- with a default value as parameter.
	-- Same with saveSettings.
	readSettings,
	readSettingsDefaults,
	readSettingsFrom,
	readSettingsFromDefaults,
	saveSettings,
	saveSettingsTo,
	getSetting,
	setSetting) where

import Data.Maybe (fromMaybe)
import System.Directory
import Control.Monad (liftM)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

import Data.Serialization

-- http://stackoverflow.com/questions/23117205/
newtype GetSetting = GetSetting (forall a. Read a => Setting a -> a)

data Setting a = Setting { name :: String, defaultValue :: a }

setting :: (Show a) => Setting a -> State Conf ()
setting (Setting name defaultV) = do
	soFar <- get
	put $ M.insert name (SettingInfo { value = show defaultV, userSet = False }) soFar

getDefaultConfig :: State Conf () -> Conf
getDefaultConfig actions = do
	execState actions M.empty

readSettings :: IO (Conf, GetSetting)
readSettings = getConfigFileName >>= readSettingsFrom

readSettingsDefaults :: Conf -> IO (Conf, GetSetting)
readSettingsDefaults defaults = do
	fname <- getConfigFileName
	readSettingsFromDefaults fname defaults

readSettingsFrom :: FilePath -> IO (Conf, GetSetting)
readSettingsFrom filename = liftM addGetSetting $ readConfigFile filename
	where
		addGetSetting conf = (conf, GetSetting $ getSetting conf)

readSettingsFromDefaults :: FilePath -> Conf -> IO (Conf, GetSetting)
readSettingsFromDefaults filename defaults = do
	(conf, get) <- readSettingsFrom filename
	return (M.union conf defaults, get)

saveSettings :: Conf -> IO ()
saveSettings conf = getConfigFileName >>= \fname -> saveSettingsTo fname conf

saveSettingsTo :: FilePath -> Conf -> IO ()
saveSettingsTo filename conf = writeConfigFile filename conf

getSetting :: (Read a) => Conf -> Setting a -> a
getSetting conf (Setting key defaultV) = maybe defaultV (read . value) (M.lookup key conf)

setSetting :: (Show a) => Conf -> Setting a -> a -> Conf
setSetting conf (Setting key _) v = M.insert key (SettingInfo { value = show v, userSet=True }) conf

-- TODO take the app name as parameter
getSettingsFolder :: IO FilePath
getSettingsFolder = do
	home <- getHomeDirectory
	let result = home ++ "/.picdate/"
	createDirectoryIfMissing False result
	return result

getConfigFileName :: IO String
getConfigFileName = fmap (++"config.ini") getSettingsFolder
