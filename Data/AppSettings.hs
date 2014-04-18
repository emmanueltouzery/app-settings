{-# LANGUAGE RankNTypes #-}

module Data.AppSettings (
	Conf,
	Setting(..),
	GetSetting(..),
	setting,
	getDefaultConfig,
	FileLocation(..),
	readSettings,
	saveSettings,
	getSetting',
	setSetting) where

import System.Directory
import qualified Data.Map as M
import Control.Monad.State

import Data.Serialization

-- http://stackoverflow.com/questions/23117205/
newtype GetSetting = GetSetting (forall a. Read a => Setting a -> a)

-- for the tests...
instance Show GetSetting where
	show _ = "GetSetting"

data Setting a = Setting { name :: String, defaultValue :: a }

setting :: (Show a) => Setting a -> State Conf ()
setting (Setting name defaultV) = do
	soFar <- get
	put $ M.insert name (SettingInfo { value = show defaultV, userSet = False }) soFar

getDefaultConfig :: State Conf () -> Conf
getDefaultConfig actions = do
	execState actions M.empty

data FileLocation = AutoFromAppName String
	| Path FilePath

getPathForLocation :: FileLocation -> IO FilePath
getPathForLocation location = case location of
	AutoFromAppName appName -> getConfigFileName appName
	Path path -> return path

readSettings :: Conf -> FileLocation -> IO (Conf, GetSetting)
readSettings defaults location = do
	filePath <- getPathForLocation location
	conf <- readConfigFile filePath
	return (M.union conf defaults, GetSetting $ getSetting' conf)

saveSettings :: FileLocation -> Conf -> IO ()
saveSettings location conf = do
	filePath <- getPathForLocation location
	writeConfigFile filePath conf

getSetting' :: (Read a) => Conf -> Setting a -> a
getSetting' conf (Setting key defaultV) = maybe defaultV (read . value) (M.lookup key conf)

setSetting :: (Show a) => Conf -> Setting a -> a -> Conf
setSetting conf (Setting key _) v = M.insert key (SettingInfo { value = show v, userSet=True }) conf

getSettingsFolder :: String -> IO FilePath
getSettingsFolder appName = do
	home <- getHomeDirectory
	let result = home ++ "/." ++ appName ++ "/"
	createDirectoryIfMissing False result
	return result

getConfigFileName :: String -> IO String
getConfigFileName appName = fmap (++"config.ini") $ getSettingsFolder appName
