{-# LANGUAGE RankNTypes #-}

module Data.AppSettings (
	Conf,
	DefaultConfig,
	Setting(..),
	GetSetting(..),
	setting,
	getDefaultConfig,
	emptyDefaultConfig,
	FileLocation(..),
	readSettings,
	ParseException,
	saveSettings,
	setSetting) where

import System.Directory
import qualified Data.Map as M
import Control.Monad.State
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

import Data.Serialization

-- http://stackoverflow.com/questions/23117205/
newtype GetSetting = GetSetting (forall a. Read a => Setting a -> a)

-- for the tests...
instance Show GetSetting where
	show _ = "GetSetting"

data Setting a = Setting { name :: String, defaultValue :: a }

-- | Information about the default configuration. Contains
-- all the settings (that you declare using getDefaultConfig)
-- and their default values. It is useful when you save a
-- configuration file, if you give this information to saveSettings,
-- it will save default options in the configuration file
-- in a commented form, as a form of documentation to a user
-- who would edit the configuration file.
-- However this is completely optional, you can give
-- emptyDefaultConfig if you don't want this behaviour.
newtype DefaultConfig = DefaultConfig Conf

-- | Default configuration containing no options. It's fine
-- to give that to saveSettings if you don't want default
-- settings being written to the configuration file in
-- commented form (see 'DefaultConfig')
emptyDefaultConfig :: DefaultConfig
emptyDefaultConfig = DefaultConfig M.empty

setting :: (Show a) => Setting a -> State Conf ()
setting (Setting nameV defaultV) = do
	soFar <- get
	put $ M.insert nameV (SettingInfo { value = show defaultV, userSet = False }) soFar

-- | Used in combination with 'setting' to register settings.
-- Registering settings is optional, see 'DefaultConfig'.
--
-- @
-- defaultSettings :: DefaultConfig
-- defaultSettings = getDefaultConfig $ do
--     setting <setting1>
--     setting <setting2>
-- @
getDefaultConfig :: State Conf () -> DefaultConfig
getDefaultConfig actions = do
	DefaultConfig $ execState actions M.empty

-- | Where to look for or store the configuration file.
data FileLocation = AutoFromAppName String
		    -- ^ Automatically build the location based on the
		    -- application name. It will be ~/.<app name>/config.ini.
		  | Path FilePath
		    -- ^ Absolute path to a location on disk.

getPathForLocation :: FileLocation -> IO FilePath
getPathForLocation location = case location of
	AutoFromAppName appName -> getConfigFileName appName
	Path path -> return path

-- | Read settings from disk.
-- Because it is doing file I/O it is smart to wrap the call
-- with a try, as I/O exceptions can be thrown.
-- Also, the function will throw a 'ParseException' if the
-- file is not properly formatted.
-- NOTE that if the file is properly formatted in general,
-- but a value is stored in an invalid format (for instance "hello"
-- for a Double), you will get no error and get the default value
-- for that setting when you attempt to read it.
--
-- This function also returns you a pair. The first element
-- is the configuration itself, which you can use to save
-- back or modify the configuration.
-- The second element is a function wrapped in the 'GetSetting'
-- newtype. This function allows you to read a configuration
-- option simply by giving that option.
--
-- Example of use:
--
-- @
-- readResult <- try $ readSettings (Path \"my.config\")
-- case readResult of
-- 	Right (conf, GetSetting getSetting) -> do
-- 		let textSize = getSetting textSizeFromWidth
-- 		saveSettings getDefaultConfig (Path \"my.config\") conf
-- 	Left (x :: SomeException) -> error \"Error reading the config file!\"
-- @
readSettings :: FileLocation -> IO (Conf, GetSetting)
readSettings location = do
	filePath <- getPathForLocation location
	exists <- doesFileExist filePath
	conf <- if exists
		then readConfigFile filePath
		else return M.empty
	return (conf, GetSetting $ getSetting' conf)

-- | It is advised to run the save within a try call
-- because it does disk I/O, otherwise the call is straightforward.
saveSettings :: DefaultConfig -> FileLocation -> Conf -> IO ()
saveSettings (DefaultConfig defaults) location conf = do
	filePath <- getPathForLocation location
	writeConfigFile filePath (M.union conf defaults)

-- TODO maybe another getSetting that'll tell you
-- if the setting is invalid in the config file instead of silently
-- give you the default?
getSetting' :: (Read a) => Conf -> Setting a -> a
getSetting' conf (Setting key defaultV) = fromMaybe defaultV $ getSettingValueFromConf conf key

getSettingValueFromConf :: Read a => Conf -> String -> Maybe a
getSettingValueFromConf conf key = do
	asString <- M.lookup key conf
	readMaybe $ value asString

-- | Change the value of a setting. You'll have to call
-- 'saveSettings' so that the change is written to disk.
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
