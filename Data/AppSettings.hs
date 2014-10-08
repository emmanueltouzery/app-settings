{-# LANGUAGE RankNTypes, GADTs #-}

module Data.AppSettings (
	-- $intro
	Conf,
	DefaultConfig(..),
	Setting(..),
	GetSetting(..),
	setting,
	getDefaultConfig,
	emptyDefaultConfig,
	FileLocation(..),
	readSettings,
	ParseException,
	saveSettings,
	setSetting,
	getSetting') where

import System.Directory
import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative

import Data.Serialization
import Data.AppSettingsInternal

-- http://stackoverflow.com/questions/23117205/
newtype GetSetting = GetSetting (forall a. Read a => Setting a -> a)

-- for the tests...
instance Show GetSetting where
	show _ = "GetSetting"

-- | Information about the default configuration. Contains
-- all the settings (that you declare using 'getDefaultConfig')
-- and their default values. It is useful when you save a
-- configuration file, if you give this information to 'saveSettings',
-- it will save default options in the configuration file
-- in a commented form, as a form of documentation to a user
-- who would edit the configuration file.
-- However this is completely optional, you can give
-- 'emptyDefaultConfig' if you don't want this behaviour.
newtype DefaultConfig = DefaultConfig Conf

-- | Default configuration containing no options. It's fine
-- to give that to 'saveSettings' if you don't want default
-- settings being written to the configuration file in
-- commented form (see 'DefaultConfig')
emptyDefaultConfig :: DefaultConfig
emptyDefaultConfig = DefaultConfig M.empty

-- | see the 'getDefaultConfig' documentation.
setting :: (Show a) => Setting a -> State Conf ()
setting (Setting nameV defaultV) = get >>= put . M.insert nameV SettingInfo { value = show defaultV, userSet = False }
setting (ListSetting nameV defaultV) = get >>= put . addListSettings False nameV defaultV 1

-- | Used in combination with 'setting' to register settings.
-- Registering settings is optional, see 'DefaultConfig'.
--
-- @
-- defaultSettings :: DefaultConfig
-- defaultSettings = getDefaultConfig $ do
--     setting \<setting1\>
--     setting \<setting2\>
-- @
getDefaultConfig :: State Conf () -> DefaultConfig
getDefaultConfig actions = DefaultConfig $ execState actions M.empty

-- | Where to look for or store the configuration file.
data FileLocation = AutoFromAppName String
		    -- ^ Automatically build the location based on the
		    -- application name. It will be ~\/.\<app name\>\/config.ini.
		  | Path FilePath
		    -- ^ Absolute path to a location on disk.

getPathForLocation :: FileLocation -> IO FilePath
getPathForLocation location = case location of
	AutoFromAppName appName -> getConfigFileName appName
	Path path -> return path

-- | Read settings from disk.
-- Because it is doing file I/O it is smart to wrap the call
-- with a 'try', as I/O exceptions can be thrown.
-- Also, the function will throw a 'ParseException' if the
-- file is not properly formatted.
-- NOTE that if the file is properly formatted in general,
-- but a value is stored in an invalid format (for instance \"hello\"
-- for a Double), you will get no error and get the default value
-- for that setting when you attempt to read it.
--
-- This function returns a pair. The first element
-- is the configuration itself, which you can use to save
-- back or modify the configuration.
-- The second element is a function wrapped in the 'GetSetting'
-- newtype. This function allows you to read a configuration
-- option simply by giving that option (without that callback
-- you'd have to call getSetting settings \<setting\>, so
-- the callback lets you save a parameter).
-- There is no such shortcut for 'setSetting' though, as it's
-- normally used less often and in other contexts, it is probably
-- OK to have that extra parameter for the setSetting.
--
-- Example of use:
--
-- @
-- readResult <- try $ readSettings (Path \"my.config\")
-- case readResult of
-- 	Right (conf, GetSetting getSetting) -> do
-- 		let textSize = getSetting fontSize
-- 		saveSettings emptyDefaultConfig (Path \"my.config\") conf
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
	writeConfigFile filePath (conf `M.union` defaults)

-- | Change the value of a setting. You'll have to call
-- 'saveSettings' so that the change is written to disk.
setSetting :: (Show a) => Conf -> Setting a -> a -> Conf
setSetting conf (Setting key _) v = M.insert key SettingInfo { value = show v, userSet=True } conf
-- an empty list for the XX list key is written as XX= in the config file.
setSetting conf (ListSetting key _) [] = M.insert key SettingInfo { value = "", userSet= True}
	$ cleanListSetting key conf
setSetting conf (ListSetting key _) elts = addListSettings True key elts 1
	$ cleanListSetting key conf

addListSettings :: Show b => Bool -> String -> [b] -> Int -> Conf -> Conf
addListSettings _ _ [] _ = id
addListSettings uset key (x:xs) index = M.insert keyForIndex SettingInfo { value = show x, userSet=uset }
		. addListSettings uset key xs (index+1)
	where keyForIndex = key ++ "_" ++ show index

cleanListSetting :: String -> Conf -> Conf
cleanListSetting key = M.filterWithKey (\k _ -> not $ isKeyForListSetting key k)

getSettingsFolder :: String -> IO FilePath
getSettingsFolder appName = do
	home <- getHomeDirectory
	let result = home ++ "/." ++ appName ++ "/"
	createDirectoryIfMissing False result
	return result

getConfigFileName :: String -> IO String
getConfigFileName appName = (++"config.ini") <$> getSettingsFolder appName

-- $intro
--
-- A library to deal with application settings.
-- This library deals with read-write application settings.
-- You will have to specify the settings that your application
-- uses, their name, types and default values.
-- Setting types must implement the 'Read' and 'Show' typeclasses. 
--
-- The settings are saved in a file in an INI-like key-value format
-- (without sections).
--
-- Reading and updating settings is done in pure code, the IO
-- monad is only used to load settings and save them to disk.
-- It is advised for the user to create a module in your project
-- holding settings handling.
--
-- You can then declare settings:
--
-- > fontSize :: Setting Double
-- > fontSize = Setting "fontSize" 14
-- > 
-- > dateFormat :: Setting String
-- > dateFormat = Setting "dateFormat" "%x"
-- > 
-- > backgroundColor :: Setting (Int, Int, Int)
-- > backgroundColor = Setting "backcolor" (255, 0, 0)
--
-- Optionally you can declare the list of all your settings:
--
-- > defaultConfig :: DefaultConfig
-- > defaultConfig = getDefaultConfig $ do
-- >     setting fontSize
-- >     setting dateFormat
-- >     setting backgroundColor
--
-- If you do it, 'saveSettings' will also save settings
-- which have not been modified, which are still at their
-- default value in the configuration file, in a commented
-- form, as a documentation to the user who may open the
-- configuration file.
-- So for instance if you declare this default configuration
-- and have set the font size to 16 but left the other
-- settings untouched, the configuration file which will be
-- saved will be:
--
-- > fontSize=16
-- > # dateFormat="%x"
-- > # backcolor=(255,0,0)
--
-- If you did not specify the list of settings, only the
-- first line would be present in the configuration file.
--
--  With an ordinary setting, one row in the configuration file
--  means one setting. That setting may of course be a list
--  for instance. This setup works very well for shorter lists
--  like [1,2,3], however if you have a list of more complex
--  items, you will get very long lines and a configuration
--  file very difficult to edit by hand.
--  For these special cases there is also the 'ListSetting'
--
--  constructor:
--
--  > testList :: Setting [String]
--  > testList = ListSetting "testList" ["list1", "list2", "list3"]
--
--  Now the configuration file looks like that:
--
--  > testList_1="list1"
--  > testList_2="list2"
--  > testList_3="list3"
--
--  Which is much more handy for big lists. An empty list is represented
--  like so:
--
--  > testList=
--
-- Once we declared the settings, we can read the configuration
-- from disk (and your settings module should export your wrapper
-- around the function offered by this library):
--
-- > readResult <- try $ readSettings (AutoFromAppName "test")
-- > case readResult of
-- > 	Right (conf, GetSetting getSetting) -> do
-- > 		let textSize = getSetting fontSize
-- > 		saveSettings emptyDefaultConfig (AutoFromAppName "test") conf
-- > 	Left (x :: SomeException) -> error "Error reading the config file!"
--
-- 'AutoFromAppName' specifies where to save the configuration file.
-- And we've already covered the getSetting in this snippet, see 
-- the 'readSettings' documentation for further information.
-- 
-- You can also look at the tests of the library on the github project for sample use.
