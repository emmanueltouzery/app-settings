{-# LANGUAGE GADTs #-}

module Data.AppSettingsInternal where

import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import qualified Data.Map as M

import Data.Serialization

-- I can't put the comments on the constructors:
-- http://trac.haskell.org/haddock/ticket/43

-- | The type of a setting.
-- It contains the setting name
-- (key in the configuration file) and its default value.
--
-- It is advised to have a module in your project handling settings.
-- In this module, you'd have all the settings declared at the
-- toplevel, and exported.
-- The rest of the application can then do
--
-- @
-- getSetting \<setting\>
-- setSetting \<conf\> \<setting\> \<value\>
-- @
--
-- and so on.
--
-- 'Setting' declares a simple setting. A value for that setting will be stored
-- in the configuration file in a single line.
--
-- 'ListSetting' however declares a list setting.
-- While it is perfectly fine to store lists
-- using the usual Setting constructor, if you have a list
-- of more complex items, you will get very long lines and a
-- configuration file very difficult to edit or review by hand.
--
-- The ListSetting will store settings using one line per item
-- in the list:
--
-- > testList :: Setting [String]
-- > testList = ListSetting "testList" ["list1", "list2", "list3"]
--
-- Now the configuration file looks like that:
--
-- > testList_1="list1"
-- > testList_2="list2"
-- > testList_3="list3"
--
-- Also note that an empty ListSetting is stored like so:
--
-- > testList=
data Setting a where
	Setting :: String -> a -> Setting a

	ListSetting :: (Read a, Show a) => String -> [a] -> Setting [a]

isKeyForListSetting :: String -> String -> Bool
isKeyForListSetting settingKey key = (settingKey ++ "_") `isPrefixOf` key
	&& (all isDigit $ drop (length settingKey+1) key)


-- TODO maybe another getSetting that'll tell you
-- if the setting is invalid in the config file instead of silently
-- give you the default?

-- | More low-level, please use the second function you get from 'readSettings',
-- wrapped in a 'GetSetting' newtype.
getSetting' :: (Read a) => Conf -> Setting a -> a
getSetting' conf (Setting key defaultV) = fromMaybe defaultV $ getSettingValueFromConf conf key
getSetting' conf (ListSetting key defaultV) = case getSettingValueFromConf conf $ key ++ "_1" of
	-- an empty list for the XX list key is written as XX= in the config file.
	Nothing -> case M.lookup key conf of
		Nothing -> defaultV
		_ -> []
	Just x -> x : decodeListSetting conf key 2

getSettingValueFromConf :: Read a => Conf -> String -> Maybe a
getSettingValueFromConf conf key = do
	asString <- M.lookup key conf
	readMaybe $ value asString

decodeListSetting  :: Read a => Conf -> String -> Int -> [a]
decodeListSetting conf key index = case getSettingValueFromConf conf fullKey of
		Nothing -> []
		Just v -> v:decodeListSetting conf key (index+1)
	where fullKey = key ++ "_" ++ show index
