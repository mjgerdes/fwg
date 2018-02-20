
module FWG.Tag where

import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Data.Text as T



data Tag = Tag { _name :: Text, _requirements :: [TagRequirement], _contents :: [TagContent] } 
  deriving (Eq, Ord, Show)

data TagRequirement = All [Text] | Any [Text] | NoneOf [Text]
  deriving (Show, Eq, Ord)

data TagContent = 
  Description Text 
  | Places [Text] 
  | Creatures [Text]
  | Things [Text]
  | Adventures [Text]
  | Complications [Text]
  deriving (Show, Eq, Ord)

predicateFromRequirement :: TagRequirement -> (Text -> Bool)
predicateFromRequirement req w = f req (T.toLower w)
  where f (All ws) w = all (==w) $ lc ws
  	f (Any ws) w = any (==w) $ lc ws
	f (NoneOf ws) w = all (/=w) $ lc ws
  	lc = fmap T.toLower


predicateFromRequirements :: [TagRequirement] -> (Text -> Bool)
predicateFromRequirements rs w = and $ fmap (($ w) . predicateFromRequirement) rs

