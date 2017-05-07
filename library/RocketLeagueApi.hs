{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- https://api.rocketleaguegame.com/docs/
module RocketLeagueApi where

import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client

getPlayerSkills :: Text -> Text -> Maybe Token -> ClientM Value
postPlayerSkills :: Text -> Value -> Maybe Token -> ClientM Value
getPlayerTitles :: Text -> Text -> Maybe Token -> ClientM Value
getPopulation :: Maybe Token -> ClientM Value
getRegions :: Maybe Token -> ClientM Value
getSkillsLeaderboard :: Text -> Text -> Maybe Token -> ClientM Value
getStatsLeaderboard :: Text -> Maybe Token -> ClientM Value
getStatLeaderboard :: Text -> Text -> Maybe Token -> ClientM Value
getPlayerStat :: Text -> Text -> Text -> Maybe Token -> ClientM Value
postPlayerStat :: Text -> Text -> Value -> Maybe Token -> ClientM Value

getPlayerSkills
  :<|> postPlayerSkills
  :<|> getPlayerTitles
  :<|> getPopulation
  :<|> getRegions
  :<|> getSkillsLeaderboard
  :<|> getStatsLeaderboard
  :<|> getStatLeaderboard
  :<|> getPlayerStat
  :<|> postPlayerStat
  = client api

api :: Proxy Api
api = Proxy

type Api
  = Capture "platform" Text :> "playerskills" :> Capture "player_id" Text :> Endpoint Get Value
  :<|> Capture "platform" Text :> "playerskills" :> ReqBody '[JSON] Value :> Endpoint Post Value
  :<|> Capture "platform" Text :> "playertitles" :> Capture "player_id" Text :> Endpoint Get Value
  :<|> "population" :> Endpoint Get Value
  :<|> "regions" :> Endpoint Get Value
  :<|> Capture "platform" Text :> "leaderboard" :> "skills" :> Capture "playlist" Text :> Endpoint Get Value
  :<|> Capture "platform" Text :> "leaderboard" :> "stats" :> Endpoint Get Value
  :<|> Capture "platform" Text :> "leaderboard" :> "stats" :> Capture "stat_type" Text :> Endpoint Get Value
  :<|> Capture "platform" Text :> "leaderboard" :> "stats" :> Capture "stat_type" Text :> Capture "player_id" Text :> Endpoint Get Value
  :<|> Capture "platform" Text :> "leaderboard" :> "stats" :> Capture "stat_type" Text :> ReqBody '[JSON] Value :> Endpoint Post Value

type Endpoint method result
  = Header "Authorization" Token :> method '[JSON] result

newtype Token = Token
  { tokenValue :: Text
  } deriving (Eq, Show)

instance ToHttpApiData Token where
  toUrlPiece token = pack "Token " <> tokenValue token
