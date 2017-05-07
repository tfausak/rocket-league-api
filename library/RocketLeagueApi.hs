{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- https://api.rocketleaguegame.com/docs/
module RocketLeagueApi where

import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Set
import Data.Text
import Servant.API
import Servant.Client

getPlayerSkills :: Platform -> PlayerId -> Maybe Token -> ClientM Value
postPlayerSkills :: Platform -> PlayerIds -> Maybe Token -> ClientM Value
getPlayerTitles :: Platform -> PlayerId -> Maybe Token -> ClientM Value
getPopulation :: Maybe Token -> ClientM Value
getRegions :: Maybe Token -> ClientM Value
getSkillsLeaderboard :: Platform -> Playlist -> Maybe Token -> ClientM Value
getStatsLeaderboard :: Platform -> Maybe Token -> ClientM Value
getStatLeaderboard :: Platform -> StatType -> Maybe Token -> ClientM Value
getPlayerStat :: Platform -> StatType -> PlayerId -> Maybe Token -> ClientM Value
postPlayerStat :: Platform -> StatType -> PlayerIds -> Maybe Token -> ClientM Value

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
  = CapturePlatform :> "playerskills" :> CapturePlayerId :> Endpoint Get Value
  :<|> CapturePlatform :> "playerskills" :> BodyPlayerIds :> Endpoint Post Value
  :<|> CapturePlatform :> "playertitles" :> CapturePlayerId :> Endpoint Get Value
  :<|> "population" :> Endpoint Get Value
  :<|> "regions" :> Endpoint Get Value
  :<|> CapturePlatform :> "leaderboard" :> "skills" :> CapturePlaylist :> Endpoint Get Value
  :<|> CapturePlatform :> "leaderboard" :> "stats" :> Endpoint Get Value
  :<|> CapturePlatform :> "leaderboard" :> "stats" :> CaptureStatType :> Endpoint Get Value
  :<|> CapturePlatform :> "leaderboard" :> "stats" :> CaptureStatType :> CapturePlayerId :> Endpoint Get Value
  :<|> CapturePlatform :> "leaderboard" :> "stats" :> CaptureStatType :> BodyPlayerIds :> Endpoint Post Value

type Endpoint method result
  = Header "Authorization" Token :> method '[JSON] result

type CapturePlatform = Capture "platform" Platform
type CapturePlayerId = Capture "player_id" PlayerId
type BodyPlayerIds = ReqBody '[JSON] PlayerIds
type CapturePlaylist = Capture "playlist" Playlist
type CaptureStatType = Capture "stat_type" StatType

newtype Token = Token
  { tokenValue :: Text
  } deriving (Eq, Ord, Show)

instance ToHttpApiData Token where
  toUrlPiece token = pack "Token " <> tokenValue token

data Platform
  = PlatformPS4
  | PlatformSteam
  | PlatformXboxOne
  deriving (Eq, Ord, Show)

instance ToHttpApiData Platform where
  toUrlPiece platform = case platform of
    PlatformPS4 -> "ps4"
    PlatformSteam -> "steam"
    PlatformXboxOne -> "xboxone"

data PlayerId
  = PlayerIdPS4 Text
  | PlayerIdSteam Integer
  | PlayerIdXboxOne Text
  deriving (Eq, Ord, Show)

instance ToHttpApiData PlayerId where
  toUrlPiece playerId = case playerId of
    PlayerIdPS4 x -> toUrlPiece x
    PlayerIdSteam x -> toUrlPiece x
    PlayerIdXboxOne x -> toUrlPiece x

instance ToJSON PlayerId where
  toJSON playerId = case playerId of
    PlayerIdPS4 x -> toJSON x
    PlayerIdSteam x -> toJSON x
    PlayerIdXboxOne x -> toJSON x

newtype PlayerIds = PlayerIds
  { playerIdsValue :: Set PlayerId
  } deriving (Eq, Ord, Show)

instance ToJSON PlayerIds where
  toJSON playerIds = object
    [ "player_ids" .= playerIdsValue playerIds
    ]

data Playlist
  = PlaylistCompetitiveSoloDuel
  | PlaylistCompetitiveDoubles
  | PlaylistCompetitiveSoloStandard
  | PlaylistCompetitiveStandard
  deriving (Eq, Ord, Show)

instance ToHttpApiData Playlist where
  toUrlPiece playlist = case playlist of
    PlaylistCompetitiveSoloDuel -> "10"
    PlaylistCompetitiveDoubles -> "11"
    PlaylistCompetitiveSoloStandard -> "12"
    PlaylistCompetitiveStandard -> "13"

data StatType
  = StatTypeAssists
  | StatTypeGoals
  | StatTypeMvps
  | StatTypeSaves
  | StatTypeShots
  | StatTypeWins
  deriving (Eq, Ord, Show)

instance ToHttpApiData StatType where
  toUrlPiece statType = case statType of
    StatTypeAssists -> "assists"
    StatTypeGoals -> "goals"
    StatTypeMvps -> "mvps"
    StatTypeSaves -> "saves"
    StatTypeShots -> "shots"
    StatTypeWins -> "wins"
