{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- https://api.rocketleaguegame.com/docs/
module RocketLeagueApi where

import Data.Aeson.Types
import Data.Map (Map)
import Data.Monoid
import Data.Proxy
import Data.Scientific
import Data.Set (Set)
import Data.Text (Text)
import Servant.API
import Servant.Client

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

getPlayerSkills :: Platform -> PlayerId -> Maybe Token -> ClientM (Single Player)
postPlayerSkills :: Platform -> PlayerIds -> Maybe Token -> ClientM [Player]
getPlayerTitles :: Platform -> PlayerId -> Maybe Token -> ClientM [Title]
getPopulation :: Maybe Token -> ClientM (Map Platform [Population])
getRegions :: Maybe Token -> ClientM [RegionInfo]
getSkillsLeaderboard :: Platform -> Playlist -> Maybe Token -> ClientM [Skill]
getStatsLeaderboard :: Platform -> Maybe Token -> ClientM [Stats]
getStatLeaderboard :: Platform -> StatType -> Maybe Token -> ClientM (Single Stats)
getPlayerStat :: Platform -> StatType -> PlayerId -> Maybe Token -> ClientM (Single Stat)
postPlayerStat :: Platform -> StatType -> PlayerIds -> Maybe Token -> ClientM (Single Stat)

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
  = CapturePlatform :> "playerskills" :> CapturePlayerId :> Endpoint Get (Single Player)
  :<|> CapturePlatform :> "playerskills" :> BodyPlayerIds :> Endpoint Post [Player]
  :<|> CapturePlatform :> "playertitles" :> CapturePlayerId :> Endpoint Get [Title]
  :<|> "population" :> Endpoint Get (Map Platform [Population])
  :<|> "regions" :> Endpoint Get [RegionInfo]
  :<|> CapturePlatform :> "leaderboard" :> "skills" :> CapturePlaylist :> Endpoint Get [Skill]
  :<|> CapturePlatform :> "leaderboard" :> "stats" :> Endpoint Get [Stats]
  :<|> CapturePlatform :> "leaderboard" :> "stats" :> CaptureStatType :> Endpoint Get (Single Stats)
  :<|> CapturePlatform :> "leaderboard" :> "stats" :> CaptureStatType :> CapturePlayerId :> Endpoint Get (Single Stat)
  :<|> CapturePlatform :> "leaderboard" :> "stats" :> CaptureStatType :> BodyPlayerIds :> Endpoint Post (Single Stat)

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
  toUrlPiece token = Text.pack "Token " <> tokenValue token

data Platform
  = PlatformPS4
  | PlatformSteam
  | PlatformXboxOne
  deriving (Eq, Ord, Show)

instance FromJSON Platform where
  parseJSON = withText "Platform" textToPlatform

instance FromJSONKey Platform where
  fromJSONKey = FromJSONKeyTextParser textToPlatform

instance ToHttpApiData Platform where
  toUrlPiece platform = case platform of
    PlatformPS4 -> "ps4"
    PlatformSteam -> "steam"
    PlatformXboxOne -> "xboxone"

textToPlatform :: Text -> Parser Platform
textToPlatform t = case t of
  "PS4" -> pure PlatformPS4
  "Steam" -> pure PlatformSteam
  "XboxOne" -> pure PlatformXboxOne
  _ -> fail $ "invalid Platform: " ++ show t

data PlayerId
  = PlayerIdPS4 Text
  | PlayerIdSteam Integer
  | PlayerIdXboxOne Text
  deriving (Eq, Ord, Show)

instance FromJSON PlayerId where
  parseJSON v = case v of
    Number n -> case floatingOrInteger n of
      Left f -> fail $ "invalid PlayerId: " ++ show (f :: Float)
      Right i -> pure $ PlayerIdSteam i
    String s -> pure $ PlayerIdPS4 s -- TODO
    _ -> fail $ "invalid PlayerId: " ++ show v

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

instance FromJSON StatType where
  parseJSON = withText "StatType" $ \t -> case t of
    "assists" -> pure StatTypeAssists
    "goals" -> pure StatTypeGoals
    "mvps" -> pure StatTypeMvps
    "saves" -> pure StatTypeSaves
    "shots" -> pure StatTypeShots
    "wins" -> pure StatTypeWins
    _ -> fail $ "invalid StatType: " ++ show t

instance ToHttpApiData StatType where
  toUrlPiece statType = case statType of
    StatTypeAssists -> "assists"
    StatTypeGoals -> "goals"
    StatTypeMvps -> "mvps"
    StatTypeSaves -> "saves"
    StatTypeShots -> "shots"
    StatTypeWins -> "wins"

data Population = Population
  { populationNumPlayers :: Integer
  , populationPlaylistId :: Integer
  } deriving (Eq, Ord, Show)

instance FromJSON Population where
  parseJSON = withObject "Population" $ \o -> do
    numPlayers <- o .: "NumPlayers"
    playlistId <- o .: "PlaylistID"
    pure Population
      { populationNumPlayers = numPlayers
      , populationPlaylistId = playlistId
      }

data RegionInfo = RegionInfo
  { regionInfoRegion :: Region
  , regionInfoPlatforms :: Platforms
  } deriving (Eq, Ord, Show)

instance FromJSON RegionInfo where
  parseJSON = withObject "RegionInfo" $ \o -> do
    region <- o .: "region"
    platforms <- o .: "platforms"
    pure RegionInfo
      { regionInfoRegion = region
      , regionInfoPlatforms = platforms
      }

data Region
  = RegionASC
  | RegionEU
  | RegionJPN
  | RegionME
  | RegionOCE
  | RegionSAM
  | RegionUSE
  | RegionUSW
  deriving (Eq, Ord, Show)

instance FromJSON Region where
  parseJSON = withText "Region" $ \t -> case t of
    "ASC" -> pure RegionASC
    "EU" -> pure RegionEU
    "JPN" -> pure RegionJPN
    "ME" -> pure RegionME
    "OCE" -> pure RegionOCE
    "SAM" -> pure RegionSAM
    "USE" -> pure RegionUSE
    "USW" -> pure RegionUSW
    _ -> fail $ "invalid Region: " ++ show t

newtype Platforms = Platforms
  { platformsValue :: Set Platform
  } deriving (Eq, Ord, Show)

instance FromJSON Platforms where
  parseJSON = withText "Platforms" $ \t -> do
    platforms <- mapM textToPlatform $ Text.splitOn "," t
    pure Platforms
      { platformsValue = Set.fromList platforms
      }

newtype Title = Title
  { titleValue :: Text
  } deriving (Eq, Ord, Show)

instance FromJSON Title where
  parseJSON = withObject "Title" $ \o -> do
    title <- o .: "title"
    pure Title
      { titleValue = title
      }

data Skill = Skill
  { skillSkill :: Integer
  , skillTier :: Integer
  , skillUserId :: PlayerId
  , skillUserName :: Text
  } deriving (Eq, Ord, Show)

instance FromJSON Skill where
  parseJSON = withObject "Skill" $ \o -> do
    skill <- o .: "skill"
    tier <- o .: "tier"
    userId <- o .: "user_id"
    userName <- o .: "user_name"
    pure Skill
      { skillSkill = skill
      , skillTier = tier
      , skillUserId = userId
      , skillUserName = userName
      }

data Stat = Stat
  { statStatType :: StatType
  , statUserId :: PlayerId
  , statUserName :: Text
  , statValue :: Integer
  } deriving (Eq, Ord, Show)

instance FromJSON Stat where
  parseJSON = withObject "Stat" $ \o -> do
    statType <- o .: "stat_type"
    userId <- o .: "user_id"
    userName <- o .: "user_name"
    value <- o .: "value"
    pure Stat
      { statStatType = statType
      , statUserId = userId
      , statUserName = userName
      , statValue = value
      }

newtype Single a = Single
  { singleValue :: a
  } deriving (Eq, Ord, Show)

instance FromJSON a => FromJSON (Single a) where
  parseJSON = withArray "Single" $ \a -> case Vector.toList a of
    [v] -> do
      x <- parseJSON v
      pure Single
        { singleValue = x
        }
    _ -> fail $ "invalid Single: " ++ show a

data Stats = Stats
  { statsStatType :: StatType
  , statsStats :: [TypedStat]
  } deriving (Eq, Ord, Show)

instance FromJSON Stats where
  parseJSON = withObject "Stats" $ \o -> do
    statType <- o .: "stat_type"
    stats <- o .: "stats"
    pure Stats
      { statsStatType = statType
      , statsStats = stats
      }

newtype TypedStat = TypedStat
  { typedStatValue :: Stat
  } deriving (Eq, Ord, Show)

instance FromJSON TypedStat where
  parseJSON = withObject "TypedStat" $ \o -> do
    userId <- o .: "user_id"
    userName <- o .: "user_name"
    maybeAssists <- o .:? "assists"
    maybeGoals <- o .:? "goals"
    maybeMvps <- o .:? "mvps"
    maybeSaves <- o .:? "saves"
    maybeShots <- o .:? "shots"
    maybeWins <- o .:? "wins"
    stat <- case (maybeAssists, maybeGoals, maybeMvps, maybeSaves, maybeShots, maybeWins) of
      (Just assists, Nothing, Nothing, Nothing, Nothing, Nothing) -> pure Stat
        { statUserId = userId
        , statUserName = userName
        , statStatType = StatTypeAssists
        , statValue = assists
        }
      (Nothing, Just goals, Nothing, Nothing, Nothing, Nothing) -> pure Stat
        { statUserId = userId
        , statUserName = userName
        , statStatType = StatTypeGoals
        , statValue = goals
        }
      (Nothing, Nothing, Just mvps, Nothing, Nothing, Nothing) -> pure Stat
        { statUserId = userId
        , statUserName = userName
        , statStatType = StatTypeMvps
        , statValue = mvps
        }
      (Nothing, Nothing, Nothing, Just saves, Nothing, Nothing) -> pure Stat
        { statUserId = userId
        , statUserName = userName
        , statStatType = StatTypeSaves
        , statValue = saves
        }
      (Nothing, Nothing, Nothing, Nothing, Just shots, Nothing) -> pure Stat
        { statUserId = userId
        , statUserName = userName
        , statStatType = StatTypeShots
        , statValue = shots
        }
      (Nothing, Nothing, Nothing, Nothing, Nothing, Just wins) -> pure Stat
        { statUserId = userId
        , statUserName = userName
        , statStatType = StatTypeWins
        , statValue = wins
        }
      _ -> fail $ "invalid TypedStat: " ++ show o
    pure TypedStat
      { typedStatValue = stat
      }

data Player = Player
  { playerUserId :: PlayerId
  , playerUserName :: Text
  , playerPlayerSkills :: [PlayerSkill]
  } deriving (Eq, Ord, Show)

instance FromJSON Player where
  parseJSON = withObject "Player" $ \o -> do
    userId <- o .: "user_id"
    userName <- o .: "user_name"
    playerSkills <- o .: "player_skills"
    pure Player
      { playerUserId = userId
      , playerUserName = userName
      , playerPlayerSkills = playerSkills
      }

data PlayerSkill = PlayerSkill
  { playerSkillDivision :: Integer
  , playerSkillMatchesPlayed :: Integer
  , playerSkillPlaylist :: Integer
  , playerSkillSkill :: Integer
  , playerSkillTier :: Integer
  , playerSkillTierMax :: Integer
  } deriving (Eq, Ord, Show)

instance FromJSON PlayerSkill where
  parseJSON = withObject "PlayerSkill" $ \o -> do
    division <- o .: "division"
    matchesPlayed <- o .: "matches_played"
    playlist <- o .: "playlist"
    skill <- o .: "skill"
    tier <- o .: "tier"
    tierMax <- o .: "tier_max"
    pure PlayerSkill
      { playerSkillDivision = division
      , playerSkillMatchesPlayed = matchesPlayed
      , playerSkillPlaylist = playlist
      , playerSkillSkill = skill
      , playerSkillTier = tier
      , playerSkillTierMax = tierMax
      }
