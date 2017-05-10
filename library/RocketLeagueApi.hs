{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- https://api.rocketleaguegame.com/docs/
module RocketLeagueApi where

import Data.Aeson.Types
import Data.List
import Data.Map (Map)
import Data.Monoid
import Data.Proxy
import Data.Scientific
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Client
import Servant.Docs hiding (Endpoint)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

getPlayerSkills :: Platform -> PlayerId -> Maybe Token -> ClientM (Single Player)
postPlayerSkills :: Platform -> PlayerIds -> Maybe Token -> ClientM [Player]
getPlayerTitles :: Platform -> PlayerId -> Maybe Token -> ClientM [Title]
getPopulation :: Maybe Token -> ClientM Populations
getRegions :: Maybe Token -> ClientM [RegionInfo]
getSkillsLeaderboard :: Platform -> Playlist -> Maybe Token -> ClientM [Skill]
getStatsLeaderboard :: Platform -> Maybe Token -> ClientM [Stats]
getStatLeaderboard :: Platform -> StatType -> Maybe Token -> ClientM (Single Stats)
getPlayerStat :: Platform -> StatType -> PlayerId -> Maybe Token -> ClientM (Single Stat)
postPlayerStat :: Platform -> StatType -> PlayerIds -> Maybe Token -> ClientM [Stat]

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
  = GetPlayerSkills
  :<|> PostPlayerSkills
  :<|> GetPlayerTitles
  :<|> GetPopulation
  :<|> GetRegions
  :<|> GetSkillsLeaderboard
  :<|> GetStatsLeaderboard
  :<|> GetStatLeaderboard
  :<|> GetPlayerStat
  :<|> PostPlayerStat

type GetPlayerSkills = CapturePlatform :> "playerskills" :> CapturePlayerId :> Endpoint Get (Single Player)
type PostPlayerSkills = CapturePlatform :> "playerskills" :> BodyPlayerIds :> Endpoint Post [Player]
type GetPlayerTitles = CapturePlatform :> "playertitles" :> CapturePlayerId :> Endpoint Get [Title]
type GetPopulation = "population" :> Endpoint Get Populations
type GetRegions = "regions" :> Endpoint Get [RegionInfo]
type GetSkillsLeaderboard = CapturePlatform :> "leaderboard" :> "skills" :> CapturePlaylist :> Endpoint Get [Skill]
type GetStatsLeaderboard = CapturePlatform :> "leaderboard" :> "stats" :> Endpoint Get [Stats]
type GetStatLeaderboard = CapturePlatform :> "leaderboard" :> "stats" :> CaptureStatType :> Endpoint Get (Single Stats)
type GetPlayerStat = CapturePlatform :> "leaderboard" :> "stats" :> CaptureStatType :> CapturePlayerId :> Endpoint Get (Single Stat)
type PostPlayerStat = CapturePlatform :> "leaderboard" :> "stats" :> CaptureStatType :> BodyPlayerIds :> Endpoint Post [Stat]

type Endpoint method result
  = Header "Authorization" Token :> method '[JSON] result

type CapturePlatform = Capture "platform" Platform
type CapturePlayerId = Capture "player_id" PlayerId
type BodyPlayerIds = ReqBody '[JSON] PlayerIds
type CapturePlaylist = Capture "playlist" Playlist
type CaptureStatType = Capture "stat_type" StatType

instance ToCapture CapturePlatform where
  toCapture _ = DocCapture "platform" "The platform."

instance ToCapture CapturePlayerId where
  toCapture _ = DocCapture "player_id" "The player ID."

instance ToCapture CapturePlaylist where
  toCapture _ = DocCapture "playlist" "The playlist."

instance ToCapture CaptureStatType where
  toCapture _ = DocCapture "stat_type" "The stat type."

newtype Token = Token
  { tokenValue :: Text
  } deriving (Eq, Ord, Show)

instance ToHttpApiData Token where
  toUrlPiece token = Text.pack "Token " <> tokenValue token

data Platform
  = PlatformPS4
  | PlatformSteam
  | PlatformXboxOne
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON Platform where
  parseJSON = genericParseJSON $ pascalConstructorOptions "Platform"

instance FromJSONKey Platform where
  fromJSONKey = FromJSONKeyTextParser (parseJSON . String)

instance ToHttpApiData Platform where
  toUrlPiece = Text.toLower . Text.pack . pascalModifier "Platform" . show

instance ToJSON Platform where
  toJSON = String . toUrlPiece

instance ToJSONKey Platform where
  toJSONKey = toJSONKeyText toUrlPiece

instance ToSample Platform where
  toSamples _ = samples [minBound .. maxBound]

newtype PlayerId = PlayerId
  { playerIdValue :: Text
  } deriving (Eq, Ord, Show)

instance FromJSON PlayerId where
  parseJSON v = case v of
    Number n -> case floatingOrInteger n of
      Left (f :: Float) -> fail $ "invalid PlayerId: " ++ show f
      Right (i :: Integer) -> pure PlayerId { playerIdValue = Text.pack $ show i }
    String s -> pure PlayerId { playerIdValue = s }
    _ -> fail $ "invalid PlayerId: " ++ show v

instance ToHttpApiData PlayerId where
  toUrlPiece = playerIdValue

instance ToJSON PlayerId where
  toJSON = toJSON . playerIdValue

instance ToSample PlayerId where
  toSamples _ =
    [ ("PlayStation 4", PlayerId "Iris_-lily-_")
    , ("Steam", PlayerId "76561198149461762")
    , ("Xbox One", PlayerId "Best Mr Pedro")
    ]

newtype PlayerIds = PlayerIds
  { playerIdsValue :: Set PlayerId
  } deriving (Eq, Ord, Show)

instance ToJSON PlayerIds where
  toJSON playerIds = object
    [ "player_ids" .= playerIdsValue playerIds
    ]

instance ToSample PlayerIds where
  toSamples _ =
    [ ("empty", PlayerIds Set.empty)
    , ("PlayStation 4", PlayerIds . Set.singleton $ PlayerId "Iris_-lily-_")
    , ("Steam", PlayerIds . Set.singleton $ PlayerId "76561198149461762")
    , ("Xbox One", PlayerIds . Set.singleton $ PlayerId "Best Mr Pedro")
    , ("multiple", PlayerIds $ Set.fromList [PlayerId "76561198067659334", PlayerId "76561198213713880"])
    ]

data Playlist
  = PlaylistCompetitiveSoloDuel
  | PlaylistCompetitiveDoubles
  | PlaylistCompetitiveSoloStandard
  | PlaylistCompetitiveStandard
  deriving (Bounded, Enum, Eq, Ord, Show)

instance ToHttpApiData Playlist where
  toUrlPiece = Text.pack . show . (+ 10) . fromEnum

data StatType
  = StatTypeAssists
  | StatTypeGoals
  | StatTypeMvps
  | StatTypeSaves
  | StatTypeShots
  | StatTypeWins
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON StatType where
  parseJSON = genericParseJSON $ snakeConstructorOptions "StatType"

instance ToHttpApiData StatType where
  toUrlPiece = Text.pack . snakeModifier "StatType" . show

instance ToJSON StatType where
  toJSON = genericToJSON $ snakeConstructorOptions "StatType"

newtype Populations = Populations
  { populationsValue :: Map Platform [Population]
  } deriving (Eq, Ord, Show)

instance FromJSON Populations where
  parseJSON v = do
    value <- parseJSON v
    pure Populations
      { populationsValue = value
      }

instance ToJSON Populations where
  toJSON = toJSON . populationsValue

instance ToSample Populations where
  toSamples _ = singleSample . Populations $ Map.fromList
    [ (PlatformPS4, [Population 5646 11])
    , (PlatformSteam, [Population 4413 11])
    , (PlatformXboxOne, [Population 1513 11])
    ]

data Population = Population
  { populationNumPlayers :: Integer
  , populationPlaylistID :: Integer
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Population where
  parseJSON = genericParseJSON $ pascalFieldOptions "population"

instance ToJSON Population where
  toJSON = genericToJSON $ pascalFieldOptions "population"

instance ToSample Population where
  toSamples _ =
    [ ("Competitive Duel", Population 322 10)
    , ("Competitive Doubles", Population 1513 11)
    , ("Competitive Solo Standard", Population 224 12)
    , ("Competitive Standard", Population 595 13)
    ]

data RegionInfo = RegionInfo
  { regionInfoRegion :: Region
  , regionInfoPlatforms :: Platforms
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON RegionInfo where
  parseJSON = genericParseJSON $ snakeFieldOptions "regionInfo"

instance ToJSON RegionInfo where
  toJSON = genericToJSON $ snakeFieldOptions "regionInfo"

instance ToSample RegionInfo where
  toSamples _ = singleSample . RegionInfo RegionUSE . Platforms $ Set.fromList [minBound .. maxBound]

data Region
  = RegionASC
  | RegionEU
  | RegionJPN
  | RegionME
  | RegionOCE
  | RegionSAM
  | RegionUSE
  | RegionUSW
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON Region where
  parseJSON = genericParseJSON $ pascalConstructorOptions "Region"

instance ToJSON Region where
  toJSON = genericToJSON $ pascalConstructorOptions "Region"

newtype Platforms = Platforms
  { platformsValue :: Set Platform
  } deriving (Eq, Ord, Show)

instance FromJSON Platforms where
  parseJSON = withText "Platforms" $ \t -> do
    platforms <- mapM (parseJSON . String) $ Text.splitOn "," t
    pure Platforms
      { platformsValue = Set.fromList platforms
      }

instance ToJSON Platforms where
  toJSON = toJSON . Text.intercalate "," . map toUrlPiece . Set.toAscList . platformsValue

newtype Title = Title
  { titleValue :: Text
  } deriving (Eq, Ord, Show)

instance FromJSON Title where
  parseJSON = withObject "Title" $ \o -> do
    title <- o .: "title"
    pure Title
      { titleValue = title
      }

instance ToJSON Title where
  toJSON title = object ["title" .= titleValue title]

instance ToSample Title where
  toSamples _ = singleSample $ Title "Season3GrandChampion"

data Skill = Skill
  { skillSkill :: Integer
  , skillTier :: Integer
  , skillUserId :: Maybe PlayerId
  , skillUserName :: Text
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Skill where
  parseJSON = genericParseJSON $ snakeFieldOptions "skill"

instance ToJSON Skill where
  toJSON = genericToJSON $ snakeFieldOptions "skill"

instance ToSample Skill where
  toSamples _ =
    [ ("PlayStation 4", Skill 1473 19 Nothing "LilPlayer-V3")
    , ("Steam", Skill 1639 19 (Just $ PlayerId "76561198067659334") "[MOCK] Kaydop")
    , ("Xbox One", Skill 1303 17 Nothing "MSTIO")
    ]

data Stat = Stat
  { statStatType :: StatType
  , statUserId :: Maybe PlayerId
  , statUserName :: Text
  , statValue :: Integer
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Stat where
  parseJSON = genericParseJSON $ snakeFieldOptions "stat"

instance ToJSON Stat where
  toJSON = genericToJSON $ snakeFieldOptions "stat"

instance ToSample Stat where
  toSamples _ =
    [ ("PlayStation 4", Stat StatTypeGoals Nothing "harmen501" 47104)
    , ("Steam", Stat StatTypeGoals (Just $ PlayerId "76561198139217900") "Ronito" 25124)
    , ("Xbox One", Stat StatTypeGoals Nothing "MadMassacre510" 43661)
    ]

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

instance ToJSON a => ToJSON (Single a) where
  toJSON (Single x) = toJSON [x]

instance ToSample a => ToSample (Single a) where
  toSamples _ = map (\(x, y) -> (x, Single y)) $ toSamples Proxy

data Stats = Stats
  { statsStatType :: StatType
  , statsStats :: [TypedStat]
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Stats where
  parseJSON = genericParseJSON $ snakeFieldOptions "stats"

instance ToJSON Stats where
  toJSON = genericToJSON $ snakeFieldOptions "stats"

instance ToSample Stats where
  toSamples _ = singleSample $ Stats StatTypeGoals $ filter (\x -> statStatType (typedStatValue x) == StatTypeGoals) $ map snd $ toSamples Proxy

newtype TypedStat = TypedStat
  { typedStatValue :: Stat
  } deriving (Eq, Ord, Show)

instance FromJSON TypedStat where
  parseJSON = withObject "TypedStat" $ \o -> do
    userId <- o .:? "user_id"
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

instance ToJSON TypedStat where
  toJSON x = object
    [ "user_id" .= statUserId (typedStatValue x)
    , "user_name" .= statUserName (typedStatValue x)
    , toUrlPiece (statStatType (typedStatValue x)) .= statValue (typedStatValue x)
    ]

instance ToSample TypedStat where
  toSamples _ = map (\(x, y) -> (x, TypedStat y)) $ toSamples Proxy

data Player = Player
  { playerUserId :: Maybe PlayerId
  , playerUserName :: Text
  , playerPlayerSkills :: [PlayerSkill]
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Player where
  parseJSON = genericParseJSON $ snakeFieldOptions "player"

instance ToJSON Player where
  toJSON = genericToJSON $ snakeFieldOptions "player"

instance ToSample Player where
  toSamples _ =
    [ ("PlayStation 4", Player Nothing "harmen501" $ map snd $ toSamples Proxy)
    , ("Steam", Player (Just $ PlayerId "76561198139217900") "Ronito" $ map snd $ toSamples Proxy)
    , ("Xbox One", Player Nothing "MadMassacre510" $ map snd $ toSamples Proxy)
    ]

data PlayerSkill = PlayerSkill
  { playerSkillDivision :: Integer
  , playerSkillMatchesPlayed :: Integer
  , playerSkillPlaylist :: Integer
  , playerSkillSkill :: Integer
  , playerSkillTier :: Integer
  , playerSkillTierMax :: Integer
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON PlayerSkill where
  parseJSON = genericParseJSON $ snakeFieldOptions "playerSkill"

instance ToJSON PlayerSkill where
  toJSON = genericToJSON $ snakeFieldOptions "playerSkill"

instance ToSample PlayerSkill where
  toSamples _ = singleSample $ PlayerSkill 0 664 11 1614 19 19

pascalConstructorOptions :: String -> Options
pascalConstructorOptions prefix = defaultOptions
  { constructorTagModifier = pascalModifier prefix
  }

snakeConstructorOptions :: String -> Options
snakeConstructorOptions prefix = defaultOptions
  { constructorTagModifier = snakeModifier prefix
  }

pascalFieldOptions :: String -> Options
pascalFieldOptions prefix = defaultOptions
  { fieldLabelModifier = pascalModifier prefix
  }

snakeFieldOptions :: String -> Options
snakeFieldOptions prefix = defaultOptions
  { fieldLabelModifier = snakeModifier prefix
  }

pascalModifier :: String -> String -> String
pascalModifier = dropPrefix

snakeModifier :: String -> String -> String
snakeModifier prefix = camelTo2 '_' . pascalModifier prefix

dropPrefix :: String -> String -> String
dropPrefix prefix string =
  if prefix `isPrefixOf` string
    then drop (length prefix) string
    else error $ "dropPrefix: " ++ show prefix ++ " is not a prefix of " ++ show string
