{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Network.HTTP.Client.TLS
import RocketLeagueApi
import Servant.Client
import System.Environment

main :: IO ()
main = do
  [rawToken] <- getArgs
  manager <- newTlsManager
  let
    token = Just . Token $ pack rawToken
    baseUrl = BaseUrl Https "api.rocketleaguegame.com" 443 "/api/v1"
    clientEnv = ClientEnv manager baseUrl
    platform = PlatformSteam
    playerId = PlayerId "76561198139217900"
    playerIds = PlayerIds [playerId, PlayerId "76561198067659334"]
    playlist = PlaylistCompetitiveSoloDuel
    statType = StatTypeGoals
    run f = do
      x <- runClientM (f token) clientEnv
      case x of
        Left l -> fail $ show l
        Right r -> print r
  run $ getPlayerSkills platform playerId
  run $ postPlayerSkills platform playerIds
  run $ getPlayerTitles platform playerId
  run $ getPopulation
  run $ getRegions
  run $ getSkillsLeaderboard platform playlist
  run $ getStatsLeaderboard platform
  run $ getStatLeaderboard platform statType
  run $ getPlayerStat platform statType playerId
  run $ postPlayerStat platform statType playerIds
