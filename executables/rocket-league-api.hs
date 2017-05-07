{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
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
    playerId = PlayerIdSteam 76561198139217900
    playerIds = PlayerIds [playerId]
    playlist = PlaylistCompetitiveSoloDuel
    statType = StatTypeGoals
  void . flip runClientM clientEnv $ do
    liftIO . print =<< getPlayerSkills platform playerId token
    liftIO . print =<< postPlayerSkills platform playerIds token
    liftIO . print =<< getPlayerTitles platform playerId token
    liftIO . print =<< getPopulation token
    liftIO . print =<< getRegions token
    liftIO . print =<< getSkillsLeaderboard platform playlist token
    liftIO . print =<< getStatsLeaderboard platform token
    liftIO . print =<< getStatLeaderboard platform statType token
    liftIO . print =<< getPlayerStat platform statType playerId token
    liftIO . print =<< postPlayerStat platform statType playerIds token
