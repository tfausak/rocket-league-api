{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
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
    platform = "steam"
    playerId = "76561198139217900"
  void . flip runClientM clientEnv $ do
    liftIO . print =<< getPlayerSkills platform playerId token
    liftIO . print =<< postPlayerSkills platform (object ["player_ids" .= [playerId]]) token
    liftIO . print =<< getPlayerTitles platform playerId token
    liftIO . print =<< getPopulation token
    liftIO . print =<< getRegions token
    liftIO . print =<< getSkillsLeaderboard platform "10" token
    liftIO . print =<< getStatsLeaderboard platform token
    liftIO . print =<< getStatLeaderboard platform "goals" token
    liftIO . print =<< getPlayerStat platform "goals" playerId token
    liftIO . print =<< postPlayerStat platform "goals" (object ["player_ids" .= [playerId]]) token
