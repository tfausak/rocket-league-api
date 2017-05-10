{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Network.HTTP.Client.TLS
import RocketLeagueApi
import Servant.Client
import Servant.Docs
import System.Environment

import qualified Data.Set as Set
import qualified Data.Text as Text

main :: IO ()
main = do
  [rawToken] <- getArgs
  manager <- newTlsManager
  let
    token = Just . Token $ Text.pack rawToken
    clientEnv = ClientEnv manager baseUrl
    run f = do
      x <- runClientM (f token) clientEnv
      case x of
        Left l -> fail $ show l
        Right r -> print r

  putStrLn . markdown $ docs api

  run $ getPopulation
  run $ getRegions

  forM_ platforms $ \platform -> do
    print platform
    run $ getStatsLeaderboard platform

    forM_ statTypes $ \statType -> do
      print (platform, statType)
      run $ getStatLeaderboard platform statType

    forM_ playlists $ \playlist -> do
      print (platform, playlist)
      run $ getSkillsLeaderboard platform playlist

  forM_ players $ \(platform, rawPlayerIds) -> do
    let playerIds = PlayerIds (Set.fromList rawPlayerIds)
    print (platform, playerIds)
    run $ postPlayerSkills platform playerIds

    forM_ statTypes $ \statType -> do
      print (platform, playerIds, statType)
      run $ postPlayerStat platform statType playerIds

    forM_ rawPlayerIds $ \playerId -> do
      print (platform, playerId)
      run $ getPlayerSkills platform playerId
      run $ getPlayerTitles platform playerId

      forM_ statTypes $ \statType -> do
        print (platform, playerId, statType)
        run $ getPlayerStat platform statType playerId
 where
  baseUrl = BaseUrl Https "api.rocketleaguegame.com" 443 "/api/v1"
  platforms = [minBound .. maxBound]
  playlists = [minBound .. maxBound]
  statTypes = [minBound .. maxBound]
  players =
    [ (PlatformPS4, [PlayerId "shamott21", PlayerId "Iris_-lily-_"])
    , (PlatformSteam, [PlayerId "76561198261574626", PlayerId "76561198149461762"])
    , (PlatformXboxOne, [PlayerId "MSTIO", PlayerId "Best Mr Pedro"])
    ]
