{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Game = Game
  { gameId :: Int
  , name :: String
  , image  :: String
  , thumbnail :: String
  , minPlayers :: Int
  , maxPlayers :: Int
  , playingTime :: Int
  , yearPublished :: Int
  , averageRating :: Double
  , rank :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Game)

type API = "collection" :> Capture "username" String :> Get '[JSON] [Game]
          :<|> "game" :> Capture "game" String :> Get '[JSON] [Game] 

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = (\_ -> return users) :<|> (\_ -> return users)

users :: [Game]
users = [ Game 1 "Agricola" "image" "thumb" 1 5 120 2006 3.456 10 ]
