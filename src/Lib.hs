{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    (
    routes
    ) where

import GHC.Generics
import Data.Monoid (mconcat)
import Data.Maybe
import Data.Text.Read
import Web.Scotty
import Web.Scotty.Cookie
import Network.Wai.Middleware.RequestLogger
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Text.Lazy as TL
import Gomoku

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User
instance ToJSON Game
instance FromJSON Game

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]

routes :: Integer -> ScottyM ()
routes seed = do

  middleware logStdoutDev

  get "/" hits
  get "/hello" hello
  get "/users" users
  get "/users/:id" findUserById
  get "/public/:name" getFile
  get "/gomoku" getGomoku
  post "/api" (postGomoku seed)
  get "/:word" meUp


hits ::  ActionM ()
hits = do
  hits <- fmap (fromMaybe "0") $ getCookie "hits"
  let hits' = case decimal hits of
                  Right n -> TL.pack . show . (+1) $ (fst n :: Integer)
                  Left _  -> "1"
  setSimpleCookie "hits" $ TL.toStrict hits'
  html $ mconcat [ "<html><body>"
                 , hits'
                 , "</body></html>"
                 ]

hello :: ActionM ()
hello = text "Hello!"

meUp :: ActionM ()
meUp = do
  beam <- param "word"
  html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

users :: ActionM ()
users = json allUsers

findUserById :: ActionM ()
findUserById = do
  id <- param "id"
  json $ filter (matchesId id) allUsers

getFile :: ActionM ()
getFile = do
  name <- param "name"
  file $ "public/" ++ name

getGomoku :: ActionM ()
getGomoku = do
  file "public/index.html"

postGomoku :: Integer -> ActionM ()
postGomoku seed = do
  coocie <- fmap (fromMaybe "0") $ getCookie "seed"
  let seed' = case decimal coocie of
                  Right n -> 1 + fst n :: Integer
                  Left _  -> seed
  let coocie' = TL.pack $ show seed'
  setSimpleCookie "seed" $ TL.toStrict coocie'
  game <- jsonData
  json $ calculate seed' game
