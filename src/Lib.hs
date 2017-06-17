{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    (
    routes
    ) where

import GHC.Generics
import Data.Monoid (mconcat)
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy.Char8 (ByteString)
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

routes :: ScottyM ()
routes = do

  middleware logStdoutDev

  get "/hello" hello
  get "/users" users
  get "/users/:id" findUserById
  get "/public/:name" getFile
  get "/gomoku" getGomoku
  post "/api" postGomoku
  get "/:word" meUp


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

postGomoku :: ActionM ()
postGomoku = do
  game <- jsonData
  json $ calculate game
