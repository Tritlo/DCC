{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
module RestaurantInfo ( Lattice(..), T -- We don't export the constructors for T
                                       -- since then users could use unT at will.
                      , User(..) , Loc , Restaurant
                      , getUserInfo, readPublic
                      , bestNearbyRestaurant, isInGothenburg, isAllowedToDrink
                      ) where

import DCC
import Lattice
import Data.List ( sortOn )
import Data.Maybe ( listToMaybe )


type Loc = (Int, Int)
data Restaurant = Restaurant { name :: String
                             , coords :: Loc
                             , rating :: Int } deriving (Show)

data User = User { firstName :: String
                 , lastName :: String
                 , location :: Loc
                 , age :: Int } deriving (Show)



lookupRestaurants :: Loc -> [Restaurant]
lookupRestaurants (0,0) = [ Restaurant "Bhoga" (0,0) 3
                          , Restaurant "Koka" (1,2) 1]
lookupRestaurants _ = []

trust :: (a -> b) -> T H a -> T L b
trust f = T . f . unT

-- Exported functions:

bestNearbyRestaurant :: T H User -> T L (Maybe Restaurant)
bestNearbyRestaurant = trust (listToMaybe . reverse . sortOn rating
                              . lookupRestaurants . location)

isInGothenburg :: T H User -> T L Bool
isInGothenburg = trust (inGothenburg . location)
  where inGothenburg (x,y) = (x >= -5) && (x <= 5) && (y >= -5) && (y <= 5)

isAllowedToDrink :: T H User -> T L Bool
isAllowedToDrink = trust ((>= 20) . age)

-- We allow reading of public information
readPublic :: T L a -> a
readPublic = unT

-- Info on the user can be read, but only in a private
-- environment.
getUserInfo :: IO (T H User)
getUserInfo = (pure . pure) $ User "Matthias Pall" "Gissurarson" (0,0) 26
