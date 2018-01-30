{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
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

-- This function would not be exported from the code base, and is only
-- defined here for convenience.
trust :: (a -> b) -> T H a -> T L b
trust f = T . f . unT


getUserInfo :: IO (T H User)
getUserInfo = (pure . pure) $ User "Matthias Pall" "Gissurarson" (0,0) 26

lookupRestaurants :: Loc -> [Restaurant]
lookupRestaurants (0,0) = [ Restaurant "The Nexus" (0,0) 3
                          , Restaurant "The Padded Duckling" (1,2) 1]
lookupRestaurants _ = []

-- Trusted code base, i.e. functions that we deem reveal little enough
-- information about the users location that it is OK to be shared.

-- We use unT and deem that the bestNearbyRestaurant is OK, i.e.
-- attackers are allowed to know the best restaurant around the user.
-- Of course, this is probably unsafe with triangulation, but we do not
-- worry about that for our example


bestNearbyRestaurant :: T H User -> T L (Maybe Restaurant)
bestNearbyRestaurant = trust (listToMaybe . sortOn rating
                              . lookupRestaurants . location)

isInNationalPark :: T H User -> T L Bool
isInNationalPark = trust (\t -> location t == (0,0))

isAllowedToDrink :: T H User -> T L Bool
isAllowedToDrink = trust (\t -> age t >= 20)

-- We allow reading of public information
readPublic :: T L a -> a
readPublic = unT


main :: IO ()
-- main = return ()
main = do user <- getUserInfo
          info <- pure (_ user)
          print (readPublic info)
