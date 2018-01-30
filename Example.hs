import RestaurantInfo

main :: IO ()
main = do user <- getUserInfo
          info <- pure (_ user)
          print (readPublic info)
