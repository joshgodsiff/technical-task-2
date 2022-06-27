module Main where

import Backend.Servant (app)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)

main :: IO ()
main = run 3421 $ simpleCors app
