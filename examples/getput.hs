{-# LANGUAGE TemplateHaskell #-}
module Main where
import Data.Maybe                 (fromJust)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Traced

do tracing_ $ putQ "bar"
   return []

main :: IO ()
main = putStrLn $(tracing_ $ litE . stringL . (show :: Maybe String -> String) =<< getQ)
