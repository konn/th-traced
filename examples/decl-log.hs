{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Traced

main :: IO ()
main = putStrLn $(do fname <- newName "str"
                     ((), s) <- tracing $ do
                       si <- sigD fname [t|String|]
                       de <- funD fname [clause [] (normalB [|"hoge"|]) []]
                       addTopDecls [si, de]

                     [|$(litE $ stringL $ unlines (map pprint $ topDecls s)) ++ $(varE fname)|]
                 )
