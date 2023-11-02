module Main (main) where

import LambdaTypes

main :: IO ()
main = print $ eval $ (lTrue `App` (Var 10)) `App` (Var 20)
