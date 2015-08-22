module Main where

import Text.Read
import Data.Either

data Function = Function { name :: String,
                           args :: Int,
                           code :: [Int] -> Int }

instance Show Function where
  show (Function name args _) = "<Function " ++ name ++ " : " ++ show args ++ ">"

data Stack = Stack [Int] deriving (Show)

apply :: Function -> Stack -> Stack
apply (Function _ args code) (Stack stack) = Stack $ (code $ take args stack) : (drop args stack)

type Table = [(String, Function)]

table :: Table
table = [("+", Function { name = "+",
                          args = 2,
                          code = sum }),
         ("-", Function { name = "-",
                          args = 2,
                          code = foldr (-) 0}),
         ("*", Function { name = "*",
                          args = 2,
                          code = product }),
         ("/", Function { name = "/",
                          args = 2,
                          code = foldr div 1 })]

eval :: String -> Stack -> Table -> Either String Stack
eval f s t = case lookup f t of
  Just function -> Right $ apply function s
  Nothing -> Left $ "Undefined function: " ++ f

main :: IO ()
main = undefined
