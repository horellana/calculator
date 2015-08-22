module Main where

import Text.Read
import Data.Either

data Function = Function { name :: String,
                           argc :: Int,
                           code :: [Int] -> Int }

instance Show Function where
  show (Function name argc _) = "<Function " ++ name ++ " : " ++ show argc ++ ">"

data Stack = Stack [Int] deriving (Show)

push :: Stack -> Int -> Stack
push (Stack s) n = Stack (n : s)

pop :: Stack -> (Stack, Int)
pop (Stack (x:xs)) = (Stack xs, x)

type Table = [(String, Function)]

table :: Table
table = [("+", Function { name = "+",
                          argc = 2,
                          code = sum }),
         ("-", Function { name = "-",
                          argc = 2,
                          code = foldr (-) 0}),
         ("*", Function { name = "*",
                          argc = 2,
                          code = product }),
         ("/", Function { name = "/",
                          argc = 2,
                          code = foldr div 1 })]

eval :: String -> Stack -> Table -> Either String Stack
eval f s t = case lookup f t of
  Just function -> Right $ apply function s
  Nothing -> Left $ "Undefined function: " ++ f

main :: IO ()
main = undefined
