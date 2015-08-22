module Main where

import Text.Read

data Function = Function { name :: String,
                           args :: Int,
                           code :: [Int] -> Int }

instance Show Function where
  show (Function name args _) = "<Function " ++ name ++ " : " ++ show args ++ ">"

data Stack = Stack [Int] deriving (Show)

apply :: Function -> Stack -> Stack
apply (Function _ args code) (Stack stack) = Stack $ (code $ take args stack) : (drop args stack)

table :: [(String, Function)]
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

eval :: String -> Stack -> Stack
eval f stack = case lookup f table of
  Just function -> apply function stack
  Nothing -> error $ "Undefined function: " ++ f

main :: IO ()
main = undefined
