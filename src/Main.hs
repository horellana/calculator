module Main where

data Function = Function { name :: String,
                           args :: Int,
                           code :: [Int] -> Int }

instance Show Function where
  show (Function name args code) = "<Function " ++ name ++ ">"

data Stack = Stack [Int] deriving (Show)

apply :: Function -> Stack -> Stack
apply (Function _ args code) (Stack stack) = Stack $ (code $ take args stack) : (drop args stack)
                                                                                                
main :: IO ()
main = do
  putStrLn "hello world"
