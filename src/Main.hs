module Main where

import Text.Read

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

apply :: Function -> Stack -> Stack
apply (Function _ argc code) stack = f code [] 0 stack
  where
    f :: ([Int] -> Int) -> [Int] -> Int -> Stack -> Stack
    f fn args cont stack'
      | cont == argc = push stack' (fn args) 
      | otherwise = let (stack'', a) = pop stack'
                    in f fn (a : args) (cont + 1) stack''

call :: String -> Stack -> Table -> Either String Stack
call f s t = case lookup f t of
  Just function -> Right $ apply function s
  Nothing -> Left $ "Undefined function: " ++ f

loop :: [String] -> Stack -> Table -> Stack
loop [] s _ = s
loop (x:xs) stack table = case readMaybe x :: Maybe Int of
  -- x == n, is a number, so add it to the stack
  -- and go to the next element
  Just n -> loop xs (push stack n) table
  -- x is a string, so see if its a known function
  -- call it, and go on 
  Nothing -> case call x stack table of
    Right (Stack t) -> loop xs (Stack t) table
    Left e -> error e

main :: IO ()
main = getContents
       >>= (\a -> return $ loop (words a) (Stack []) table)
       >>= print

  
