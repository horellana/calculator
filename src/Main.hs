module Main where

import Text.Read

data Function = Function { name :: String,
                           argc :: Int,
                           code :: [Int] -> Int }

instance Show Function where
  show (Function name argc _) = "<Function " ++ name ++ " : " ++ show argc ++ ">"

data Stack = Stack [Int] deriving (Show)

type Table = [(String, Function)]
type Error = String

push :: Stack -> Int -> Stack
push (Stack s) n = Stack (n : s)

pop :: Stack -> Either Error (Stack, Int)
pop (Stack []) = Left "stack underflow"
pop (Stack (x:xs)) = Right (Stack xs, x)

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
                          code = foldr div 1 }),
         ("%", Function { name = "%",
                          argc = 2,
                          code = foldr mod 1})]

apply :: Function -> Stack -> Either Error Stack
apply (Function _ argc code) stack = f code [] 0 stack
  where
    f :: ([Int] -> Int) -> [Int] -> Int -> Stack -> Either Error Stack
    f fn args cont stack'
      | cont == argc = Right $ push stack' (fn args) 
      | otherwise = do (stack'', a) <- pop stack'
                       f fn (a : args) (cont + 1) stack''

                  
call :: String -> Stack -> Table -> Either Error Stack
call f s t = case lookup f t of
  Just function -> apply function s
  Nothing -> Left $ "Undefined function: " ++ f

main :: IO ()
main = do input <- getContents
          case loop (words input) (Stack []) of
            Right (Stack s) -> print s
            Left err -> print err
  where
    loop [] s = Right s
    loop (x:xs) stack = case readMaybe x :: Maybe Int of
      -- x == n, is a number, so add it to the stack
      -- and go to the next element
      Just n -> loop xs (push stack n)
      -- x is a string, so see if its a known function
      -- call it, and go on 
      Nothing -> do (Stack t) <- call x stack table
                    loop xs (Stack t)
        

  
         

  
