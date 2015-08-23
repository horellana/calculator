{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Read (double)
import qualified Data.Text as T (Text, splitOn, append)
import qualified Data.Text.IO as TIO (getLine)

data Function = Function { fArgc :: Int,
                           fCode :: [Double] -> Double }
                           
data Stack = Stack [Double]

type Table = [(T.Text, Function)]

type Error = T.Text

push :: Stack -> Double -> Stack
push (Stack s) n = Stack (n : s)

pop :: Stack -> Either Error (Stack, Double)
pop (Stack []) = Left "Stack underflow"
pop (Stack (x:xs)) = Right (Stack xs, x)

table :: Table
table = [("+", Function 2 sum),
         ("-", Function 2 $ foldr (-) 0),
         ("*", Function 2 product),
         ("/", Function 2 $ foldr (/) 1),
         ("**", Function 2 pow),
         ("sqrt", Function 1 $ sqrt . head)]
  where
    pow [a,b] = product $ replicate (ceiling a) b
                    
apply :: Function -> Stack -> Either Error Stack
apply (Function argc code) = f code [] 0
  where
    f fn args cont stack'
      | cont == argc = Right $ push stack' (fn args) 
      | otherwise = do (stack'', a) <- pop stack'
                       f fn (a : args) (cont + 1) stack''
                  
call :: T.Text -> Stack -> Table -> Either Error Stack
call f s t = case lookup f t of
  Just function -> apply function s
  Nothing -> Left $ T.append "Undefined function: "  f

main :: IO ()
main = loop $ Stack []
  where
    loop stack = do input <- TIO.getLine
                    case evalLine (words' input) stack of
                      Right (Stack s) -> do putStrLn "-----"
                                            mapM_ print s
                                            loop $ Stack s
                      Left err -> print err
    words' = T.splitOn " "
    evalLine [] stack = Right stack
    evalLine (x:xs) stack = case double x of
      Right (n, _) -> evalLine xs (push stack n)
      Left _ -> call x stack table >>= evalLine xs
