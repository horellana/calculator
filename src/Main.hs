{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)
import qualified Data.Text.Lazy.Read as TR (double)
import qualified Data.Text.Lazy.IO as TIO (getLine, getContents)
import qualified Data.Text.Lazy as T (Text, words, append, lines)

data Function = Function Int ([Double] -> Double)

data Stack = Stack [Double] deriving (Show)

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
         ("e", Function 0 $ const $ exp 1),
         ("**", Function 2 pow),
         ("pi", Function 0 $ const pi),
         ("exp", Function 2 tenPow),
         ("log", Function 1 $ log . head),
         ("fib", Function 1 fib),
         ("fact", Function 1 fact),
         ("abs", Function 1 $ abs . head),
         ("sqrt", Function 1 $ sqrt . head),
         ("sin", Function 1 $ sin . head),
         ("cos", Function 1 $ cos . head),
         ("tan", Function 1 $ tan . head)]
  where
    pow [a,b] = product $ replicate (ceiling b) a
    tenPow [a,b] = a * pow [10, b]
    fact [n] = product [2..n]
    fib [n] | n == 0 = 0
            | n < 1 = 1
            | otherwise = fib [n - 1] + fib [n - 2]

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

eval :: Stack -> T.Text -> Either Error Stack
eval stack s = f (T.words s) stack
  where
    f [] stack' = Right stack'
    f (x:xs) stack' = case TR.double x of Right (n, _) -> f xs (push stack' n)
                                          Left _ -> call x stack' table >>= f xs

interactive :: IO ()
interactive = loop $ Stack []
  where
    loop stack = do input <- TIO.getLine
                    case eval stack input of
                      Right stack'@(Stack elements) -> do forM_ elements print
                                                          putStrLn "---"
                                                          loop stack'
                      Left err -> print err >> loop stack

batch :: IO ()
batch = TIO.getContents >>= loop (Stack []) . T.lines
  where
    loop (Stack numbers) [] = forM_ numbers print
    loop stack (x:xs) = either print (`loop` xs) $ eval stack x

main :: IO ()
main = do args <- getArgs
          if not $ null args
            then interactive
            else batch
