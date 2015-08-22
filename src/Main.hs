module Main where

import Text.Read (readMaybe)

data Function = Function { argc :: Int,
                           code :: [Double] -> Double }
                           
data Stack = Stack [Double] deriving (Show)

type Table = [(String, Function)]

type Error = String

push :: Stack -> Double -> Stack
push (Stack s) n = Stack (n : s)

pop :: Stack -> Either Error (Stack, Double)
pop (Stack []) = Left "Stack underflow"
pop (Stack (x:xs)) = Right (Stack xs, x)

table :: Table
table = [("+", Function { argc = 2,
                          code = sum }),
         ("-", Function { argc = 2,
                          code = foldr (-) 0}),
         ("*", Function { argc = 2,
                          code = product }),
         ("/", Function { argc = 2,
                          code = foldr (/) 1 }),
         ("**", Function { argc = 2,
                           code = pow })]

pow :: [Double] -> Double
pow [a,b] = product $ replicate (ceiling a) b
                    
apply :: Function -> Stack -> Either Error Stack
apply (Function argc code) stack = f code [] 0 stack
  where
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
            Right (Stack s) -> mapM_ print s
            Left err -> putStrLn err
  where
    loop [] stack = Right stack
    loop (x:xs) stack = case readMaybe x of
      Just n -> loop xs (push stack n)
      Nothing -> call x stack table >>= loop xs
