module Main where


-- Importo la funcion readMaybe desde el modulo Text.Read
import Text.Read (readMaybe)

-- creo un nuevo tipo de dato (como un struct en c), 'Function'.
-- tiene 2 atributos, argc (numero de argumentos que recibe la funcion)
-- y code, que es de tipo ([int] -> int), esto significa que la funcion
-- 'code' recibe una lista de enteros y retorna un entero
data Function = Function { argc :: Int,
                           code :: [Int] -> Int }
                           
-- otro tipo de dato.
-- Esta es otra manera de declararlos, en este caso
-- creo un Stack (pila), el que consistira em una lista de enteros
data Stack = Stack [Int] deriving (Show)

-- Aqui digo que Table es un sinonimo al tipo [(String, Function)]
-- Este tipo representa una lista de tuplas
type Table = [(String, Function)]

-- Lo mismo, Error es un sinonomo al tipo String
type Error = String

-- Defino la funcion push
-- esta recibe 1 stack, 1 entero y retorna 1 Stack
push :: Stack -> Int -> Stack
-- Aqui hago pattern matching,
-- s corresponde a la lista de enteros del stack
-- n corresponde al entero.
-- Creo una nueva lista, concatenando n y la lista s
-- y la ocupo para generar un stack nuevo
push (Stack s) n = Stack (n : s)


-- El tipo de dato Either tiene dos atributos
-- 'Right a' y 'Left a'. Este tipo de dato se ocupa
-- para reflejar que una funcion pudo ejecutarse
-- (en este caso retornaria Right a, 'a' representa el resultado),
-- o si ocurrio un error (Left error)
pop :: Stack -> Either Error (Stack, Int)
-- Si pop recibe un stack vacio entonces es un error
pop (Stack []) = Left "Stack underflow"
-- Si no hacemos pattern matching para sacar el primer elemento de la lista 'x'
-- retornando una tupla con el stack con el resto de la lista 'xs' y el elemento 'x' 
pop (Stack (x:xs)) = Right (Stack xs, x)

table :: Table
table = [("+", Function { argc = 2,
                          code = sum }),
         ("-", Function { argc = 2,
                          code = foldr (-) 0}),
         ("*", Function { argc = 2,
                          code = product }),
         ("/", Function { argc = 2,
                          code = foldr div 1 }),
         ("%", Function { argc = 2,
                          code = foldr mod 1}),
         ("**", Function { argc = 2,
                          code = pow })]

pow :: [Int] -> Int
pow [a,b] = product $ replicate b a
                    
apply :: Function -> Stack -> Either Error Stack
apply (Function argc code) stack = f code [] 0 stack
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
            Right (Stack s) -> mapM_ print s
            Left err -> putStrLn err
  where
    loop [] stack = Right stack
    loop (x:xs) stack = case readMaybe x of
      Just n -> loop xs (push stack n)
      Nothing -> call x stack table >>= loop xs
