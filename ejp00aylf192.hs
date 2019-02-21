{-- 
Recibe un entero n y regresa una lista con los primeros n + 1 elementos de la
sucesión de Fibonacci.
--}
sFibonacci :: Int -> [Int]
sFibonacci n
    | n < 0 = error "rango no válido"
    | otherwise = take (n + 1) (sFibonacciAux 1 1)

sFibonacciAux :: Int -> Int -> [Int]
sFibonacciAux a b 
    | a <= 0 || b <= 0 = error "rango no válido"
    | otherwise = a : (sFibonacciAux b (a + b))

{--
Recibe una lista de elementos comparables y un elemento de la lista y regresa
una nueva lista sin ninguna aparición de este elemento.
--}
quitaElemento :: (Eq a) => [a] -> a -> [a]
quitaElemento [] _ = []
quitaElemento (x:xs) a = 
    if x == a
        then (quitaElemento xs a)
        else x:(quitaElemento xs a)

{--
Recibe un entero y regresa una lista con los divisores propios de éste.
--}
divisoresPropios :: Int -> [Int]
divisoresPropios n = [ k | k <- [1..(n-1)], (mod n k) == 0]

{--
Reciba un entero positivo y diga si es perfecto o no.
--}
esPerfecto :: Int -> Bool
esPerfecto n = n == sum (divisoresPropios n)

{--
Recibe dos enteros positivos y determina si son amigos.
--}
sonAmigos :: Int -> Int -> Bool
sonAmigos a b = sum (divisoresPropios a) == b && sum(divisoresPropios b) == a

{--
Recibe un entero y regresa la suma de sus dígitos hasta que quede un número de
un solo dígito.
--}
supersuma :: Int -> Int
supersuma a
    | (div a 10) == 0 = a
    | otherwise = supersuma (sum (listDig a))

listDig :: Int -> [Int]
listDig a 
    | (div a 10) == 0 = [a]
    | otherwise = (mod a 10) : (listDig (div a 10 ))

{--
Obtienen la reversa de una lista usando las funciones de orden superior
foldr y foldl respectivamente.
--}

reversar :: [a] -> [a]
reversar l = foldr (\ x xs -> xs ++ [x]) [] l

reversal :: [a] -> [a]
reversal l = foldl (\ xs x -> x:xs) [] l