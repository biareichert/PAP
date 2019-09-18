-- Questão 1

concatenacao :: Eq a => [a] -> [a] -> [a]
concatenacao [] a =
    a
concatenacao (x:xs) a =
    x : concatenacao xs a


-- concatenacao (1:2:[]) a =
-- 1 : concatenacao (2:[]) a =
-- 1 : 2 : concatenacao [] a =
-- 1 : 2 : a


-- Questão 2

pertence :: Eq a => a -> [a] -> Bool
pertence a [] = False
pertence a (x:xs) =
  if a == x then
    True
  else
    pertence a xs

-- Questão 3

intersecao :: Eq a => [a] -> [a] -> [a]
intersecao [] y = []
intersecao x [] = []
intersecao xs (y:ys) =
  if pertence y xs then
    y : intersecao xs ys
  else
   intersecao xs ys

-- Questão 4

inverso :: [a] -> [a]
inverso [] = []
inverso l = inversoAc [] l
    where
	inversoAc ac [] = ac
        inversoAc ac (x:xs) = inversoAc (x:ac) xs

-- Questão 5

primeiros :: Int -> [a] -> [a]
primeiros x [] = error "Lista vazia"
primeiros 0 ys = []
primeiros x (y:ys) =
    y : primeiros (x-1) ys

-- Questão 6

ultimos :: Int -> [a] -> [a]
ultimos x [] = error "Lista vazia"
ultimos 0 ys = []
ultimos x ys = inverso (primeiros (x) (inverso ys))

-- Questão 7

--binParaInt :: String -> Int
--binParaInt
--converte' :: [Int] -> Int
--converte' [] = 0
--converte' (x:xs) = ((2 ^ comprimento xs) * x) + (2 ^ (comprimento xs-1)) * x

--ps: função comprimento mede o tamanho da cauda da lista atual

-- Questão 8

intParaBin :: Int -> String
intParaBin 0 = 0
intParaBin 1 = 1
intParaBin n = n : intParaBin(n `div` 2) ++ show(n `mod` 2)

-- Questão 9

menorValor :: [Int] -> Int
menorValor [] =
    error "Não rola"
menorValor (x:xs) =
    worker xs x
    where
        worker [] y =
            y
        worker (x:xs) y =
            let menor = if x < y then
                            x
                        else
                           y in
            worker xs menor


-- tamanho lista
tamanhoLista :: [a] -> Int
tamanhoLista [] = 0
tamanhoLista (x:xs) = 1 + tamanhoLista (xs)


{-
        ultimos 2 [1,2,3] =
        1 : ultimos 1 [2,3] =
        1 : 2 : ultimos 0 [3] =
        1 : 2 : [] =
        [1, 2]
http://wiki.di.uminho.pt/twiki/pub/Education/Archive/ProgramacaoFuncional/PF65-80.pdf
-}

--foldr
dobrar_dir :: (a -> b -> b) -> b -> [a] -> b
dobrar_dir f acc [] = acc
dobrar_dir f acc (x:xs) =
    f x (dobrar_dir f acc xs)    
-- explicação: caso da lista já estiver vazia retornar somente o acumulador
-- retorna recursivamente a operação (f), acumulador(acc) e a função dobrar_dir com a cauda
-- imaginar a árvore de ordem de  operação desenhada no quadro
-- OU fazer o foldr pronto do Haskell: dobrar_dir = foldr
-- obs: dobrar_dir (+) 0 [1,2,3] = 1 + (2 + (3 + 0))

-- foldl
dobrar_esq :: (b -> a -> b) -> b -> [a] -> b
dobrar_esq f acc [] = acc
dobrar_esq f acc (x:xs) =
    dobrar_esq f (f acc x) xs
-- dobrar_esq (+) 0 [1,2,3] = ((0 + 1) + 2) + 3
