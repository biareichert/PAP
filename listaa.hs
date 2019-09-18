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
--binParaInt n = 
--	auxiliar n 
--	where
--		auxiliar n =
--			if n == 1 then



--binParaInt
--converte' :: [Int] -> Int
--converte' [] = 0
--converte' (x:xs) = ((2 ^ comprimento xs) * x) + (2 ^ (comprimento xs-1)) * x

--ps: função comprimento mede o tamanho da cauda da lista atual

-- Questão 8

intParaBin :: Int -> String
intParaBin n =
	inverso (auxiliar n)
	where
		auxiliar 0 = "0"
		auxiliar 1 = "1"
		auxiliar n =
		if n `mod` 2 == 0 then
			'0' : auxiliar (n `div` 2)
		else
			'1' : auxiliar (n `div` 2)

-- Questão 9

menorValor :: Ord a => [a] -> a
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

-- Questão 10 //terminar, ele remove todos

removerPrimeiro :: Eq a => [a] -> a -> [a]
removerPrimeiro [] x = []
removerPrimeiro (y:ys) x = 
	if x == y then
		removerPrimeiro ys x
	else
		y : removerPrimeiro ys x
        
-- Questão 11 //terminar preciso usar recursividade na função ordenar


ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (y:ys) = 
	auxiliar y x
	where
		auxiliar [] x = x
		auxiliar (y:ys) = menorValor ys


-- Questão 12

dobrar_dir :: (a -> b -> b) -> b -> [a] -> b
dobrar_dir f acc [] = acc
dobrar_dir f acc (x:xs) =
    f x (dobrar_dir f acc xs)    


-- Questão 13

dobrar_esq :: (b -> a -> b) -> b -> [a] -> b
dobrar_esq f acc [] = acc
dobrar_esq f acc (x:xs) =
    dobrar_esq f (f acc x) xs



-- tamanho lista
tamanhoLista :: [a] -> Int
tamanhoLista [] = 0
tamanhoLista (x:xs) = 1 + tamanhoLista (xs)


{-
ordena [3,2,1] = 

        ultimos 2 [1,2,3] =
        1 : ultimos 1 [2,3] =
        1 : 2 : ultimos 0 [3] =
        1 : 2 : [] =
        [1, 2]
http://wiki.di.uminho.pt/twiki/pub/Education/Archive/ProgramacaoFuncional/PF65-80.pdf
-}
