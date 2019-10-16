main :: IO ()
main = do
	numeros <- lerNumeros
	print(converterParaArvore numeros)


lerNumeros :: IO [Int]
lerNumeros = do
	texto <- getLine
	let numero = read texto
	if numero == 0
		then do
			return []
	else do
		cauda <- lerNumeros
		return (numero : cauda)

data Tree a = Leaf a
	| Node (Tree a) (Tree a)
	deriving Show

converterParaArvore :: [Int] -> Tree Int
converterParaArvore (x:[]) = Leaf x
converterParaArvore (x:xs) = Node (Leaf x) (converterParaArvore xs)
