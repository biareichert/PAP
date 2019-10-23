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

-- #####################################################################

repl :: IO ()
repl = do
    putStr "?- "
    line <- getLine
    --testar se a linha é um predicado válido em Prolog
    putStrLn("Voce digitou: "++line)
    repl

multiply :: [Int] -> [Int]
multiply lista = do
    x <- lista
    y <- [2,10]
    return (x*y)

main :: IO()
main = do
    print $ multiply [1,3,7]
    
    
-- ################### Trabalho #######################

x = Var(0,"X")
y = Var(0,"Y")
z = Var(0,"Z")

likes = Predicate ("likes", [Atom "bob", Atom "apple"])

main :: IO()
main = do
	print $ unifyTerm x likes
	print $ unifyTerm likes x  

unifyTerm :: Term -> Term -> Maybe Substitution

-- Caso (REFL)
unifyTerm (Var X) (Var Y) |  x == y =
	Just []

-- Caso (ATOM) implementar

-- Caso (LEFT)
unifyTerm (Var x) e | not (occursChek e x) =
	Just [(x,e)]  

-- Caso (RIGHT)
unifyTerm e (Var x) | not (occursCheck e x) = 
	Just [(x,e)]

-- Caso (PRED) implementar

-- Caso geral: não é possível unificar
unifyTerm x y =
	Nothing

occursCheck :: Term -> Name -> Bool
occursCheck e x = False -- Implementar

data Term = Atom String 
	| Var Name
	| Predicate Predicate
	deriving (Show, Eq)

type Name = (Int, String)

type Predicate = (String, [Term])

type Rule = (Predicate, [Predicate])

type Substitution = [(Name, Term)]
