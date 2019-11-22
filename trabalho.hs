import Text.ParserCombinators.Parsec

data Term = Atom String
          | Var Name
          | Predicate Predicate
          deriving (Show, Eq)


type Name = (Int, String)
type Predicate = (String, [Term])
type Rule = (Predicate, [Predicate])
type Substitution = [(Name, Term)]

db :: [Rule]
db = [  (Predicate ("likes", ["ana", "banana"])) ]

main :: IO()
main = do
    repl

repl :: IO()
repl = do
    putStr "?- "
    line <- getLine
    putStrLn ("Você digitou '" ++ (whitespace line) ++ "'")
    repl

run :: FilePath -> IO()

whitespace :: [Char] -> [Char]
whitespace [] = []
whitespace (x:xs) = 
    if x == ' ' then
        whitespace xs
    else 
        x : whitespace xs

compose :: Unifier -> Unifier -> Unifier
compose xs ys =
    ++ xs (mapear subst_in_unifier xs) ys

substTerm :: Substitution -> Term -> Term

substPred :: Substitution -> Predicate -> Predicate

unifyTerm :: Term -> Term -> Maybe Substitution

unifyPredicate :: Predicate -> Predicate -> Maybe Substitution

unifyBody :: [Term] -> [Term] -> Maybe Substitution

occursCheck :: Term -> Name -> Bool
occursCheck (Atom a) n = 
    False

occursCheck (Var v) n = 
    v == n

occursCheck (Predicate (s, [])) n = 
    False

occursCheck (Predicate (s, (t:ts))) n = 
    if (occursCheck t n) then
        True
    else
        occursCheck (Predicate (s, ts)) n

freshen :: Rule -> Rule

resolve :: Predicate -> [Rule] -> [Substitution]

resolveBody :: Substitution -> [Rule] -> [Predicate] -> [Substitution]
