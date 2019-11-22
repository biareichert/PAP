import Text.ParserCombinators.Parsec

data Term = Atom String
          | Var Name
          | Predicate Predicate
          deriving (Show, Eq)


type Name = (Int, String)
type Predicate = (String, [Term])
type Rule = (Predicate, [Predicate])
type Substitution = [(Name, Term)]

atom :: Parser Term
atom = do
    name <- many1 lower
    return (Atom name)

variable :: Parser Term
variable = do
    name <- many1 upper
    return (Var (1, name))

parse_type :: Parser Term
parse_type =
  try atom <|> variable
  
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

--unifyTerm :: Term -> Term -> Maybe Substitution

--unifyPredicate :: Predicate -> Predicate -> Maybe Substitution

--unifyBody :: [Term] -> [Term] -> Maybe Substitution

unify :: Term -> Term -> Maybe Substitution

-- Regra (REFL)
unify (Var a) (Var b) | a == b =
    Just []

-- Regra (LEFT)
unify (Var a) t | not (occursCheck t a) =
    Just [(a, t)]

-- Regra (RIGHT)
-- unify (Term t) (Var a) | not (occursCheck a t) =
--     Just [(a, t)]

-- Regra (INT)
unify (Atom a) (Atom b) =
    Just []

-- Regra (ARROW)
-- unify (TypeArrow t1 r1) (TypeArrow t2 r2) = do
--     theta1 <- unify t1 t2
--     theta2 <- unify (subst theta1 r1) (subst theta1 r2)
--     --
--     Just (compose theta2 theta1)

-- Caso geral (não dá pra unificar)
unify a b =
    Nothing

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
