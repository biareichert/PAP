import Text.ParserCombinators.Parsec

data Term = Atom String
          | Var Name
          | Predicate Predicate
          deriving (Show, Eq)


type Name = (Int, String)
type Predicate = (String, [Term])
type Rule = (Predicate, [Predicate])
type Substitution = [(Name, Term)]

main :: IO()
main = do
    repl

repl :: IO()
repl = do
    putStr "?- "
    line <- getLine
    putStrLn ("Você digitou '" ++ (whitespace line) ++ "'")
    repl

whitespace :: [Char] -> [Char]
whitespace [] = []
whitespace (x:xs) =
    if x == ' ' then
        whitespace xs
    else
        x : whitespace xs

mapear :: (a -> b) -> [a] -> [b]
mapear t [] = []
mapear t (x:xs) = t x : mapear t xs

compose :: unify -> unify -> unify
compose xs ys =
    ++ xs (mapear Substitution xs) ys

--substTerm :: Substitution -> Term -> Term

--substPred :: Substitution -> Predicate -> Predicate

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
unify t (Var a) | not (occursCheck t a) =
    Just [(a, t)]

-- Regra (INT)
unify (Atom a) (Atom b) =
    Just []

-- Regra (ARROW)
unify (Predicate t1 r1) (Predicate t2 r2) = do
   theta1 <- unify t1 t2
   theta2 <- unify (substPred theta1 r1) (substPred theta1 r2)
   Just (compose theta2 theta1)

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

-- freshen :: Rule -> Rule


resolve :: Predicate -> [Rule] -> [Substitution]
-- resolve :: Predicate -> [Rule] -> Substitution
resolve goal [] = []
resolve goal ( (pred, body) : rules) =
    if (verify goal pred) then
        (unifyPred goal pred) ++ (resolve goal rules)
    else
        (resolve goal rules)

--resolveBody :: Substitution -> [Rule] -> [Predicate] -> [Substitution]


-- =========== Parser ===========

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

----------------------------------------------
db1 :: [Rule]
db1 = [ (("likes", [ (Atom "ana"), (Atom "banana") ]), []),
        (("likes", [ (Atom "ana"), (Atom "potato") ]), []),
        (("likes", [ (Atom "bob"), (Atom "potato") ]), []) ]

db2 :: [Rule]
db2 = [ (("likes", [ (Atom "ana"),    (Var (1, "Y")) ]), []),
        (("likes", [ (Atom "ana"), (Atom "potato") ]), []),
        (("dislikes", [ (Atom "bob"), (Atom "potato") ]), []) ]

my_goal :: Predicate
my_goal = ("likes", [ (Atom "ana") , (Var (1, "X")) ])

my_goal_potato :: Predicate
my_goal_potato = ("likes", [ (Var (1, "Z")), (Atom "potato") ])


verify :: Predicate -> Predicate -> Bool
verify (str_a, [] ) (str_b, []) = True

verify (goal_str, ( (Atom goal_t) : goal_ts)) (pred_str, ( (Atom pred_t) : pred_ts)) =
    if (goal_t == pred_t && goal_str == pred_str) then
        True && (verify (goal_str, (goal_ts)) (pred_str, (pred_ts)))
    else
        False

verify (goal_str, (goal_t : goal_ts)) (pred_str, (pred_t : pred_ts)) =
    True && (verify (goal_str, (goal_ts)) (pred_str, (pred_ts)))

unifyPred :: Predicate -> Predicate -> Maybe [Substitution]
unifyPred (str_a, [] ) (str_b, []) = []
unifyPred (goal_str, ( (Atom goal_t) : goal_ts)) (pred_str, ( (Atom pred_t) : pred_ts)) =
    if goal_t == pred_t then
        (unifyPred (goal_str, (goal_ts)) (pred_str, (pred_ts)))
    else
        []

unifyPred (goal_str, (goal_t : goal_ts)) (pred_str, (pred_t : pred_ts)) =
    (unify goal_t pred_t ) : (unifyPred (goal_str, (goal_ts)) (pred_str, (pred_ts)))
