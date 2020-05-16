import Data.Char (isUpper)

data Statement
  = Atom Char
  | Not Statement
  | And Statement Statement
  | Or Statement Statement
  | Implies Statement Statement
  deriving (Eq, Show)

data Derrivation
  = Derrivation [(Statement, Rule)]
  deriving (Show)

data Rule
  = Given -- raw statement that was previously proven
  | Join -- the two joined statements are implied in the derrived statement
  | Separate Statement
  | DoubleNegation Statement
  | Assumption Statement Derrivation
  | Detachment Statement -- the other statement is implied
  | Contrapositive Statement -- forward or backwards
  | Demorgan Statement
  | Switch Statement
  deriving (Show)

prettyPrint :: Statement -> String
prettyPrint stmt = case stmt of
  Atom    a   -> [a]
  Not     a   -> "~" ++ prettyPrint a
  And     a b -> "(" ++ prettyPrint a ++ " & " ++ prettyPrint b ++ ")"
  Or      a b -> "(" ++ prettyPrint a ++ " | " ++ prettyPrint b ++ ")"
  Implies a b -> "(" ++ prettyPrint a ++ " > " ++ prettyPrint b ++ ")"

type Result a = Either String a

parseStatement :: String -> Result Statement
parseStatement s = do
  (stmt, rest) <- parseNext s
  if rest /= ""
    then Left $ "left over after statement: " ++ rest
    else return stmt

parseDerrivation :: String -> Result Derrivation
parseDerrivation = undefined

parseNext :: String -> Result (Statement, String)
parseNext "" = Left "empty statement"
parseNext (c:cs) = case c of
  ' ' -> parseNext cs
  '(' -> do
    (left, rest1) <- parseNext cs
    (op, rest2) <- parseOp rest1
    (right, rest3) <- parseNext rest2
    rest4 <- requireNext ')' rest3
    return (op left right, rest4)
  '~' -> do
    (expr, rest) <- parseNext cs
    return (Not expr, rest)
  _ | isUpper c ->
      return (Atom c, cs)
  _ ->
    Left $ "Invalid expression starting at " ++ (c:cs)

parseOp :: String -> Result (Statement -> Statement -> Statement, String)
parseOp "" = Left "expected an operation, found EOF"
parseOp (c:cs) = case c of
  ' ' -> parseOp cs
  '&' -> return (And, cs)
  '|' -> return (Or, cs)
  '>' -> return (Implies, cs)
  _   -> Left $ "Expected an operation, found " ++ (c:cs)

requireNext :: Char -> String -> Result String
requireNext c "" = Left $ "required " ++ [c] ++ " but found EOF"
requireNext c (x:xs) =
  if c == x
  then return xs
  else Left $ "required " ++ [c] ++ " but found " ++ (x:xs)

main = putStrLn "Hello world from propositionalcalculus"
