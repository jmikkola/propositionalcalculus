import Control.Monad (foldM)
import Data.Char (isUpper)
import System.Environment (getArgs)


data Statement
  = Atom Char
  | Not Statement
  | And Statement Statement
  | Or Statement Statement
  | Implies Statement Statement
  deriving (Eq, Show)

type Step = (Statement, Rule)

data Derrivation = Derrivation [Step]
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


checkDerrivation :: Derrivation -> Result ()
checkDerrivation (Derrivation steps) = do
  _ <- foldM checkStep [] steps
  return ()

type Proven = [Statement]

checkStep :: Proven -> Step -> Result Proven
checkStep proven (newStatement, rule) = case rule of
  Given -> do
    requireProven proven newStatement
    return proven

  Join -> case newStatement of
    (And a b) -> do
      requireProven proven a
      requireProven proven b
      return (newStatement : proven)
    _ ->
      Left $ "join requires statement to be a (x & y), got " ++ prettyPrint newStatement

  Separate stmt -> do
    requireProven proven stmt
    case stmt of
      (And a _) | a == newStatement -> return (a : proven)
      (And _ b) | b == newStatement -> return (b : proven)
      (And _ _) -> Left $ prettyPrint newStatement ++ " is not part of " ++ prettyPrint stmt
      _         -> Left $ "cannot separate " ++ prettyPrint stmt

  DoubleNegation stmt -> do
    requireProven proven stmt
    if impliedByDoubleNegation newStatement stmt
      then return (newStatement : proven)
      else Left $ "cannot derrive " ++ prettyPrint newStatement ++ " from " ++
           prettyPrint stmt ++ " by double negation"

  Assumption stmt (Derrivation steps) -> do
    innerProven <- foldM checkStep (stmt : proven) steps
    let newTheorem = Implies stmt (head innerProven)
    if newTheorem == newStatement
      then return (newStatement : proven)
      else Left $ "Assumption proved " ++ prettyPrint newTheorem ++
           ", but goal was " ++ prettyPrint newStatement

  Detachment stmt -> do
    assumedStatements <- findDetachments stmt newStatement
    mapM_ (requireProven proven) assumedStatements
    return (newStatement : proven)

  Contrapositive stmt -> do
    requireProven proven stmt
    if isContraOf newStatement stmt || isContraOf stmt newStatement
      then return (newStatement : proven)
      else Left $ prettyPrint newStatement ++ " is not a contrapositive of " ++ prettyPrint stmt

  Demorgan stmt -> do
    requireProven proven stmt
    if isDemorganOf newStatement stmt
      then return (newStatement : proven)
      else Left $ "cannot derrive " ++ prettyPrint newStatement ++ " from " ++
           prettyPrint stmt ++ " with De Morgan's law"

  Switch stmt -> do
    requireProven proven stmt
    if isSwitchOf stmt newStatement || isSwitchOf newStatement stmt
      then return (newStatement : proven)
      else Left  $ "cannot derrive " ++ prettyPrint newStatement ++ " from " ++
           prettyPrint stmt ++ " with a switch"

findDetachments :: Statement -> Statement -> Result [Statement]
findDetachments proven goal
  | proven == goal                        = return []
  | Just stmt <- isDetachment proven goal = return [stmt]
  | otherwise                             = case (proven, goal) of
      (Not a1, Not b1) ->
        findDetachments a1 b1
      (And a1 a2, And b1 b2) -> do
        stmts1 <- findDetachments a1 b1
        stmts2 <- findDetachments a2 b2
        return $ stmts1 ++ stmts2
      (Or a1 a2, Or b1 b2) -> do
        stmts1 <- findDetachments a1 b1
        stmts2 <- findDetachments a2 b2
        return $ stmts1 ++ stmts2
      (Implies a1 a2, Implies b1 b2) -> do
        stmts1 <- findDetachments a1 b1
        stmts2 <- findDetachments a2 b2
        return $ stmts1 ++ stmts2
      _ ->
        Left $ prettyPrint goal ++ " is not a detachment of " ++ prettyPrint proven

isDetachment :: Statement -> Statement -> Maybe Statement
isDetachment (Implies a b) goal =
  if b == goal then Just a else Nothing
isDetachment _ _ =
  Nothing

impliedByDoubleNegation :: Statement -> Statement -> Bool
impliedByDoubleNegation = equalOrEquivalent test
  where test a             (Not (Not b)) = a == b
        test (Not (Not a)) b             = a == b
        test _             _             = False

isContraOf :: Statement -> Statement -> Bool
isContraOf = equalOrEquivalent test
  where test (Implies x y) (Implies notY notX) =
          (Not x) == notX && (Not y) == notY
        test _             _                   =
          False

isDemorganOf :: Statement -> Statement -> Bool
isDemorganOf = equalOrEquivalent test
  where test (And (Not x1) (Not y1)) (Not (Or x2 y2))        =
          x1 == x2 && y1 == y2
        test (Not (Or x1 y1))        (And (Not x2) (Not y2)) =
          x1 == x2 && y1 == y2
        test _                       _                       =
          False

isSwitchOf :: Statement -> Statement -> Bool
isSwitchOf = equalOrEquivalent test
  where test (Or x1 y1) (Implies (Not x2) y2) =
          x1 == x2 && y1 == y2
        test _          _                     =
          False

equalOrEquivalent :: (Statement -> Statement -> Bool) -> Statement -> Statement -> Bool
equalOrEquivalent test a b
  | a == b    = True
  | test a b  = True
  | otherwise = case (a, b) of
      (Not a1, Not b1) ->
        equalOrEquivalent test a1 b1
      (And a1 a2, And b1 b2) ->
        equalOrEquivalent test a1 b1 && equalOrEquivalent test a2 b2
      (Or a1 a2, Or b1 b2) ->
        equalOrEquivalent test a1 b1 && equalOrEquivalent test a2 b2
      (Implies a1 a2, Implies b1 b2) ->
        equalOrEquivalent test a1 b1 && equalOrEquivalent test a2 b2
      _ ->
        False

requireProven :: Proven -> Statement -> Result ()
requireProven proven stmt =
  if stmt `elem` proven
  then return ()
  else Left $ "Cannot prove statement " ++ prettyPrint stmt ++ " given:\n" ++
       unlines (map prettyPrint proven)

prettyPrint :: Statement -> String
prettyPrint stmt = case stmt of
  Atom    a   -> [a]
  Not     a   -> "~" ++ prettyPrint a
  And     a b -> "(" ++ prettyPrint a ++ " & " ++ prettyPrint b ++ ")"
  Or      a b -> "(" ++ prettyPrint a ++ " | " ++ prettyPrint b ++ ")"
  Implies a b -> "(" ++ prettyPrint a ++ " > " ++ prettyPrint b ++ ")"

printIndent :: Int -> Derrivation -> String
printIndent indent (Derrivation steps) =
  unlines $ map (printStep indent) steps

printStep :: Int -> Step -> String
printStep indent (stmt, rule) =
  showIndent indent ++ prettyPrint stmt ++ printRule indent rule

showIndent :: Int -> String
showIndent n =
  if n < 1 then "" else "  " ++ showIndent (n - 1)

printRule :: Int -> Rule -> String
printRule indent rule = case rule of
  Given                  -> ""
  Join                   -> " <- join"
  Separate       stmt    -> " <- separate "       ++ prettyPrint stmt
  DoubleNegation stmt    -> " <- doublenegation " ++ prettyPrint stmt
  Detachment     stmt    -> " <- detach "         ++ prettyPrint stmt
  Contrapositive stmt    -> " <- contrapositive " ++ prettyPrint stmt
  Demorgan       stmt    -> " <- demorgan "       ++ prettyPrint stmt
  Switch         stmt    -> " <- switch "         ++ prettyPrint stmt
  Assumption stmt derriv ->
    " <- [\n" ++
    showIndent (indent + 1) ++ prettyPrint stmt ++ "\n" ++
    printIndent (indent + 1) derriv ++
    showIndent indent ++ "]"

type Result a = Either String a

parseStatement :: String -> Result Statement
parseStatement s = do
  (stmt, rest) <- parseNext s
  if nonBlank rest
    then Left $ "left over after statement: " ++ rest
    else return stmt

parseDerrivation :: String -> Result Derrivation
parseDerrivation s = do
  steps <- parseSteps (filter nonBlank $ lines s) []
  return $ Derrivation steps

nonBlank :: String -> Bool
nonBlank "" = False
nonBlank (c:cs) = case c of
  ' ' -> nonBlank cs
  '#' -> False -- this starts a comment
  _   -> True -- any other character is probably part of a statement

isBlank = not . nonBlank

parseSteps :: [String] -> [Step] ->  Result [Step]
parseSteps [] steps =
  return $ reverse steps
parseSteps (line:ls) steps = do
  (stmt, lineRest) <- parseNext line
  (rule, lines) <- parseRule lineRest ls
  let step = (stmt, rule)
  parseSteps lines (step : steps)

parseRule :: String -> [String] -> Result (Rule, [String])
parseRule lineRest lines
  | isBlank lineRest = return (Given, lines)
  | otherwise = do
      remainder <- readArrow lineRest
      let (ruleName, rest) = splitFirst remainder
      case ruleName of
        "join" -> do
          requireBlank rest
          return (Join, lines)
        "separate" ->
          parseRuleFor Separate rest lines
        "doublenegation" ->
          parseRuleFor DoubleNegation rest lines
        "detach" ->
          parseRuleFor Detachment rest lines
        "contrapositive" ->
          parseRuleFor Contrapositive rest lines
        "demorgan" ->
          parseRuleFor Demorgan rest lines
        "switch" ->
          parseRuleFor Switch rest lines
        "[" -> do
          requireBlank rest
          parseAssumption lines
        _ ->
          Left $ "invalid rule name: " ++ ruleName


parseAssumption :: [String] -> Result (Rule, [String])
parseAssumption [] = Left "unexpected end of assumption"
parseAssumption (l:ls) = do
  assumedStmt <- parseStatement l
  (steps, remainingLines) <- parseAssumptionSteps ls []
  return (Assumption assumedStmt (Derrivation steps), remainingLines)

parseAssumptionSteps :: [String] -> [Step] -> Result ([Step], [String])
parseAssumptionSteps [] steps = Left "unfinished assumption"
parseAssumptionSteps (l:ls) steps
  | trim l == "]" =
    return (reverse steps, ls)
  | otherwise = do
      (stmt, lineRest) <- parseNext l
      (rule, lines) <- parseRule lineRest ls
      let step = (stmt, rule)
      parseAssumptionSteps lines (step : steps)

parseRuleFor :: (Statement -> Rule) -> String -> [String] -> Result (Rule, [String])
parseRuleFor builder rest lines = do
  stmt <- parseStatement rest
  return (builder stmt, lines)

readArrow :: String -> Result String
readArrow ('<':'-':rest) = return (eatWhitespace rest)
readArrow (' ':rest) = readArrow rest
readArrow s = Left $ "Expected <-, got " ++ s

eatWhitespace :: String -> String
eatWhitespace (' ':rest) = eatWhitespace rest
eatWhitespace s = s

trim = eatWhitespace . reverse . eatWhitespace . reverse

splitFirst :: String -> (String, String)
splitFirst "" = ("", "")
splitFirst (' ':s) = ("", eatWhitespace s)
splitFirst (c:s) =
  let (before, after) = splitFirst s
  in (c:before, after)

requireBlank :: String -> Result ()
requireBlank s =
  if isBlank s then return ()
  else Left $ "Expected the end of a line, got " ++ s

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

checkFile :: String -> Either String ()
checkFile content = do
  parsed <- parseDerrivation content
  checkDerrivation parsed

run :: String -> IO ()
run name = do
  content <- readFile name
  case checkFile content of
    Left err ->
      putStrLn err
    Right _ ->
      putStrLn "valid"

main = do
  args <- getArgs
  case args of
    [filename] -> run filename
    _ -> putStrLn "usage: propositionalcalculus <examples/example.txt>"
