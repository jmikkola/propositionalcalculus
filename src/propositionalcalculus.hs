data Statement
  = Atom String
  | Not Statement
  | And Statement Statement
  | Or Statement Statement
  | Implies Statement Statement
  deriving (Eq, Show)

prettyPrint :: Statement -> String
prettyPrint stmt = case stmt of
  Atom    a   -> a
  Not     a   -> "~" ++ prettyPrint a
  And     a b -> "<" ++ prettyPrint a ++ "∧" ++ prettyPrint b ++ ">"
  Or      a b -> "<" ++ prettyPrint a ++ "∨" ++ prettyPrint b ++ ">"
  Implies a b -> "<" ++ prettyPrint a ++ "→" ++ prettyPrint b ++ ">"

main = putStrLn "Hello world from propositionalcalculus"
