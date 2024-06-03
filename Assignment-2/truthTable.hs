-- Define the data type for logical expressions
data LogicExpr
    = Var String
    | And LogicExpr LogicExpr
    | Or LogicExpr LogicExpr
    | Not LogicExpr
    deriving (Show, Eq)

-- Define the type aliases
type Binding = [(String, Bool)]
type TruthTable = [(Binding, Bool)]

-- Function to evaluate a logical expression given a variable binding
eval :: Binding -> LogicExpr -> Bool
eval env (Var x)      = lookupVar x env
eval env (And e1 e2)  = eval env e1 && eval env e2
eval env (Or e1 e2)   = eval env e1 || eval env e2
eval env (Not e)      = not (eval env e)

-- Helper function to lookup the value of a variable in the binding
lookupVar :: String -> Binding -> Bool
lookupVar x env = case lookup x env of
                    Just val -> val
                    Nothing  -> error ("Variable " ++ x ++ " not found in the binding")

-- Function to get all variables in a logical expression
getVars :: LogicExpr -> [String]
getVars (Var x)      = [x]
getVars (And e1 e2)  = getVars e1 ++ getVars e2
getVars (Or e1 e2)   = getVars e1 ++ getVars e2
getVars (Not e)      = getVars e

-- Function to generate all possible bindings for a list of variables
generateBindings :: [String] -> [Binding]
generateBindings []     = [[]]
generateBindings (x:xs) = [ (x, val):binding | val <- [True, False], binding <- generateBindings xs ]

-- Function to compute the truth table for a logical expression
truthTable :: LogicExpr -> TruthTable
truthTable expr = [(binding, eval binding expr) | binding <- generateBindings (uniqueVars expr)]
  where
    uniqueVars = rmDups . getVars

-- Remove duplicate values from a list (From lectures)
rmDups [] = []
rmDups (x:xs) = x : filter (/= x) (rmDups xs)



-- Tests
main :: IO ()
main = do
  -- Truth Table
  print("===== truthTable =====")
  -- a∨b 
  print ( truthTable (Or (Var "a") (Var "b")) ) -- [([("a",True),("b",True)],True),([("a",True),("b",False)],True),([("a",False),("b",True)],True),([("a",False),("b",False)],False)]
  -- a∧b 
  print ( truthTable (And (Var "a") (Var "b")) ) -- [([("a",True),("b",True)],True),([("a",True),("b",False)],False),([("a",False),("b",True)],False),([("a",False),("b",False)],False)]
  -- ¬a∨b (a⇒b)
  print ( truthTable (Or (Not (Var "a")) (Var "b")) ) -- [([("a",True),("b",True)],True),([("a",True),("b",False)],False),([("a",False),("b",True)],True),([("a",False),("b",False)],True)]
  -- (¬a∧¬b)∨(a∧b) (a⇔b)
  print ( truthTable (Or (And (Not (Var "a")) (Not (Var "b"))) (And (Var "a") (Var "b"))) ) -- [([("a",True),("b",True)],True),([("a",True),("b",False)],False),([("a",False),("b",True)],False),([("a",False),("b",False)],True)]
  -- a∧(a∨b)
  print ( truthTable (And (Var "a") (Or (Var "a") (Var "b"))) ) -- [([("a",True),("b",True)],True),([("a",True),("b",False)],True),([("a",False),("b",True)],False),([("a",False),("b",False)],False)]