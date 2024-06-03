data LogicExpr = Var String
               | Or LogicExpr LogicExpr
               | And LogicExpr LogicExpr
               | Not LogicExpr
               deriving Show

-- Push Negations
pushNegation :: LogicExpr -> LogicExpr
pushNegation (Var name) = Var name
pushNegation (Not (Var name)) = Not (Var name)
pushNegation (Not (Not inner)) = pushNegation inner
pushNegation (Or left right) = Or (pushNegation left) (pushNegation right)
pushNegation (And left right) = And (pushNegation left) (pushNegation right)
pushNegation (Not (Or left right)) = And (pushNegation (Not left)) (pushNegation (Not right))
pushNegation (Not (And left right)) = Or (pushNegation (Not left)) (pushNegation (Not right))

-- Distributive law
distribute :: LogicExpr -> LogicExpr -> LogicExpr
distribute x (And a b) = And (distribute x a) (distribute x b)
distribute (And a b) y = And (distribute a y) (distribute b y)
distribute x y = Or x y

-- Intermediate function that uses distributive law on an assumed pushed negation input
cnfHelper :: LogicExpr -> LogicExpr
cnfHelper (Var name) = Var name
cnfHelper (Not expr) = Not (cnfHelper expr)
cnfHelper (Or left right) = distribute (cnfHelper left) (cnfHelper right)
cnfHelper (And left right) = And (cnfHelper left) (cnfHelper right)

-- Main function. Uses the above functions to return the final result
cnf :: LogicExpr -> LogicExpr
cnf expr = cnfHelper (pushNegation expr)



-- Tests
main :: IO ()
main = do
  -- Push Negation
  print("===== pushNegation =====")
  print ( pushNegation (Var "a") ) -- Var "a"
  print ( pushNegation (Not (Var "a")) ) -- Not (Var "a")
  print ( pushNegation (Not (Not (Var "a"))) ) -- Var "a"
  print ( pushNegation (Not (Or (Var "a") (Var "b"))) ) -- And (Not (Var "a")) (Not (Var "b"))
  print ( pushNegation (Not (And (Var "a") (Var "b"))) ) -- Or (Not (Var "a")) (Not (Var "b"))
  print ( pushNegation (Or (Var "a") (Var "b")) ) -- Or (Var "a") (Var "b")
  print ( pushNegation (And (Var "a") (Var "b")) ) -- And (Var "a") (Var "b")
  print ( pushNegation (Or (Var "a") (And (Var "a") (Var "c"))) ) -- Or (Var "a") (And (Var "a") (Var "c"))
  putStrLn ""
  
  -- Distributive Law
  print("===== Distributive Law =====")
  print ( distribute (Var "a") (Var "b") ) -- Or (Var "a") (Var "b")
  print ( distribute (Var "a") (And (Var "b") (Var "c")) ) -- And (Or (Var "a") (Var "b")) (Or (Var "a") (Var "c"))
  print ( distribute (And (Var "b") (Var "c")) (Var "a") ) -- And (Or (Var "b") (Var "a")) (Or (Var "c") (Var "a"))
  putStrLn ""
  
  -- Conjuctive Normal For
  print("===== Conjuctive Normal Form =====")
  print( cnf (Var "a") ) -- Var "a"
  print( cnf (Not (Var "a")) ) -- Not (Var "a")
  print( cnf (Or (Var "a") (Var "b")) ) -- Or (Var "a") (Var "b")
  print( cnf (And (Var "a") (Var "b")) ) -- And (Var "a") (Var "b")
  print( cnf (Or (Var "a") (Or (Var "a") (Var "c"))) ) -- Or (Var "a") (Or (Var "a") (Var "c"))
  print( cnf (And (Var "a") (And (Var "a") (Var "c"))) ) -- And (Var "a") (And (Var "a") (Var "c"))
  print( cnf (Or (Var "a") (And (Var "b") (Var "c"))) ) -- And (Or (Var "a") (Var "b")) (Or (Var "a") (Var "c"))
  print( cnf (Or (And (Var "b") (Var "c")) (Var "a")) ) -- And (Or (Var "b") (Var "a")) (Or (Var "c") (Var "a"))
  print( cnf (And (Var "a") (Or (Var "b") (Var "c"))) ) -- And (Var "a") (Or (Var "b") (Var "c"))
  print( cnf (And (Or (Var "b") (Var "c")) (Var "a")) ) -- And (Or (Var "b") (Var "c")) (Var "a")
  print( cnf (Or (And (Var "a") (Var "b")) (And (Var "c") (Var "d"))) ) -- And (And (Or (Var "a") (Var "c")) (Or (Var "b") (Var "c"))) (And (Or (Var "a") (Var "d")) (Or (Var "b") (Var "d")))
  print( cnf (Or (Var "a") (Not (And (Var "b") (Var "c")))) ) -- Or (Var "a") (Or (Not (Var "b")) (Not (Var "c")))
  print( cnf (Not (And (Var "a") (Var "b")) )) -- Or (Not (Var "a")) (Not (Var "b"))