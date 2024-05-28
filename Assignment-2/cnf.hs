data LogicExpr = Var String
               | Or LogicExpr LogicExpr
               | And LogicExpr LogicExpr
               | Not LogicExpr
               deriving Show

pushNegation :: LogicExpr -> LogicExpr
pushNegation (Var name) = Var name
pushNegation (Not (Var name)) = Not (Var name)
pushNegation (Not (Not inner)) = pushNegation inner
pushNegation (Not (Or left right)) = And (pushNegation (Not left)) (pushNegation (Not right))
pushNegation (Not (And left right)) = Or (pushNegation (Not left)) (pushNegation (Not right))
pushNegation (Or left right) = Or (pushNegation left) (pushNegation right)
pushNegation (And left right) = And (pushNegation left) (pushNegation right)

cnfHelper :: LogicExpr -> LogicExpr
cnfHelper (Var name) = Var name
cnfHelper (Not expr) = Not (cnfHelper expr)
cnfHelper (Or left right) = distribute (cnfHelper left) (cnfHelper right)
cnfHelper (And left right) = And (cnfHelper left) (cnfHelper right)

distribute :: LogicExpr -> LogicExpr -> LogicExpr
distribute (And a b) y = And (distribute a y) (distribute b y)
distribute x (And a b) = And (distribute x a) (distribute x b)
distribute x y = Or x y

cnf :: LogicExpr -> LogicExpr
cnf expr = cnfHelper (pushNegation expr)




-- Tests
main :: IO ()
main = do
  -- pushNegation -Basic-
  print ( pushNegation (Var "a") ) -- Var "a"
  print ( pushNegation (Not (Var "a")) ) -- Not (Var "a")
  print ( pushNegation (Not (Not (Var "a"))) ) -- Var "a"
  print ( pushNegation (Not (Or (Var "a") (Var "b"))) ) -- And (Not (Var "a")) (Not (Var "b"))
  print ( pushNegation (Not (And (Var "a") (Var "b"))) ) -- Or (Not (Var "a")) (Not (Var "b"))
  print ( pushNegation (Or (Var "a") (Var "b")) ) -- Or (Var "a") (Var "b")
  print ( pushNegation (And (Var "a") (Var "b")) ) -- And (Var "a") (Var "b")
  
  -- pushNegation -Basic-