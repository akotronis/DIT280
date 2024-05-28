data LogicExpr = Var String
               | Or LogicExpr LogicExpr
               | And LogicExpr LogicExpr
               | Not LogicExpr
               deriving Show

-- cnf :: LogicExpr -> LogicExpr
-- cnf expr = cnfHelper (pushNegation expr) []

pushNegation :: LogicExpr -> LogicExpr
pushNegation (Var name) = Var name
pushNegation (Not (Var name)) = Not (Var name)
pushNegation (Not (Not inner)) = inner
pushNegation (Not (Or left right)) = And (pushNegation (Not left)) (pushNegation (Not right))
pushNegation (Not (And left right)) = Or (pushNegation (Not left)) (pushNegation (Not right))
pushNegation (Or left right) = Or (pushNegation left) (pushNegation right)
pushNegation (And left right) = And (pushNegation left) (pushNegation right)

-- cnfHelper :: LogicExpr -> [LogicExpr] -> LogicExpr
-- cnfHelper (Var name) acc = And (acc ++ [Var name])
-- cnfHelper (Not expr) acc = cnfHelper (pushNegation expr) (acc ++ [Not (head acc)]) -- Apply De Morgan's after pushing negation inwards 
-- cnfHelper (Or left right) acc = let (leftCnf, leftAcc) = cnfHelper left []
--                                 (rightCnf, rightAcc) = cnfHelper right leftAcc
--                             in And (acc ++ leftAcc ++ rightAcc ++ [Or leftCnf rightCnf]) -- Apply distributive law
-- cnfHelper (And left right) acc = let (leftCnf, leftAcc) = cnfHelper left acc
--                                 (rightCnf, rightAcc) = cnfHelper right leftAcc
--                             in And (leftAcc ++ rightAcc ++ [And leftCnf rightCnf]) -- Apply distributive law

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