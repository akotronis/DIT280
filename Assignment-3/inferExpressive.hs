import Data.Maybe (catMaybes) -- Return the Justs

data Var = Var String deriving (Eq, Show)
type Fact = [(String, Var)]
type Rule = ([Fact], Fact)
type Rules = [Rule]
type Facts = [Fact]


-- Remove duplicate values from a list (From lectures)
rmDups [] = []
rmDups (x:xs) = x : filter (/= x) (rmDups xs)


-- Helper function to check if one list is a subset of another
subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs


type Substitution = [(Var, String)]

-- Apply a substitution to a fact
applySubstitution :: Substitution -> Fact -> Fact
applySubstitution subs = map (\(s, v) -> (s, Var (lookupSubstitution v subs)))


-- Lookup a variable in a substitution
lookupSubstitution :: Var -> Substitution -> String
lookupSubstitution (Var v) subs = case lookup (Var v) subs of
                                      Just s  -> s
                                      Nothing -> error "Unbound variable"


-- Attempt to match a fact to a condition, generating a substitution if successful
match :: Fact -> Fact -> Maybe Substitution
match [] [] = Just []
match ((s1, v1):xs1) ((s2, v2):xs2) | s1 == s2 && v1 == v2 = match xs1 xs2
match ((s1, v1):xs1) ((s2, v2):xs2) | s1 == s2 = do
                                            rest <- match xs1 xs2
                                            return ((v1, s2):rest)
match _ _ = Nothing


-- Helper function to match a fact from known facts
matchFact :: Facts -> Fact -> [Substitution]
matchFact knownFacts condition = catMaybes (map (match condition) knownFacts)


-- Apply a single rule to known facts
applyRule :: Facts -> Rule -> [Fact]
applyRule knownFacts (conditions, conclusion) = do
  sub <- mapM (matchFact knownFacts) conditions
  let unifiedSub = foldr1 (+++) sub
  return (applySubstitution unifiedSub conclusion)


-- Function to apply all rules to known facts and concatenate results
applyRules :: Rules -> Facts -> [Fact]
applyRules rules knownFacts = concatMap (applyRule knownFacts) rules


-- Function to iteratively apply rules until no new facts are derived
iterRules :: Rules -> Facts -> Facts
iterRules rules knownFacts =
  let newFacts = rmDups (applyRules rules knownFacts)
      allFacts = rmDups (knownFacts ++ newFacts)
  in if subset newFacts knownFacts
     then knownFacts
     else iterRules rules allFacts


-- Function to infer all possible facts
infer :: Rules -> Facts -> Facts
infer rules initialFacts = iterRules rules initialFacts


-- Function to combine substitutions
(+++) :: Substitution -> Substitution -> Substitution
(+++) s1 s2 = s1 ++ filter (\(v, _) -> v `notElem` map fst s1) s2


-- Test scenarios
rules1 :: Rules
rules1 = [ ([ [("Room", Var "X")
             , ("MotionDetected", Var "X")] ]
           , [("TurnOnLights", Var "X")] )
         ]

initialFacts1 :: Facts
initialFacts1 = [ [("Room", Var "LivingRoom")]
                , [("Room", Var "BedRoom1")]
                , [("Room", Var "BedRoom2")]
                , [("MotionDetected", Var "BedRoom1")]
                ]

expectedOutcome1 :: Facts
expectedOutcome1 = [ [("Room", Var "LivingRoom")]
                   , [("Room", Var "BedRoom1")]
                   , [("Room", Var "BedRoom2")]
                   , [("MotionDetected", Var "BedRoom1")]
                   , [("TurnOnLights", Var "BedRoom1")]
                   ]

rules2 :: Rules
rules2 = [ ([ [("Application", Var "Applicant")
             , ("CreditScoreAbove700", Var "Applicant")] ]
           , [("ApproveLoan", Var "Applicant")] )
         , ([ [("Application", Var "Applicant")
             , ("CreditScoreBelow600", Var "Applicant")] ]
           , [("DenyLoan", Var "Applicant")] )
         , ([ [("ApproveLoan", Var "Applicant")] ]
           , [("NotifyApplicant", Var "Applicant")] )
         , ([ [("DenyLoan", Var "Applicant")] ]
           , [("NotifyApplicant", Var "Applicant")] )
         ]

initialFacts2 :: Facts
initialFacts2 = [ [("Application", Var "Alice")]
                , [("CreditScoreAbove700", Var "Alice")]
                , [("Application", Var "Bob")]
                , [("CreditScoreBelow600", Var "Bob")]
                ]

expectedOutcome2 :: Facts
expectedOutcome2 = [ [("Application", Var "Alice")]
                   , [("CreditScoreAbove700", Var "Alice")]
                   , [("ApproveLoan", Var "Alice")]
                   , [("NotifyApplicant", Var "Alice")]
                   , [("Application", Var "Bob")]
                   ]

main :: IO ()
main = do
  -- Scenario 1
  let inferredFacts = infer rules1 initialFacts1
  putStrLn "Inferred Facts for Scenario 1:"
  mapM_ print inferredFacts
  putStrLn ""

  -- Scenario 2
  let inferredFacts = infer rules2 initialFacts2
  putStrLn "Inferred Facts for Scenario 2:"
  mapM_ print inferredFacts
  putStrLn ""