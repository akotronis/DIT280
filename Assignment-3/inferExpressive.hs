import Data.Maybe (catMaybes)
import Data.List (nub)
import Control.Monad (guard)

data Var = Var String deriving (Eq, Show)
type Fact = [Either String Var]
type Rule = ([Fact], Fact)
type Rules = [Rule]
type Facts = [Fact]

-- Custom function to remove duplicates
rmDups :: Eq a => [a] -> [a]
rmDups [] = []
rmDups (x:xs) = x : filter (/= x) (rmDups xs)

type Substitution = [(Var, String)]

-- Apply a substitution to a fact
applySubstitution :: Substitution -> Fact -> Fact
applySubstitution subs = map (\e -> case e of
                                      Left s -> Left s
                                      Right v -> Left (lookupSubstitution v subs))

-- Lookup a variable in a substitution
lookupSubstitution :: Var -> Substitution -> String
lookupSubstitution v subs = case lookup v subs of
                              Just s  -> s
                              Nothing -> error "Unbound variable"

-- Attempt to match a fact to a condition, generating a substitution if successful
match :: Fact -> Fact -> Maybe Substitution
match [] [] = Just []
match (Left s1 : xs1) (Left s2 : xs2) | s1 == s2 = match xs1 xs2
match (Left s : xs1) (Right v : xs2) = do
  rest <- match xs1 xs2
  return ((v, s) : rest)
match (Right v : xs1) (Left s : xs2) = do
  rest <- match xs1 xs2
  return ((v, s) : rest)
match (Right v1 : xs1) (Right v2 : xs2) | v1 == v2 = match xs1 xs2
match _ _ = Nothing



-- Apply a single rule to known facts
applyRule :: Facts -> Rule -> [Fact]
applyRule knownFacts (conditions, conclusion) = do
  sub <- mapM (matchFact knownFacts) conditions
  let unifiedSub = foldr1 (+++) sub
  return (applySubstitution unifiedSub conclusion)

-- Helper function to match a fact from known facts
matchFact :: Facts -> Fact -> [Substitution]
matchFact knownFacts condition = catMaybes $ map (match condition) knownFacts

-- Apply all rules to known facts and concatenate results
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

-- Helper function to check if one list is a subset of another
subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

-- Function to combine substitutions
(+++) :: Substitution -> Substitution -> Substitution
(+++) s1 s2 = s1 ++ filter (\(v, _) -> v `notElem` map fst s1) s2


-- Scenario 1: Room lighting control
rules1 :: Rules
rules1 = [ ([ [Left "Room", Right (Var "X")]
            , [Left "MotionDetected", Right (Var "X")] ]
          , [Left "TurnOnLights", Right (Var "X")] )
         ]

initialFacts1 :: Facts
initialFacts1 = [ [Left "Room", Left "LivingRoom"]
                , [Left "Room", Left "BedRoom1"]
                , [Left "Room", Left "BedRoom2"]
                , [Left "MotionDetected", Left "BedRoom1"]
                ]

expectedOutcome1 :: Facts
expectedOutcome1 = [ [Left "Room", Left "LivingRoom"]
                   , [Left "Room", Left "BedRoom1"]
                   , [Left "Room", Left "BedRoom2"]
                   , [Left "MotionDetected", Left "BedRoom1"]
                   , [Left "TurnOnLights", Left "BedRoom1"]
                   ]

-- Scenario 2: Extended loan approval system
rules2 :: Rules
rules2 = [ ([ [Left "Application", Right (Var "Applicant")]
            , [Left "CreditScoreAbove700", Right (Var "Applicant")] ]
          , [Left "ApproveLoan", Right (Var "Applicant")] )
         , ([ [Left "Application", Right (Var "Applicant")]
            , [Left "CreditScoreBelow600", Right (Var "Applicant")] ]
          , [Left "DenyLoan", Right (Var "Applicant")] )
         , ([ [Left "ApproveLoan", Right (Var "Applicant")] ]
          , [Left "NotifyApplicant", Right (Var "Applicant")] )
         , ([ [Left "DenyLoan", Right (Var "Applicant")] ]
          , [Left "NotifyApplicant", Right (Var "Applicant")] )
         ]

initialFacts2 :: Facts
initialFacts2 = [ [Left "Application", Left "Alice"]
                , [Left "CreditScoreAbove700", Left "Alice"]
                , [Left "Application", Left "Bob"]
                , [Left "CreditScoreBelow600", Left "Bob"]
                ]

expectedOutcome2 :: Facts
expectedOutcome2 = [ [Left "Application", Left "Alice"]
                   , [Left "CreditScoreAbove700", Left "Alice"]
                   , [Left "ApproveLoan", Left "Alice"]
                   , [Left "NotifyApplicant", Left "Alice"]
                   , [Left "Application", Left "Bob"]
                   , [Left "CreditScoreBelow600", Left "Bob"]
                   , [Left "DenyLoan", Left "Bob"]
                   , [Left "NotifyApplicant", Left "Bob"]
                   ]

-- Scenario 3: Medical diagnosis with variables
rules3 :: Rules
rules3 = [ ([ [Left "Symptom", Right (Var "X"), Left "Fever"]
            , [Left "Symptom", Right (Var "X"), Left "Cough"] ]
          , [Left "Diagnose", Right (Var "X"), Left "Flu"] )
         , ([ [Left "Symptom", Right (Var "X"), Left "Fever"]
            , [Left "Symptom", Right (Var "X"), Left "Rash"] ]
          , [Left "Diagnose", Right (Var "X"), Left "Measles"] )
         ]

initialFacts3 :: Facts
initialFacts3 = [ [Left "Symptom", Left "Patient1", Left "Fever"]
                , [Left "Symptom", Left "Patient1", Left "Cough"]
                , [Left "Symptom", Left "Patient2", Left "Fever"]
                , [Left "Symptom", Left "Patient2", Left "Rash"]
                ]

expectedOutcome3 :: Facts
expectedOutcome3 = [ [Left "Symptom", Left "Patient1", Left "Fever"]
                   , [Left "Symptom", Left "Patient1", Left "Cough"]
                   , [Left "Diagnose", Left "Patient1", Left "Flu"]
                   , [Left "Symptom", Left "Patient2", Left "Fever"]
                   , [Left "Symptom", Left "Patient2", Left "Rash"]
                   , [Left "Diagnose", Left "Patient2", Left "Measles"]
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

  -- Scenario 3
  let inferredFacts = infer rules3 initialFacts3
  putStrLn "Inferred Facts for Scenario 3:"
  mapM_ print inferredFacts
  putStrLn ""
