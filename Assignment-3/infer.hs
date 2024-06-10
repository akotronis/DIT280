type Fact = String
type Rule = ([Fact], Fact)
type Rules = [Rule]
type Facts = [Fact]


-- Remove duplicate values from a list (From lectures)
rmDups [] = []
rmDups (x:xs) = x : filter (/= x) (rmDups xs)


-- Helper function to check if one list is a subset of another
subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs


-- Function to apply a single rule
applyRule :: Facts -> Rule -> [Fact]
applyRule knownFacts (conditions, conclusion)
  | all (`elem` knownFacts) conditions = [conclusion]
  | otherwise = []


-- Function to apply all rules to known facts and concatenate results
applyRules :: Rules -> Facts -> [Fact]
applyRules rules knownFacts = concatMap (applyRule knownFacts) rules


-- Helper function to iteratively apply rules until no new facts are derived
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


-- Tests
rules1 :: Rules
rules1 = [ (["It's morning", "Temperature is below 18°C"], "Turn on heating")
         , (["Temperature is below 18°C", "Turn on heating"], "Check for open windows")
         , (["No one is home", "Turn on heating"], "Send notification to homeowner")
         ]
initialFacts1 :: Facts
initialFacts1 = ["It's morning", "Temperature is below 18°C"]


rules2 :: Rules
rules2 = [ (["Motion detected", "It's night"], "Turn on security lights")
         , (["Motion detected", "No one is home"], "Send security alert")
         , (["No one is home", "Turn on security lights"], "Record security footage")
         , (["Record security footage"], "Do not eat all the icecream")
         ]
initialFacts2 :: Facts
initialFacts2 = ["Motion detected", "It's night", "No one is home"]


rules3 :: Rules
rules3 = [ (["Application received", "Credit score is above 700"], "Approve loan")
         , (["Approve loan"], "Notify applicant")
         , (["Application received", "Credit score is below 600"], "Deny loan")
         , (["Deny loan"], "Notify applicant")
         , (["Deny loan"], "Give applicant a big hug insted")
         , (["Application received", "Credit score is between 600 and 700"], "Request additional documents")
         , (["Request additional documents", "Documents received"], "Approve loan")
         ]
initialFacts3a :: Facts
initialFacts3a = ["Application received", "Credit score is between 600 and 700", "Documents received"]
initialFacts3b :: Facts
initialFacts3b = ["Application received", "Credit score is below 600"]


main :: IO ()
main = do
  let inferredFacts = infer rules1 initialFacts1
  print( "======= Scenario 1 =======")
  mapM_ putStrLn inferredFacts
  putStrLn ""
  -- It's morning
  -- Temperature is below 18°C
  -- Turn on heating
  -- Check for open windows
  
  let inferredFacts = infer rules2 initialFacts2
  print( "======= Scenario 2 =======")
  mapM_ putStrLn inferredFacts
  putStrLn ""
  -- Motion detected
  -- It's night
  -- No one is home
  -- Turn on security lights
  -- Send security alert
  -- Record security footage
  -- Do not eat all the icecream
  
  let inferredFacts = infer rules3 initialFacts3a
  print( "======= Scenario 3a =======")
  mapM_ putStrLn inferredFacts
  putStrLn ""
  -- Application received
  -- Credit score is between 600 and 700
  -- Documents received
  -- Request additional documents
  -- Approve loan
  -- Notify applicant
  
  let inferredFacts = infer rules3 initialFacts3b
  print( "======= Scenario 3b =======")
  mapM_ putStrLn inferredFacts
  putStrLn ""
  -- Application received
  -- Credit score is below 600
  -- Deny loan
  -- Notify applicant
  -- Give applicant a big hug insted

