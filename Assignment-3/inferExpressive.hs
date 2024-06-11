import Data.Maybe (maybeToList)

type Condition = (String, String)
type Fact = [String]
type Rule = ([Condition], Fact)
type KnowledgeBase = [Fact]
type Substitution = (String, String)

unify :: [Condition] -> Fact -> Maybe Substitution
unify [] [] = Just ("", "")
unify ((condName, condVar):restCond) (fact:restFact)
    | condName == fact && condVar == factVal = unify restCond restFact
    | otherwise = Nothing
  where
    factVal = head restFact
unify _ _ = Nothing

applySubstitutionToFact :: Substitution -> Fact -> Fact
applySubstitutionToFact _ [] = []
applySubstitutionToFact (var, newVal) (x:xs)
    | x == var = newVal : applySubstitutionToFact (var, newVal) xs
    | otherwise = x : applySubstitutionToFact (var, newVal) xs

infer :: KnowledgeBase -> Rule -> KnowledgeBase
infer kb (conditions, conclusion) =
    let potentialSubstitutions = [unify conditions fact | fact <- kb]
        inferredFacts = map (\subst -> applySubstitutionToFact subst conclusion) (concatMap maybeToList potentialSubstitutions)
    in kb ++ inferredFacts

initialFacts :: KnowledgeBase
initialFacts = [["Room", "LivingRoom"], ["Room", "BedRoom1"], ["Room", "BedRoom2"], ["MotionDetected", "BedRoom1"]]

rules :: [Rule]
rules = [ ( [("Room", "X"), ("MotionDetected", "X") ], ["TurnOnLights", "X"]) ]

inferredFacts :: KnowledgeBase
inferredFacts = foldl infer initialFacts rules

main :: IO ()
main = print inferredFacts
