import Data.List (partition)

-- checks if a relation is an equivalence relation (reflexive, symmetric and transitive) on the set
is_equiv :: (a -> a -> Bool) -> [a] -> Bool
is_equiv r xs = reflexive && symmetric && transitive
  where
    reflexive  = all (\x -> r x x) xs
    pairs      = [(x,y) | x <- xs, y <- xs]
    symmetric  = all (\(x,y) -> r x y == r y x) pairs
    triples    = [(x,y,z) | x <- xs, y <- xs, z <- xs]
    transitive = all (\(x,y,z) -> not (r x y && r y z) || r x z) triples

-- get the equivalence classes of the relation on the set
classes :: (a -> a -> Bool) -> [a] -> [[a]]
classes r xs
  | is_equiv r xs = get_classes xs
  | otherwise     = error "relation is not an equivalence relation on the given set"
  where
    get_classes []     = []
    get_classes (x:ys) = (x : eq) : get_classes rest
      where
        (eq, rest) = partition (r x) ys

-- keeps the present relations and adds reflexive pairs
reflexive_closure :: Eq a => (a -> a -> Bool) -> a -> a -> Bool
reflexive_closure r x y = r x y || x == y
