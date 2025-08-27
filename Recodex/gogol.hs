googolMod :: Integer -> Integer -> Integer
googolMod n k = modexp 10 (10^n) k

modexp :: Integer -> Integer -> Integer -> Integer
modexp _ 0 m = 1 `mod` m
modexp a e m
  | even e    = let reduced = modexp a (e `div` 2) m in (reduced * reduced) `mod` m
  | otherwise = let reduced = modexp a (e - 1) m in (a * reduced) `mod` m
