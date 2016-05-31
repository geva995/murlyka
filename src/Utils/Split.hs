module Split(
  splitBy
) where

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e xs = reverse $ splitBy' e xs []

splitBy' :: Eq a => a -> [a] -> [[a]] -> [[a]]
splitBy' _ [] yys = yys
splitBy' e xs xxs = 
  if null z then y:xxs else splitBy' e (tail z) (y:xxs)
  where
    (y, z) = span (/= e) xs