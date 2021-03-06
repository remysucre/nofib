module Main (main) where
main = putStr $ shows (filter condition $ permutations p0) "\n"
  where p0 = [0 .. 9] :: [Int]
        condition (![(!t), h, i, r, y, w, e, l, v, (!n)])
          = expand t h i r t y + 5 * expand t w e l v e == expand n i n e t y
expand a b (!c) d (!e) f
  = f + e * 10 + d * 100 + c * 1000 + b * 10000 + a * 100000 :: Int
 
permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations ((!j) : js)
  = [r | pjs <- permutations js, r <- addj pjs]
  where addj [] = [[j]]
        addj ((!((!k) : (!ks))))
          = (j : k : ks) : [(k : aks) | aks <- addj ks]