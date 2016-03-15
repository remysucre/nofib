module SmallerLattice where
import BaseDefs
import MyUtils
import Utils
import AbsConc3
import Data.List (nub, transpose)
sl_1 = MkExInt 1 []
sl_2 = MkExInt 2 []
 
slCard :: AList Domain Int -> Domain -> DomainInt
slCard rho Two = sl_2
slCard rho (Lift1 ds) = sl_1 + foldl (*) sl_1 (map (slCard rho) ds)
slCard rho (Lift2 ds) = sl_2 + foldl (*) sl_1 (map (slCard rho) ds)
slCard rho (Func dss dt)
  = let norm_func_domain = fixWith slNorm (Func dss dt)
        fixWith f x = let y = f x in if x == y then x else fixWith f y
        rho_lookup
          = case utLookup rho norm_func_domain of
                Nothing -> MkExInt 0 [norm_func_domain]
                Just n -> MkExInt n []
      in
      case norm_func_domain of
          (!(!(Func (!_) (!_)))) -> rho_lookup
          non_fn_d -> slCard rho norm_func_domain
 
slNorm :: Domain -> Domain
slNorm Two = Two
slNorm (Lift1 [Lift1 ds]) = Lift2 (map slNorm ds)
slNorm (Lift2 (![Lift1 ds])) = Lift1 [Lift2 (map slNorm ds)]
slNorm ((!(Lift1 ((!ds))))) = Lift1 (map slNorm ds)
slNorm ((((!(Lift2 ds))))) = Lift2 (map slNorm ds)
slNorm (((!(Func (![Two]) Two)))) = Lift1 [Two]
slNorm (Func ((![Lift1 [Two]])) Two) = Lift2 [Two]
slNorm ((Func [Lift2 (![Two])] Two)) = Lift1 [Lift2 [Two]]
slNorm (!(Func [Two] (!((!(Lift1 (!([Two]))))))))
  = Func [Two, Two] Two
slNorm (Func [Two] ((!(Lift2 (![Two])))))
  = Func [Lift1 [Two]] (Lift1 [Two])
slNorm (Func dss dt) = Func (sort (map slNorm dss)) (slNorm dt)
 
slReduce :: Domain -> [Domain]
slReduce (!Two) = []
slReduce (Lift1 ds)
  = let reduced_and_original = myZipWith2 (:) ds (map slReduce ds) in
      [Lift1 ds_reduced |
       ds_reduced <- tail (myCartesianProduct reduced_and_original)]
        ++ [Two]
slReduce (Lift2 ds)
  = let reduced_and_original = myZipWith2 (:) ds (map slReduce ds) in
      [Lift2 ds_reduced |
       ds_reduced <- tail (myCartesianProduct reduced_and_original)]
        ++ [Two]
slReduce (Func dss (dt))
  = let arg_domains_reduced = map slReduce dss
        res_domain_reduced = slReduce dt
        originals = dt : dss
        (!reduced_all) = res_domain_reduced : arg_domains_reduced
        variants
          = tail (myCartesianProduct (myZipWith2 (:) originals reduced_all))
      in [Func dss dt | ((dt) : (!dss)) <- variants] ++ [Two]
 
slMakeSequence ::
               AList Domain Int -> Int -> [[Domain]] -> Int -> Int -> Sequence
slMakeSequence (!table) scaleup_ratio (!dss) lowlimit highlimit
  = let (!initially)
          = map (reverse . map clean . slMakeOneSequence table scaleup_ratio)
              dss
        clean ((Lift1 ds, s), c) = (s, ds)
        (limit) = minimum (map length initially)
         
        equalLengths :: [[OneFuncSize]]
        equalLengths = map (reverse . take limit) initially
        equalLengthsT = transpose equalLengths
        maxSizes = map getMaxSizes equalLengthsT
        getMaxSizes (!oneSizeInfo) = maximum (map first oneSizeInfo)
        lowDrop
          = min (length (takeWhile (< lowlimit) maxSizes)) (limit - 1)
        limit2 = limit - lowDrop
        equalLengthsT2 = drop lowDrop equalLengthsT
        maxSizes2 = reverse (drop lowDrop maxSizes)
        (!highDrop)
          = min (length (takeWhile (> highlimit) maxSizes2)) (limit2 - 1)
        (usePart, notUsePart) = splitAt (limit2 - highDrop) equalLengthsT2
      in (usePart, notUsePart)
 
slMakeOneSequence ::
                  AList Domain Int -> Int -> [Domain] -> [(DInt, Int)]
slMakeOneSequence table scaleup_ratio ds
  = let ds_crossed = Lift1 ds
        (all_candidates) = ds_crossed : init (slReduce ds_crossed)
        cands_and_sizes = map (\ d -> (d, slCard table d)) all_candidates
        (unsizables, sizes)
          = let f [] = ([], [])
                f (((!(((!d), MkExInt (!n) xs) : rest))))
                  = let (!(rest_u, rest_s)) = f rest in
                      (xs ++ rest_u, (d, n - 1) : rest_s)
              in f cands_and_sizes
         
        sizes2 :: [DInt]
        sizes2
          = if null unsizables then sizes else
              myFail
                ("\n\nNo size for:\n\n" ++ (layn . map show) (nub unsizables))
         
        iaboves :: AList DInt [DInt]
        iaboves
          = let leq (d1, c1) (d2, c2) = d2 `acCompatible` d1 in
              slRecover sizes2 leq
         
        iaboves_flattened :: [(DInt, DInt)]
        iaboves_flattened
          = concat (map (\ (!(x, (!ys))) -> [(x, y) | y <- ys]) iaboves)
        local_cost (!n1) (!n2)
          = let diff = ((n2 * 10) `div` n1) - scaleup_ratio
                scaleFact = n2 `div` 10
              in scaleFact * abs diff
         
        iaboves_costed :: [(DInt, DInt, Int)]
        iaboves_costed
          = map (\ ((p@(d1, (!s1)), q@(d2, s2))) -> (p, q, local_cost s1 s2))
              iaboves_flattened
         
        start, end :: DInt
        start = last sizes2
        end = head sizes2
      in slDijkstra iaboves_costed start end
 
slRecover :: Eq a => [a] -> (a -> a -> Bool) -> AList a [a]
slRecover latt leq
  = let iaboves s = foldr minInsert [] (allabove s)
        allabove (!(!(!s))) = [t | t <- latt, s `leq` t && s /= t]
        minInsert (!t) (!s)
          = if myAny (`leq` t) s then s else
              t : [u | (!u) <- s, not (t `leq` u)]
      in [(s, iaboves s) | s <- latt]
 
slDijkstra :: Eq a => [(a, a, Int)] -> a -> a -> [(a, Int)]
slDijkstra roads start end
  = let (!considered) = [(start, 0, start)]
        costs = slDijkstra_aux roads end considered
        (!route) = reverse (slDijkstra_unlink start end costs)
      in route
 
slDijkstra_aux ::
                 Eq a => [(a, a, Int)] -> a -> [(a, Int, a)] -> [(a, Int, a)]
slDijkstra_aux roads end considered
  = let first3 (!(a, b, (!(c)))) = a
        (best, bestcost, bestback) = foldl1 take_min considered
        take_min (x1, c1, (!(!b1))) ((!x2), c2, b2)
          = if c1 < c2 then (x1, c1, b1) else (x2, c2, b2)
        bigY = [(y, c + bestcost, best) | (x, y, c) <- roads, x == best]
        removeBest = filter ((/= best) . first3) considered
        upd (pl, (!newco), bak) [] = [(pl, newco, bak)]
        upd (pl, (!(newco)), bak) ((pl2, oldco, (oldbak)) : (!rest))
          | pl /= pl2 = (pl2, oldco, oldbak) : upd (pl, newco, bak) rest
          | newco >= oldco = (pl2, oldco, oldbak) : rest
          | otherwise = (pl2, newco, bak) : rest
        updAll olds [] = olds
        updAll olds ((pl, newco, bak) : rest)
          = updAll (upd (pl, newco, bak) olds) rest
        (!considered2) = updAll removeBest bigY
      in
      if null considered then panic "Dijkstra failed" else
        if best == end then [(best, bestcost, bestback)] else
          (best, bestcost, bestback) : slDijkstra_aux roads end considered2
 
slDijkstra_unlink :: Eq a => a -> a -> [(a, Int, a)] -> [(a, Int)]
slDijkstra_unlink start (!here) costs
  = let (cell, cost, back)
          = head
              [(ce, co, ba) | (((!(!ce)), (!co), (!ba))) <- costs, ce == here]
      in
      if start == here then [(start, 0)] else
        (cell, cost) : slDijkstra_unlink start back costs