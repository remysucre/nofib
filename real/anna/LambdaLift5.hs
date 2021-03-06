module LambdaLift5 where
import BaseDefs
import Utils
import MyUtils
import Dependancy
import Data.List (nub)
 
llMergeLams :: CExprP Naam -> CExprP Naam
llMergeLams ((!(EVar (v)))) = EVar v
llMergeLams (ENum n) = ENum n
llMergeLams (EConstr c) = EConstr c
llMergeLams (EAp e1 e2) = EAp (llMergeLams e1) (llMergeLams e2)
llMergeLams (ECase sw alts)
  = ECase (llMergeLams sw)
      [(n, (ps, llMergeLams rhs)) | (n, (ps, rhs)) <- alts]
llMergeLams (((!((!(ELam vs1 (ELam (!vs2) (!e))))))))
  = llMergeLams (ELam (vs1 ++ vs2) e)
llMergeLams (ELam vs e) = ELam vs (llMergeLams e)
llMergeLams (ELet rf defs (!e))
  = ELet rf (map2nd llMergeLams defs) (llMergeLams e)
 
llName :: CExprP Naam -> CExprP Naam
llName (((!(EVar (!v))))) = EVar v
llName (!((!(ENum n)))) = ENum n
llName ((!(EConstr (c)))) = EConstr c
llName ((!(EAp (!(e1)) e2))) = EAp (llName e1) (llName e2)
llName ((ELam vs (!e)))
  = ELet False [("_sc", ELam vs (llName e))] (EVar "_sc")
llName (ECase sw alts)
  = ECase (llName sw)
      [(n, (ps, llName rhs)) | (n, (ps, rhs)) <- alts]
llName (((!(ELet (!rf) defs e))))
  = ELet rf (map fix defs) (llName e)
  where fix ((!n), ELam (vs) e) = (n, ELam vs (llName e))
        fix (n, (!non_lam_e)) = (n, llName non_lam_e)
 
llUnique ::
         NameSupply ->
           AList Naam Naam -> CExprP Naam -> (NameSupply, CExprP Naam)
llUnique ns (!dict) (ENum n) = (ns, ENum n)
llUnique ns dict (((!(EConstr c)))) = (ns, EConstr c)
llUnique ns dict (EAp e1 e2)
  = let (ns_new1, e1_new) = llUnique ns dict e1
        (ns_new2, (!(!e2_new))) = llUnique ns_new1 dict e2
      in (ns_new2, EAp e1_new e2_new)
llUnique ns dict (ECase (!sw) alts)
  = let ((!(ns_new1)), sw_new) = llUnique ns dict sw
        (ns_new2, (alts_new)) = mapAccuml fixAlt ns_new1 alts
        fixAlt ns ((!n), (ps, rhs))
          = let (new_ns, new_params) = utGetNames ns (llCheckUnique ps)
                (!new_dict) = zip ps new_params ++ dict
                ((!(final_ns)), final_rhs) = llUnique new_ns new_dict rhs
              in (final_ns, (n, (new_params, final_rhs)))
      in (ns_new2, ECase sw_new alts_new)
llUnique ns (!dict) (!(EVar v))
  = case utLookup dict v of
        Just v2 -> (ns, EVar v2)
        Nothing -> myFail ("No such variable \"" ++ v ++ "\"")
llUnique ns dict (!(ELam vs e))
  = let ((!(new_ns)), new_params) = utGetNames ns (llCheckUnique vs)
        new_dict = zip vs new_params ++ dict
        (final_ns, (!final_e)) = llUnique new_ns new_dict e
      in (final_ns, ELam new_params final_e)
llUnique ns dict (ELet rf defs (!e))
  = let ((!(new_ns2)), (!new_defs)) = mapAccuml fixDef new_ns1 defs
        (final_ns, new_e) = llUnique new_ns2 dictAug e
        hereNames = llCheckUnique (map first defs)
        (new_ns1, hereBinds) = utGetNames ns (llCheckUnique hereNames)
        dictAug = zip hereNames (map ('_' :) hereBinds) ++ dict
        dictForDefs = if rf then dictAug else dict
        fixDef (!ns_loc) ((!n), rhs)
          = let (ns_loc_final, (!(!rhs_final)))
                  = llUnique ns_loc dictForDefs rhs
              in (ns_loc_final, (utSureLookup dictAug "llUnique" n, rhs_final))
      in (final_ns, ELet rf new_defs new_e)
 
llCheckUnique :: [Naam] -> [Naam]
llCheckUnique names
  = let getdups [] = []
        getdups [x] = []
        getdups (x : (y : xys))
          | x == y = x : getdups (dropWhile (== x) xys)
          | otherwise = getdups (y : xys)
        dups = getdups (sort names)
      in
      if null dups then names else
        myFail
          ("Duplicate identifiers in the same scope:\n\t" ++ show dups)
 
llFreeVars :: CExprP Naam -> AnnExpr Naam (Set Naam)
llFreeVars ((!(ENum (k)))) = (utSetEmpty, ANum k)
llFreeVars (!(EVar (!v))) = (utSetSingleton v, AVar v)
llFreeVars ((!(EConstr (!(!c))))) = (utSetEmpty, AConstr c)
llFreeVars ((!(EAp e1 (e2))))
  = let a_e1@((!(f_e1, _))) = llFreeVars e1
        a_e2@((f_e2), (!_)) = llFreeVars e2
      in (utSetUnion f_e1 f_e2, AAp a_e1 a_e2)
llFreeVars (((ELam args body)))
  = let (!body_a@(body_f, _)) = llFreeVars body in
      (utSetSubtraction body_f (utSetFromList args), ALam args body_a)
llFreeVars ((!(ELet isRec defns ((!body)))))
  = let (binders, values) = unzip2 defns
        binderSet = utSetFromList binders
        values' = map llFreeVars values
        defns' = zip binders values'
        (!freeInValues) = utSetUnionList [free | (free, _) <- values']
        defnsFree
          | isRec = utSetSubtraction freeInValues binderSet
          | otherwise = freeInValues
        body' = llFreeVars body
        bodyFree = utSetSubtraction (first body') binderSet
      in (utSetUnion defnsFree bodyFree, ALet isRec defns' body')
llFreeVars ((((!(ECase e alts)))))
  = let (eFree, _) = e'
        (!e') = llFreeVars e
        alts' = [(con, (args, llFreeVars e)) | (con, (args, e)) <- alts]
        free = utSetUnionList (map f alts')
        f (con, (args, (!((!free), exp))))
          = utSetSubtraction free (utSetFromList args)
      in (utSetUnion eFree free, ACase e' alts')
 
llEqns :: AnnExpr Naam (Set Naam) -> [Eqn]
llEqns (!(_, AVar _)) = []
llEqns (_, (!(ANum _))) = []
llEqns (((!_), (!(AConstr _)))) = []
llEqns (_, AAp a1 a2) = llEqns a1 ++ llEqns a2
llEqns ((!((!_))), (!(ALam _ (e)))) = llEqns e
llEqns ((!_), ACase sw alts)
  = llEqns sw ++ concat (map (llEqns . second . second) alts)
llEqns (_, (!(ALet rf (!defs) body)))
  = let binders = [n | (n, rhs) <- defs]
        eqnsHere
          = [case llSplitSet fv of
                 (facc, vacc) -> EqnNVC n vacc facc
             | ((!n), (!(fv, ((!rhsa))))) <- defs]
        innerEqns = concat [llEqns rhs | (n, rhs@(fv, rhsa)) <- defs]
        nextEqns = llEqns body
      in eqnsHere ++ innerEqns ++ nextEqns
 
llAddParams ::
            AList Naam (Set Naam) -> AnnExpr Naam (Set Naam) -> CExprP Naam
llAddParams (!env) (_, ANum (!n)) = ENum n
llAddParams env (_, AConstr (c)) = EConstr c
llAddParams env (!(_, AVar v)) = mkApChain vParams
  where (!vParams) = utLookup env v
        mkApChain ((!(Just (vs))))
          = foldl EAp (EVar v) (map EVar (utSetToList vs))
        mkApChain Nothing = EVar v
llAddParams env (_, AAp e1 e2)
  = EAp (llAddParams env e1) (llAddParams env e2)
llAddParams env ((_, ALam args body))
  = ELam args (llAddParams env body)
llAddParams env (_, ACase sw alts)
  = ECase (llAddParams env sw) (map f alts)
  where f (!(naam, (!((params, (!body))))))
          = (naam, (params, llAddParams env body))
llAddParams env ((!_), (!(ALet rFlag (defs) ((!body)))))
  = ELet rFlag (map fixDef defs) fixedBody
  where (!fixedBody) = llAddParams env body
        fixDef (n, (df, (!(ALam vs rhs))))
          = let (new_params)
                  = utSetToList (utSureLookup env "llAddParams1" n)
              in (n, ELam (new_params ++ vs) (llAddParams env rhs))
        fixDef (n, ((df), (!non_lambda_rhs)))
          = let new_params = utSetToList (utSureLookup env "llAddParams2" n)
              in (n, ELam new_params (llAddParams env (df, non_lambda_rhs)))
 
llFlatten :: CExprP Naam -> (AList Naam (CExprP Naam), CExprP Naam)
llFlatten ((!(EVar v))) = ([], EVar v)
llFlatten (ENum n) = ([], ENum n)
llFlatten (((!(EConstr c)))) = ([], EConstr c)
llFlatten (((!(EAp e1 (!e2))))) = (e1b ++ e2b, EAp e1f e2f)
  where (e1b, e1f) = llFlatten e1
        ((!e2b), e2f) = llFlatten e2
llFlatten (!((!(ELam (!ps) (e1))))) = (e1b, ELam ps e1f)
  where ((e1b, (!e1f))) = llFlatten e1
llFlatten ((!(ECase sw (alts))))
  = (swb ++ concat altsb, ECase swf altsf)
  where (swb, swf) = llFlatten sw
        altsFixed = map fixAlt alts
        fixAlt (name, (pars, rhs)) = (name, (pars, llFlatten rhs))
        altsf = map getAltsf altsFixed
        getAltsf (name, (pars, ((rhsb), rhsf))) = (name, (pars, rhsf))
        (!altsb) = map getAltsb altsFixed
        getAltsb (name, (pars, (rhsb, rhsf))) = rhsb
llFlatten (!((ELet rf (!dl) rhs))) = (dlFlattened ++ rhsb, rhsf)
  where (rhsb, rhsf) = llFlatten rhs
        dlFixed = map fixDef dl
        fixDef (name, rhs) = (name, llFlatten rhs)
        dlFlattened = dsHere ++ concat dsInside
        (!dsHere) = map here dlFixed
        here (name, (!(!(inDs, (!frhs))))) = (name, frhs)
        dsInside = map inside dlFixed
        inside (name, (!(inDs, frhs))) = inDs
 
llPretty ::
         (AList Naam (CExprP Naam), AList Naam [Naam]) ->
           (AList Naam (CExprP Naam), AList Naam [Naam])
llPretty (scDefs, scFrees)
  = let scDefNames = map first scDefs
        scTable = getContentious scDefNames
        ((!(scDefs1, (!scFrees1))))
          = ([(prettyScName scTable n,
               llMapCoreTree (prettyScName scTable) cexp)
              | (n, cexp) <- scDefs],
             map1st (prettyScName scTable) scFrees)
        lamTableTable = map makeLamTable scDefs1
        makeLamTable (n, ELam vs _) = getContentious vs
        makeLamTable (n, (!non_lam_s)) = []
        scFrees2 = myZipWith2 fixParams scFrees1 lamTableTable
        fixParams (!(n, ps)) contentious
          = (n, map (prettyVarName contentious) ps)
        (!scDefs2) = myZipWith2 fixDef scDefs1 lamTableTable
        fixDef ((!(n)), cexp) contentious
          = (n, llMapCoreTree (prettyVarName contentious) cexp)
        getContentious (!names)
          = let sortedNames = sort names
                gc (!(![])) = []
                gc [x] = []
                gc (x : (y : xys))
                  | rootName x == rootName y = x : y : gc (y : xys)
                  | otherwise = gc (y : xys)
                contentions = nub (gc sortedNames)
              in contentions
        prettyScName contentions n
          | head n == '_' && n `notElem` contentions = rootName n
          | otherwise = n
        prettyVarName contentions n
          | head n /= '_' && n `notElem` contentions = rootName n
          | otherwise = n
        rootName = takeWhile (/= ')')
      in (scDefs2, scFrees2)
 
llSplitSet :: Set Naam -> (Set Naam, Set Naam)
llSplitSet (!list)
  = let split ((!facc), vacc) n
          = if head n == '_' then (n : facc, vacc) else (facc, n : vacc)
      in
      case foldl split ([], []) (utSetToList list) of
          (!(fs, vs)) -> (utSetFromList fs, utSetFromList vs)
 
llZapBuiltins :: [Naam] -> Eqn -> Eqn
llZapBuiltins builtins (((EqnNVC n v (!c))))
  = EqnNVC n v
      (utSetFromList (filter (`notElem` builtins) (utSetToList c)))
 
llSolveIteratively :: [Eqn] -> AList Naam (Set Naam)
llSolveIteratively eqns = loop eqns initSets
  where initSets = [(n, utSetEmpty) | EqnNVC n v c <- eqns]
        loop (!eqns) (!aSet)
          = let newSet = map (sub_eqn aSet) eqns in
              if newSet == aSet then newSet else loop eqns newSet
        sub_eqn subst ((!(EqnNVC n v (!c))))
          = let allVars = utSetToList v ++ utSetToList c
                allSub = utSetUnionList (map sub allVars)
                sub var = utLookupDef subst var (utSetSingleton var)
              in
              case llSplitSet allSub of
                  (facc, vacc) -> (n, vacc)
 
llMapCoreTree :: (Naam -> Naam) -> CExprP Naam -> CExprP Naam
llMapCoreTree f (EVar (!v)) = EVar (f v)
llMapCoreTree f (ENum (!n)) = ENum n
llMapCoreTree (f) (!((!(EConstr ((!c)))))) = EConstr c
llMapCoreTree f ((ELam vs (!e)))
  = ELam (map f vs) (llMapCoreTree f e)
llMapCoreTree f (EAp e1 e2)
  = EAp (llMapCoreTree f e1) (llMapCoreTree f e2)
llMapCoreTree f (ELet rf (!dl) e)
  = ELet rf [(f n, llMapCoreTree f rhs) | (!(n, (!rhs))) <- dl]
      (llMapCoreTree f e)
llMapCoreTree f (ECase sw alts)
  = ECase (llMapCoreTree f sw)
      [(cn, (map f ps, llMapCoreTree f rhs)) | (cn, (ps, rhs)) <- alts]
 
llMain ::
       [Naam] -> CExprP Naam -> Bool -> (CExprP Naam, AList Naam [Naam])
llMain builtInNames expr doPretty
  = let fvAnnoTree
          = (llFreeVars .
               second .
                 llUnique 0 initialRenamer . llName . llMergeLams . deDependancy)
              expr
        builtInFns = filter ((== '_') . head) builtInNames
        initFreeEnv = [(n, utSetEmpty) | n <- builtInNames]
        initialRenamer = map (\ n -> (tail n, n)) builtInFns
        eqns = llEqns fvAnnoTree
        eqns_with_builtins_zapped = map (llZapBuiltins builtInFns) eqns
        eqns_solved = llSolveIteratively eqns_with_builtins_zapped
        (!(scDefs, mainE)) = llFlatten (llAddParams eqns_solved fvAnnoTree)
        (prettyScDefs, prettyNewParams)
          = if doPretty then llPretty (scDefs, scParams) else
              (scDefs, scParams)
        scParams = map2nd utSetToList eqns_solved
        exprReconstituted = ELet True prettyScDefs mainE
        exprDepended = deDependancy exprReconstituted
      in (exprDepended, prettyNewParams)