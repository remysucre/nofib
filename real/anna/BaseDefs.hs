module BaseDefs where
 
type AList a b = [(a, b)]
 
type DefnGroup a = [(Bool, [a])]
 
type ST a b = b -> (a, b)
 
data ATree a b = ALeaf
               | ABranch (ATree a b) a b (ATree a b) Int
               deriving Eq
 
data Reply a b = Ok a
               | Fail b
               deriving Eq
 
type NameSupply = Int
 
type Oseq = Int -> Int -> [Char]
 
type Iseq = Oseq -> Oseq
 
data Set a = MkSet [a]
           deriving Eq
 
type Bag a = [a]
 
data Flag = Typecheck
          | Simp
          | NoCaseOpt
          | ShowHExpr
          | NoPretty
          | NoFormat
          | NoBaraki
          | SimpleInv
          | PolyLim Int
          | MonoLim Int
          | ForceAll
          | DryRun
          | LowerLim Int
          | UpperLim Int
          | ScaleUp Int
          deriving Eq
bdDefaultSettings
  = [PolyLim 10000, MonoLim 10000, LowerLim 0, UpperLim 1000000,
     ScaleUp 20]
bdDryRunSettings
  = [NoBaraki, LowerLim 0, UpperLim 0, PolyLim 1, MonoLim 1,
     ScaleUp 20]
 
data SAInfo = SAResult String Domain Route
            | SASearch ACMode String Int Int
            | SASizes String [OneFuncSize] [OneFuncSize]
            | SAHExpr String (HExpr Naam)
            | SASL [Route] [Route]
            | SAGiveUp [String]
 
data ExceptionInt a = MkExInt Int [a]
                    deriving (Eq, Ord, Show)
 
instance (Show a, Ord a) => Num (ExceptionInt a) where
        (MkExInt i1 xs1) + (MkExInt i2 xs2)
          = MkExInt (i1 + i2) (xs1 ++ xs2)
        (MkExInt i1 xs1) * (MkExInt i2 xs2)
          = MkExInt (i1 * i2) (xs1 ++ xs2)
 
type DomainInt = ExceptionInt Domain
 
type DInt = (Domain, Int)
 
type OneFuncSize = (Int, [Domain])
 
type Sequence = ([[OneFuncSize]], [[OneFuncSize]])
 
type Naam = [Char]
 
type Alter = AlterP Naam
 
type AlterP a = ([a], CExprP a)
 
type ScValue = ScValueP Naam
 
type ScValueP a = ([a], CExprP a)
 
type CoreProgram = CoreProgramP Naam
 
type CoreProgramP a = ([TypeDef], [(Naam, ScValueP a)])
 
type AtomicProgram = ([TypeDef], CExpr)
 
type TypeDef = (Naam, [Naam], [ConstrAlt])
 
type ConstrAlt = (Naam, [TDefExpr])
 
data TDefExpr = TDefVar Naam
              | TDefCons Naam [TDefExpr]
              deriving Eq
 
type CExpr = CExprP Naam
 
data CExprP a = EVar Naam
              | ENum Int
              | EConstr Naam
              | EAp (CExprP a) (CExprP a)
              | ELet Bool [(a, CExprP a)] (CExprP a)
              | ECase (CExprP a) [(Naam, AlterP a)]
              | ELam [a] (CExprP a)
              deriving Eq
 
type AnnExpr a b = (b, AnnExpr' a b)
 
data AnnExpr' a b = AVar Naam
                  | ANum Int
                  | AConstr Naam
                  | AAp (AnnExpr a b) (AnnExpr a b)
                  | ALet Bool [AnnDefn a b] (AnnExpr a b)
                  | ACase (AnnExpr a b) [AnnAlt a b]
                  | ALam [a] (AnnExpr a b)
                  deriving Eq
 
type AnnDefn a b = (a, AnnExpr a b)
 
type AnnAlt a b = (Naam, ([a], (AnnExpr a b)))
 
type AnnProgram a b = [(Naam, [a], AnnExpr a b)]
 
data Eqn = EqnNVC Naam (Set Naam) (Set Naam)
         deriving Eq
 
type TVName = ([Int], [Int])
 
type Message = [Char]
 
data TExpr = TVar TVName
           | TArr TExpr TExpr
           | TCons [Char] [TExpr]
           deriving Eq
 
data TypeScheme = Scheme [TVName] TExpr
                deriving Eq
 
type Subst = AList TVName TExpr
 
type TcTypeEnv = AList Naam TypeScheme
 
type TypeEnv = AList Naam TExpr
 
type TypeNameSupply = TVName
 
type TypeInfo = (Subst, TExpr, AnnExpr Naam TExpr)
 
type TypeDependancy = DefnGroup Naam
 
type Point = (Domain, Route)
 
data FrontierElem = MkFrel [Route]
                  deriving (Eq, Ord, Show)
 
data Frontier = Min1Max0 Int [FrontierElem] [FrontierElem]
              deriving (Eq, Ord, Show)
 
data Domain = Two
            | Lift1 [Domain]
            | Lift2 [Domain]
            | Func [Domain] Domain
            deriving (Eq, Ord, Show, Read)
 
data Route = Zero
           | One
           | Stop1
           | Up1 [Route]
           | Stop2
           | Up2
           | UpUp2 [Route]
           | Rep Rep
           deriving (Eq, Ord, Show)
 
data Rep = RepTwo Frontier
         | Rep1 Frontier [Rep]
         | Rep2 Frontier Frontier [Rep]
         deriving (Eq, Ord, Show)
 
data DExpr = DXTwo
           | DXLift1 [DExpr]
           | DXLift2 [DExpr]
           | DXFunc [DExpr] DExpr
           | DXVar String
           deriving Eq
 
type RSubst = AList String Route
 
type DSubst = AList String Domain
 
type DRRSubst = AList String (Domain, Route, Route)
 
type DExprEnv = AList String DExpr
 
data ConstrElem = ConstrRec
                | ConstrVar Int
                deriving (Eq, Ord, Show)
 
data ACMode = Safe
            | Live
            deriving Eq
 
type MemoList = AList [Route] Route
 
data AppInfo = A2
             | ALo1
             | AHi1 Int Int Domain
             | ALo2
             | AMid2
             | AHi2 Int Int Domain
             deriving Eq
 
data HExpr a = HApp (HExpr a) (HExpr a)
             | HVAp (HExpr a) [HExpr a]
             | HLam [a] (HExpr a)
             | HVar a
             | HMeet [HExpr a]
             | HPoint Route
             | HTable (AList Route (HExpr a))
             deriving (Eq, Show)
 
type PrPoint = [Int]
 
type PrDomain = [PrPoint]
 
type Token = (Int, [Char])
 
data PResult a = PFail [Token]
               | POk a [Token]
               deriving Eq
 
type Parser a = [Token] -> PResult a
 
data PartialExpr = NoOp
                 | FoundOp Naam CExpr
                 deriving Eq
 
type StaticComponent =
     (DExprEnv, DSubst, AList Naam [ConstrElem], AList Naam [Naam],
      [Flag], (Int, Int, Int, Int, Int), AList Domain Int)