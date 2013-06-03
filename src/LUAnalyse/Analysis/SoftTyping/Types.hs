{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module LUAnalyse.Analysis.SoftTyping.Types where

import LUAnalyse.Framework.Lattice
import qualified LUAnalyse.ControlFlow.Flow as Flow

import Prelude hiding (all, any, concat, foldr)
import Utility (outerUnionWith, longZipWith)

import Control.DeepSeq
import Data.Foldable
import Data.Lens.Common
import Data.Lens.Template
import Data.List (intersperse)
import Data.Maybe (catMaybes)

import qualified Data.Map as M
import qualified Data.Set as S

data LuaType
    = Nil
    | Boolean 
    | Number -- range? integral?
    | String -- number-coercible or not? length range?
    | Table TableType  -- array? cardinality?
    | Function FunctionType
    -- | Thread
    -- | UserData

data BoolPowerset = EmptyBool | OnlyTrue | OnlyFalse | TrueAndFalse
    deriving Eq

instance NFData BoolPowerset where
    rnf EmptyBool = ()
    rnf OnlyTrue = ()
    rnf OnlyFalse = ()
    rnf TrueAndFalse = ()

instance Lattice BoolPowerset where
    top = TrueAndFalse
    bottom = EmptyBool

    EmptyBool </ _ = True
    _ </ EmptyBool = False
    TrueAndFalse </ _ = False
    _ </ TrueAndFalse = True
    _ </ _ = False

    join p q
        | p </ q = q
        | q </ p = p
        | otherwise = top
    meet p q
        | p </ q = p
        | q </ p = q
        | otherwise = bottom

data StringLatticePoint = StringBottom | StringConstants (S.Set String) | StringTop
    deriving (Eq, Show)

maximumStringSetSize :: Int
maximumStringSetSize = 1

instance NFData StringLatticePoint where
    rnf StringBottom = ()
    rnf StringTop = ()
    rnf (StringConstants x) = x `deepseq` ()

instance Lattice StringLatticePoint where
    top = StringTop
    bottom = StringBottom
    
    StringBottom </ _ = True
    _ </ StringBottom = False
    StringTop </ _ = False
    _ </ StringTop = True
    StringConstants x </ StringConstants y = x `S.isSubsetOf` y

    join StringTop _ = StringTop
    join _ StringTop = StringTop
    join StringBottom x = x
    join x StringBottom = x
    join (StringConstants x) (StringConstants y)
      = let r = x `S.union` y
        in if S.size r > maximumStringSetSize
           then StringTop
           else StringConstants r

    meet StringTop x = x
    meet x StringTop = x
    meet StringBottom _ = StringBottom
    meet _ StringBottom = StringBottom
    meet (StringConstants x) (StringConstants y)
      = let r = x `S.intersection` y
        in if S.null r
           then StringBottom
           else StringConstants r

-- | Product of lattice points for every Lua type.
data LuaTypeSet = LuaTypeSet
    { _ltsNil       :: Bool
    , _ltsBoolean   :: BoolPowerset
    , _ltsNumber    :: Bool -- TODO small number of constants, naturality, integrality
    , _ltsString    :: StringLatticePoint
    , _ltsTable     :: TableType
    , _ltsFunction  :: FunctionType
    }
    deriving Eq

instance NFData LuaTypeSet where
    rnf (LuaTypeSet x0 x1 x2 x3 x4 x5) = x0 `deepseq` x1 `deepseq` x2 `deepseq` x3 `deepseq` x4 `deepseq` x5 `deepseq` ()

topLuaTypeSet :: LuaTypeSet
topLuaTypeSet = LuaTypeSet top top top top top top

data TableType
    = TableType
        -- | Type sets for constant indices.
        (M.Map ConstantTableKey LuaTypeSet)
        -- | Union of type sets for variable indices (with known types).
        (M.Map VariableTableKey LuaTypeSet)
    | TableTop
    | TableBottom
        -- explicit bottom: value can't be of type 'table'
    deriving Eq

instance NFData TableType where
    rnf TableBottom = ()
    rnf TableTop = ()
    rnf (TableType x0 x1) = x0 `deepseq` x1 `deepseq` ()

emptyTableType :: TableType
emptyTableType = TableType M.empty M.empty

data ConstantTableKey
    = KBoolean  Bool
    | KNumber   Double -- range? integral?
    | KString   String -- number-coercible or not? length range?
    -- | KTable
    -- | KFunction
    deriving (Eq, Ord, Show)

instance NFData ConstantTableKey where
    rnf (KBoolean x0) = x0 `deepseq` ()
    rnf (KNumber x0) = x0 `deepseq` ()
    rnf (KString x0) = x0 `deepseq` ()

data VariableTableKey
    = VBoolean
    | VNumber
    | VString
    | VTable
    | VFunction
    deriving (Eq, Ord, Show)

instance NFData VariableTableKey where
    rnf VBoolean = ()
    rnf VNumber = ()
    rnf VString = ()
    rnf VTable = ()
    rnf VFunction = ()

constantTableKey :: Flow.Constant -> Maybe ConstantTableKey
constantTableKey (Flow.BooleanConst v)  = Just $ KBoolean v
constantTableKey (Flow.NumberConst v)   = Just $ KNumber v
constantTableKey (Flow.StringConst v)   = Just $ KString v
constantTableKey _ = Nothing

variableTableKey :: LuaType -> Maybe VariableTableKey
variableTableKey Boolean = Just VBoolean
variableTableKey Number = Just VNumber
variableTableKey String = Just VString
variableTableKey (Table _) = Just VTable
variableTableKey (Function _) = Just VFunction
variableTableKey _ = Nothing

vkeyOfCkey :: ConstantTableKey -> VariableTableKey
vkeyOfCkey (KBoolean _) = VBoolean
vkeyOfCkey (KNumber _)  = VNumber
vkeyOfCkey (KString _)  = VString

data FunctionType
    = FunctionType [LuaTypeSet] [LuaTypeSet] FunctionEffects
    | FunctionTop
    | FunctionBottom -- explicit bottom: value can't be of type 'table'
    deriving Eq

data FunctionEffects
    = FunctionEffects (M.Map Flow.Variable (LuaTypeSet {- type before -}, LuaTypeSet {- type after -}))
    | EffectTop
    deriving Eq

instance NFData FunctionType where
    rnf FunctionTop = ()
    rnf FunctionBottom = ()
    rnf (FunctionType x0 x1 x2) = x0 `deepseq` x1 `deepseq` x2 `deepseq` ()

instance NFData FunctionEffects where
    rnf EffectTop = ()
    rnf (FunctionEffects x0) = x0 `deepseq` ()

-- | Indicates whether the first type is a 'subtype' of the second type. For table and function 
--   types, this is equivalent to (</). For other types, a `subType` b holds only if the types are
--   equal. This means that, for example, a number is not a subtype of a string, even though 
--   numbers can be coerced to strings. 
--   TODO: do we want this? Or do we want to specify some coercion rules here? Or do we want to 
--         take care of coercion in the definition of the FunctionType lattice?
subType :: LuaType -> LuaType -> Bool
subType a b =
 case (a,b) of
  (Nil        , Nil        ) -> True
  (Boolean    , Boolean    ) -> True
  (Number     , Number     ) -> True
  (String     , String     ) -> True
  (Table ta   , Table tb   ) -> ta </ tb
  (Function fa, Function fb) -> fa </ fb
  (_,           _          ) -> False

sameType :: LuaType -> LuaType -> Bool
sameType a b = subType a b && subType b a

-- Only implemented to facilitate Set.
instance Eq LuaType where
    (==) = sameType

-- Only implemented to facilitate Set.
instance Ord LuaType where
    Nil <= Nil = True
    _ <= Nil = False
    Boolean <= Boolean = True
    _ <= Boolean = False
    Number <= Number = True
    _ <= Number = False
    String <= String = True
    _ <= String = False
    Table x <= Table y = x </ y
    _ <= Table _ = False
    Function x <= Function y = x </ y
    _ <= Function _ = False

-- | Table lattice. 
--   NOTE: assumes tables are sorted on keys. Make sure to maintain this invariant.
instance Lattice TableType where
 TableBottom </ _ = True
 _ </ TableBottom = False
 _ </ TableTop = True
 TableTop </ _ = False
 TableType llc llv </ TableType rrc rrv
    = M.isSubmapOfBy (</) llc rrc
    && M.isSubmapOfBy (</) llv rrv
 join TableBottom x = x
 join x TableBottom = x
 join TableTop _ = TableTop
 join _ TableTop = TableTop
 join (TableType llc llv) (TableType rrc rrv)
    = TableType (M.unionWith join llc rrc) (M.unionWith join llv rrv)
 meet TableBottom _ = TableBottom
 meet _ TableBottom = TableBottom
 meet TableTop x = x
 meet x TableTop = x
 meet (TableType llc llv) (TableType rrc rrv)
    = TableType (M.intersectionWith meet llc rrc) (M.intersectionWith meet llv rrv)
 bottom = TableBottom
 top = TableTop

unpackedTableTop :: TableType
unpackedTableTop
  = let make vty mp = M.insert vty top mp
        allVTypes = [VBoolean, VNumber, VString, VTable, VFunction]
    in force $ TableType M.empty (foldr make M.empty allVTypes)

-- | Function lattice.
instance Lattice FunctionType where
 FunctionBottom </ _ = True
 _ </ FunctionBottom = False
 FunctionTop </ _ = False
 _ </ FunctionTop = True

 FunctionType pIn pOut pEffects </ FunctionType qIn qOut qEffects
    | length pIn <= length qIn, length pOut <= length qOut
    , all id (zipWith (</) pIn  pOut)
    , all id (zipWith (</) pOut qOut)
    , pEffects </ qEffects
    = True
    | otherwise = False

 join FunctionTop _ = FunctionTop
 join _ FunctionTop = FunctionTop
 join FunctionBottom x = x
 join x FunctionBottom = x
 FunctionType pIn pOut pEffects `join` FunctionType qIn qOut qEffects
    = FunctionType
        (longZipWith join pIn qIn)
        (longZipWith join pOut qOut)
        (pEffects `join` qEffects)

 meet FunctionTop x = x
 meet x FunctionTop = x
 meet FunctionBottom _ = FunctionBottom
 meet _ FunctionBottom = FunctionBottom
 FunctionType pIn pOut pEffects `meet` FunctionType qIn qOut qEffects
    = FunctionType
        (longZipWith meet pIn qIn)
        (longZipWith meet pOut qOut)
        (pEffects `meet` qEffects)

 bottom = FunctionBottom
 top = FunctionTop

-- | Function effects lattice, under "is a sub-effect of".
instance Lattice FunctionEffects where
 _ </ EffectTop = True
 EffectTop </ _ = False

 FunctionEffects ps </ FunctionEffects qs = all id $ outerUnionWith cmpEffects ps qs
  where
    cmpEffects :: Maybe (LuaTypeSet, LuaTypeSet) -> Maybe (LuaTypeSet, LuaTypeSet) -> Bool
    cmpEffects Nothing _ = True
    cmpEffects _ Nothing = False
    cmpEffects (Just (pBefore, pAfter)) (Just (qBefore, qAfter))
        | qBefore </ pBefore    -- contravariant
        , pAfter </ qAfter      -- covariant
        = True
        | otherwise
        = False

 join _ EffectTop = EffectTop
 join EffectTop _ = EffectTop
 join (FunctionEffects ps) (FunctionEffects qs) = FunctionEffects $ M.unionWith effJoin ps qs
  where
    effJoin :: (LuaTypeSet, LuaTypeSet) -> (LuaTypeSet, LuaTypeSet) -> (LuaTypeSet, LuaTypeSet)
    effJoin (pBefore, pAfter) (qBefore, qAfter) = (pBefore `meet` qBefore, pAfter `join` qAfter)
 
 meet x EffectTop = x
 meet EffectTop x = x
 meet (FunctionEffects ps) (FunctionEffects qs) = FunctionEffects $ M.unionWith effMeet ps qs
  where
    effMeet :: (LuaTypeSet, LuaTypeSet) -> (LuaTypeSet, LuaTypeSet) -> (LuaTypeSet, LuaTypeSet)
    effMeet (pBefore, pAfter) (qBefore, qAfter) = (pBefore `join` qBefore, pAfter `meet` qAfter)

 bottom = FunctionEffects M.empty
 top = EffectTop

-- | The Lua type set lattice.
instance Lattice LuaTypeSet where
 LuaTypeSet x0 x1 x2 x3 x4 x5 </ LuaTypeSet y0 y1 y2 y3 y4 y5
    = all id
        [ {-# SCC x0 #-} x0 </ y0
        , {-# SCC x1 #-} x1 </ y1
        , {-# SCC x2 #-} x2 </ y2
        , {-# SCC x3 #-} x3 </ y3
        , {-# SCC x4 #-} x4 </ y4
        , {-# SCC x5 #-} x5 </ y5
        ]
 join (LuaTypeSet x0 x1 x2 x3 x4 x5) (LuaTypeSet y0 y1 y2 y3 y4 y5)
    = LuaTypeSet (x0 `join` y0) (x1 `join` y1) (x2 `join` y2)
                 (x3 `join` y3) (x4 `join` y4) (x5 `join` y5) 
 meet (LuaTypeSet x0 x1 x2 x3 x4 x5) (LuaTypeSet y0 y1 y2 y3 y4 y5)
    = LuaTypeSet (x0 `meet` y0) (x1 `meet` y1) (x2 `meet` y2)
                 (x3 `meet` y3) (x4 `meet` y4) (x5 `meet` y5) 
 bottom = LuaTypeSet bottom bottom bottom bottom bottom bottom
 top = LuaTypeSet top top top top top top

$( makeLens ''LuaTypeSet )

instance Show LuaTypeSet where
    show l = (\x -> "["++x++"]") . concat . intersperse "," . catMaybes . map ($ l) $
        [ determine ltsNil      "nil"
        , determine ltsBoolean  "boolean"
        , determine ltsNumber   "number"
        , determine2 ltsString
        , determine2 ltsTable
        , determine2 ltsFunction
        ]
      where
        determine2 :: (Lattice b, Show b) => Lens a b -> a -> Maybe String
        determine2 f ll
            | ll ^. f </ bottom = Nothing
            | otherwise = Just . show $ ll ^. f

        determine :: Lattice b => Lens a b -> r -> a -> Maybe r
        determine f desc ll
            | ll ^. f </ bottom = Nothing
            | otherwise = Just desc

deriving instance Show FunctionEffects
deriving instance Show FunctionType
deriving instance Show TableType

singleType :: LuaType -> LuaTypeSet
singleType Nil          = ltsNil      ^= top $ bottom
singleType Boolean      = ltsBoolean  ^= top $ bottom
singleType Number       = ltsNumber   ^= top $ bottom
singleType String       = ltsString   ^= top $ bottom
singleType (Table t)    = ltsTable    ^= t   $ bottom
singleType (Function t) = ltsFunction ^= t   $ bottom

tableMemberType :: LuaTypeSet -> LuaTypeSet -> LuaTypeSet
tableMemberType tab idx
  = let vkeys = variableTableKeysOf idx
        ckeys = constantTableKeysOf idx
        tab' = tab ^. ltsTable
        vtypes, ctypes :: LuaTypeSet
        vtypes = foldl' (\acc k -> acc `join` tableVMemberType tab' k) bottom vkeys
        ctypes = foldl' (\acc k -> acc `join` tableKMemberType tab' k) bottom ckeys
        
        mightBeNil = if S.null vkeys then bottom else singleType Nil
    in force $ (vtypes `join` ctypes) `join` mightBeNil
  where
    tableKMemberType :: TableType -> ConstantTableKey -> LuaTypeSet
    tableKMemberType TableBottom _ = bottom
    tableKMemberType TableTop _ = top
    tableKMemberType tt@(TableType cmap _) kidx
      = let vtype = tableVMemberType tt (vkeyOfCkey kidx)
        in M.findWithDefault vtype kidx cmap

    tableVMemberType :: TableType -> VariableTableKey -> LuaTypeSet
    tableVMemberType TableBottom _ = bottom
    tableVMemberType TableTop _ = top
    tableVMemberType (TableType cmap vmap) vidx
      = let cmap' = M.mapKeysWith join vkeyOfCkey cmap
            bothMap = M.unionWith join vmap cmap'
        in M.findWithDefault (singleType Nil) vidx bothMap

constantTableKeysOf :: LuaTypeSet -> S.Set ConstantTableKey
constantTableKeysOf lt
  = let boolKeys = S.fromList $ case lt ^. ltsBoolean of
            EmptyBool -> []
            OnlyTrue -> [KBoolean True]
            OnlyFalse -> [KBoolean False]
            TrueAndFalse -> [KBoolean False, KBoolean True]
        stringKeys = case lt ^. ltsString of
            StringConstants x -> S.map KString x
            _ -> S.empty
    in boolKeys `S.union` stringKeys

advanceTableType :: LuaTypeSet -> LuaTypeSet -> LuaTypeSet -> LuaTypeSet
advanceTableType tab idx val
  = let cfuns = fmap advanceC . S.toList . constantTableKeysOf $ idx
        vfuns = fmap advanceV . S.toList . variableTableKeysOf $ idx

        advance = foldr (.) id cfuns . foldr (.) id vfuns

        advanceC :: ConstantTableKey -> TableType -> TableType
        advanceC k TableTop = advanceC k unpackedTableTop
        advanceC _ TableBottom = TableBottom
        advanceC k (TableType cmap vmap)
          = let cmap' = M.insert k val cmap
            in TableType cmap' vmap

        advanceV :: VariableTableKey -> TableType -> TableType
        advanceV k TableTop = advanceV k unpackedTableTop
        advanceV _ TableBottom = TableBottom
        advanceV k (TableType cmap vmap)
          = let vmap' = M.insert k val vmap
            in TableType cmap vmap'

    in force $ ltsTable ^%= advance $ tab

-- TODO Restructure this, so that variable keys are not build when we only have
--      constant keys in the type set.
variableTableKeysOf :: LuaTypeSet -> S.Set VariableTableKey
variableTableKeysOf l = S.fromList . catMaybes . map ($ l) $
    [ determine ltsNumber   VNumber
    , determineStrings
    , determine ltsTable    VTable
    , determine ltsFunction VFunction
    ]
  where
    determine :: Lattice b => Lens a b -> r -> a -> Maybe r
    determine f desc ll
        | ll ^. f </ bottom = Nothing
        | otherwise = Just desc

    determineStrings :: LuaTypeSet -> Maybe VariableTableKey
    determineStrings ll = case ll ^. ltsString of
        StringTop -> Just VString
        _ -> Nothing

constantBooleanType :: Bool -> LuaTypeSet
constantBooleanType v
  = let vv = if v then OnlyTrue else OnlyFalse
    in ltsBoolean ^= vv $ bottom

constantStringType :: String -> LuaTypeSet
constantStringType x = ltsString ^= StringConstants (S.singleton x) $ bottom
