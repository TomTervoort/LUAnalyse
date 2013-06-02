{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.Analysis.SoftTyping.Types where

import LUAnalyse.Framework.Lattice

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

type LuaNumber = Double
type LuaString = String
type LuaVariable = String

data LuaType
    = Nil
    | Boolean 
    | Number -- range? integral?
    | String -- number-coercible or not? length range?
    | Table TableType  -- array? cardinality?
    | Function FunctionType
    -- | Thread
    -- | UserData
    

data LuaTypeSet
    = LuaTypeSet [LuaType]
    

topLuaTypeSet :: LuaTypeSet
topLuaTypeSet = LuaTypeSet
    [ Nil
    , Boolean 
    , Number
    , String
    , Table TableTop
    , Function FunctionTop
    ]

data TableType
    = TableCons (TableKey, LuaTypeSet) TableType -- NOTE: should be sorted on TableKey
    | TableTop      -- any other key may have any other value
    | TableBottom   -- any other key is nil
    

data TableKey
    = NumberKey LuaNumber
    | StringKey LuaString
    deriving (Eq, Ord)
    

data FunctionType
    = FunctionType [LuaTypeSet] [LuaTypeSet] FunctionEffects
    | FunctionTop
    

data FunctionEffects
    = FunctionEffects (M.Map LuaVariable (LuaTypeSet {- type before -}, LuaTypeSet {- type after -}))
    | EffectTop

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

-- | Table lattice. 
--   NOTE: assumes tables are sorted on keys. Make sure to maintain this invariant.
instance Lattice TableType where
 a </ b = 
   case (a,b) of
    (_          , TableTop) -> True
    (TableBottom, _       ) -> True
    (TableCons (ka, ta) as, TableCons (kb, tb) bs) -> case compare ka kb of
                                                       LT -> a </ bs
                                                       GT -> False
                                                       EQ -> ta </ tb && as </ bs
    (_,_) -> False
  
 join a b =
  case (a,b) of
   (TableTop, _)    -> TableTop
   (_, TableTop)    -> TableTop
   (TableBottom, t) -> t
   (t, TableBottom) -> t
   (TableCons (ka, ta) as, TableCons (kb, tb) bs) -> 
    case compare ka kb of
     LT -> TableCons (ka, ta) $ as `join` b
     GT -> TableCons (kb, tb) $ a `join` bs
     EQ -> TableCons (ka, ta `join` tb) $ as `join` bs

 meet a b =
    case (a,b) of
     (TableTop, t)    -> t
     (t, TableTop)    -> t
     (TableBottom, _) -> TableBottom
     (_, TableBottom) -> TableBottom
     (TableCons (ka, ta) as, TableCons (kb, tb) bs) -> 
      case compare ka kb of
       LT -> as `meet` b
       GT -> a `meet` bs
       EQ | ta `meet` tb </ bottom -> as `meet` bs
          | otherwise              -> TableCons (ka, ta `meet` tb) $ as `meet` bs
       

 bottom = TableBottom
 top = TableTop

-- | Function lattice.
instance Lattice FunctionType where
 _ </ FunctionTop = True
 FunctionTop </ _ = False

 FunctionType pIn pOut pEffects </ FunctionType qIn qOut qEffects
    | length pIn <= length qIn, length pOut <= length qOut
    , all id (zipWith (</) qIn pIn)     -- contravariant
    , all id (zipWith (</) pOut qOut)   -- covariant
    , pEffects </ qEffects
    = True
    | otherwise = False
 join p q
    | p </ q = p
    | q </ p = q
    | otherwise = bottom
 meet p q
    | p </ q = q
    | q </ p = p
    | otherwise = top
 bottom = FunctionType [] [] (FunctionEffects [])
 top = FunctionTop

-- | Function effects lattice.
instance Lattice FunctionEffects where
 _ </ EffectTop = True
 EffectTop </ _ = False

 FunctionEffects ps </ FunctionEffects qs = isWeakSubmapOfBy cmpEffects ps qs
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

 join x EffectTop = x
 join EffectTop x = x
 join (FunctionEffects ps) (FunctionEffects qs) = FunctionEffects $ M.unionWith effJoin ps qs
  where
    effJoin :: (LuaTypeSet, LuaTypeSet) -> (LuaTypeSet, LuaTypeSet) -> (LuaTypeSet, LuaTypeSet)
    effJoin (pBefore, pAfter) (qBefore, qAfter) = (pBefore `meet` qBefore, pAfter `join` qAfter)
 
 meet _ EffectTop = EffectTop
 meet EffectTop _ = EffectTop
 meet (FunctionEffects ps) (FuncitonEffects qs) = FunctionEffecst $ M.unionWith effMeet ps qs
  where
    effMeet :: (LuaTypeSet, LuaTypeSet) -> (LuaTypeSet, LuaTypeSet) -> (LuaTypeSet, LuaTypeSet)
    effMeet (pBefore, pAfter) (qBefore, qAfter) = (pBefore `join` qBefore, pAfter `meet` qAfter)

 bottom = FunctionEffects []
 top = EffectTop

isWeakSubmapOfBy :: (Maybe v -> Maybe v -> Bool) -> M.Map k v -> M.Map k v -> Bool
isWeakSubmapOfBy cmp lhs rhs
    = S.all (uncurry cmp)
    . S.map (\k -> lhs `M.lookup` k, rhs `M.lookup` k))
    $ M.keys lhs `S.union` M.keys rhs

-- | The Lua type set lattice.
instance Lattice LuaTypeSet where
 LuaTypeSet xs </ LuaTypeSet ys = all (\x -> any (subType x) ys) ys
 join (LuaTypeSet xs) (LuaTypeSet ys) = LuaTypeSet $ unionBy sameType xs ys
 meet (LuaTypeSet xs) (LuaTypeSet ys) = LuaTypeSet $ intersectBy sameType xs ys
 bottom = LuaTypeSet []
 top = topLuaTypeSet
