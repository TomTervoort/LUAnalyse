{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.Analysis.SoftTyping.Types where

import LUAnalyse.Framework.Lattice

type LuaNumber = Double
type LuaString = String
type LuaVariable = String

data LuaType
    = Nil
    | Boolean
    | Number -- range? integral?
    | String -- number-coercible or not? length range?
    | Table TableType -- array? cardinality?
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
    = FunctionEffects [(LuaVariable, LuaTypeSet -> LuaTypeSet)]
    | EffectTop


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
 (</) = undefined 
 join = undefined
 meet = undefined
 bottom = FunctionType [] [] (FunctionEffects [])
 top = FunctionTop

-- | The Lua type set lattice.
instance Lattice LuaTypeSet where
 (</) = undefined 
 join = undefined
 meet = undefined
 bottom = LuaTypeSet []
 top = topLuaTypeSet