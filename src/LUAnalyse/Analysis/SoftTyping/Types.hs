module LUAnalyse.Analysis.SoftTyping.Types where

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
    , Function (FunctionType topLuaTypeSet topLuaTypeSet EffectTop)
    ]

data TableType
    = TableCons (TableKey, LuaTypeSet) TableType
    | TableTop      -- any other key may have any other value
    | TableBottom   -- any other key is nil

data TableKey
    = NumberKey LuaNumber
    | StringKey LuaString

data FunctionType
    = FunctionType [LuaTypeSet] [LuaTypeSet] FunctionEffects

data FunctionEffects
    = FunctionEffects [(LuaVariable, LuaTypeSet -> LuaTypeSet)]
    | EffectTop


-- | Table lattice.
instance Lattice TableType where
 (</) = undefined 
 join = undefined
 meet = undefined
 bottom = TableBottom
 top = TableTop

-- | Function effects lattice.
instance Lattice FunctionEffects where
 (</) = undefined 
 join = undefined
 meet = undefined
 bottom = FunctionEffects []
 top = EffectTop

-- | The Lua type set lattice.
instance Lattice LuaTypeSet where
 (</) = undefined 
 join = undefined
 meet = undefined
 bottom = LuaTypeSet []
 top = topLuaTypeSet