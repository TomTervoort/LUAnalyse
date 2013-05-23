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
