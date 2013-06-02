{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module LUAnalyse.Analysis.SoftTyping.Analysis where

import LUAnalyse.ControlFlow.Flow
import LUAnalyse.Framework.Framework
import LUAnalyse.Framework.Lattice
import LUAnalyse.Analysis.SoftTyping.Types (LuaType, LuaTypeSet (..), TableType (..),
                                            FunctionType (..), FunctionEffects (..))
import qualified LUAnalyse.Analysis.SoftTyping.Types as Ty

import Utility (outerUnionWith)

import Data.Maybe
import Control.Arrow
import Control.Monad hiding (join)
import qualified Data.Map as M

data SoftTypingAnalysis = SoftTypingAnalysis
data SoftTypingLattice
    -- | By default, a variable can be of any type (@top :: LuaTypeSet@).
    --   In the bottom, every variable can be of any type, so the map is
    --   empty.
    = SoftTypingLattice (M.Map Variable LuaTypeSet)
    -- | The top is reached if every imaginable @Variable@ can't
    --   exist under the typing rules (@bottom :: LuaTypeSet@). This
    --   can only occur, in an infinitely sized program or when writing
    --   to the global variable table.
    | SoftTypingLatticeTop
    deriving Show

instance Lattice SoftTypingLattice where
    _ </ SoftTypingLatticeTop = True
    SoftTypingLatticeTop </ _ = False

    SoftTypingLattice p </ SoftTypingLattice q = outerUnionWith True stlLeq q p
      where
        stlLeq :: Maybe LuaTypeSet -> Maybe LuaTypeSet -> Bool -> Bool
        stlLeq _ _ False = False
        stlLeq Nothing _ _ = False
        stlLeq _ Nothing _ = True
        stlLeq (Just qq) (Just pp) _ = qq </ pp

    join SoftTypingLatticeTop _ = SoftTypingLatticeTop
    join _ SoftTypingLatticeTop = SoftTypingLatticeTop
    join (SoftTypingLattice p) (SoftTypingLattice q) = SoftTypingLattice (M.unionWith join p q)

    meet SoftTypingLatticeTop x = x
    meet x SoftTypingLatticeTop = x
    meet (SoftTypingLattice p) (SoftTypingLattice q) = SoftTypingLattice (M.intersectionWith meet p q)

    top = SoftTypingLatticeTop
    bottom = SoftTypingLattice M.empty

txOverwriteType :: Variable -> LuaTypeSet -> SoftTypingLattice -> SoftTypingLattice
txOverwriteType var types (SoftTypingLattice l) = SoftTypingLattice $ M.insert var types l
txOverwriteType _ _ SoftTypingLatticeTop = SoftTypingLatticeTop -- TODO It's actually a semi-bound lattice...

txConstrainType :: Variable -> LuaTypeSet -> SoftTypingLattice -> SoftTypingLattice
txConstrainType var types lat@(SoftTypingLattice l) = SoftTypingLattice $ M.insert var (types `meet` (txGetType var lat)) l
txConstrainType _ _ SoftTypingLatticeTop = SoftTypingLatticeTop

txGetType :: Variable -> SoftTypingLattice -> LuaTypeSet
txGetType var (SoftTypingLattice l) = M.findWithDefault top var l

txConstrainEqualBaseTypes :: Variable -> Variable -> SoftTypingLattice -> SoftTypingLattice
txConstrainEqualBaseTypes lhs rhs l
  = let lhsTys = {- TODO convert to basic type -} txGetType lhs l
        rhsTys = {- TODO convert to basic type -} txGetType rhs l
        common = lhsTys `meet` rhsTys
    in txConstrainType lhs common . txConstrainType rhs common $ l

-- | Looks up a function type from the lattice. If the variable in question can not possibly
--   be a function, Nothing is returned. If multiple function types are possible, then those 
--   function types are joined togheter.
txGetFunctionType :: Variable -> SoftTypingLattice -> Maybe FunctionType
txGetFunctionType var (SoftTypingLattice l) =
 do LuaTypeSet ts <- M.lookup var l
    let fs = mapMaybe getFunc ts
    guard $ not (null fs)
    return $ union fs
 where getFunc (Ty.Function f) = Just f
       getFunc _            = Nothing

-- | Look up the type of a table. Nothing is returned if the variable in question can not possibly 
--   be a table. If multiple tables are possible, their union is returned.
txGetTableType :: Variable -> SoftTypingLattice -> Maybe TableType
txGetTableType var (SoftTypingLattice l) = 
 do LuaTypeSet ts <- M.lookup var l
    let tas = mapMaybe getTable ts
    guard $ not (null ts)
    return $ union tas
 where getTable (Ty.Table t) = Just t
       getTable _            = Nothing

-- | Looks up the type of a table member. Unnassigned members are considered nil.
getTableMemberType :: TableType -> Ty.TableKey -> LuaTypeSet
getTableMemberType tt key =
 case tt of
  TableTop                       -> top
  TableBottom                    -> LuaTypeSet [Ty.Nil]
  TableCons (k,t) ts | k == key  -> t
                     | otherwise -> getTableMemberType ts key

-- | Returns the updated type of a table of which a member has been assigned with a new type.
setTableMemberType :: TableType -> Ty.TableKey -> LuaTypeSet -> TableType
setTableMemberType tt key new =
 case tt of
  c@(TableCons (k, t) ts) -> case compare k key of
                              EQ -> TableCons (key, new) ts
                              LT -> TableCons (k, t) $ setTableMemberType ts key new
                              GT -> TableCons (key, new) c
  end                     -> TableCons (key, new) end

setTableUnkownIndexType :: TableType -> LuaTypeSet -> TableType
setTableUnkownIndexType tt ty =
 case tt of
  TableCons (k,t) ts -> TableCons (k, t `join` ty) $ setTableUnkownIndexType ts ty
  TableBottom        -> TableTop
  TableTop           -> TableTop

-- | Combines txGetTableType and getTableMemberType. Returns bottom if the variable can not be a
--   table.
txGetTableMemberType :: Variable -> Ty.TableKey -> SoftTypingLattice -> LuaTypeSet
txGetTableMemberType var key l = maybe bottom (flip getTableMemberType key) $ txGetTableType var l

-- | Sets or overwrites the type of a table member.
--   TODO: current behavior when var is no table: first create a new empty table. This is probably
--         undesired, but how else should this be handled?
txSetTableMemberType :: Variable -> Ty.TableKey -> LuaTypeSet -> SoftTypingLattice -> SoftTypingLattice
txSetTableMemberType var key val l = 
 let oldTable = fromMaybe bottom (txGetTableType var l)
     newTable = setTableMemberType oldTable key val
  in txOverwriteType var (singleType $ Ty.Table newTable) l

-- | Indicates that a table member should have a particular type, but without knowing which table 
--   member. This will join the type in question with all existing member types and make sure the
--   table ends with top.
txAddUnkownIndexType :: Variable -> LuaTypeSet -> SoftTypingLattice -> SoftTypingLattice
txAddUnkownIndexType var val l =
 let oldTable = fromMaybe bottom (txGetTableType var l)
     newTable = setTableUnkownIndexType oldTable val
  in txOverwriteType var (singleType $ Ty.Table newTable) l


-- | Executes the side-effects of a function upon the soft-typing lattice.
runFunctionEffects :: FunctionEffects -> SoftTypingLattice -> SoftTypingLattice
runFunctionEffects EffectTop = const bottom
runFunctionEffects (FunctionEffects effs) = 
 foldr (>>>) id [txConstrainType var before >>> txOverwriteType var after 
                 | (var, (before, after)) <- M.assocs effs               ]


luaConstantType :: Constant -> LuaType
luaConstantType (FunctionConst _ref) = Ty.Function top
luaConstantType (NumberConst _value) = Ty.Number
luaConstantType (StringConst _value) = Ty.String
luaConstantType (BooleanConst _value) = Ty.Boolean
luaConstantType TableConst = Ty.Table top
luaConstantType NilConst = Ty.Nil

singleType :: LuaType -> LuaTypeSet
singleType = LuaTypeSet . (:[])

assignmentTx var value l = txOverwriteType var (txGetType value l) l 
numericArithTx var lhs rhs
    = txOverwriteType var (singleType Ty.Number)
    . txConstrainType lhs (singleType Ty.Number)
    . txConstrainType rhs (singleType Ty.Number)
numericArithTxUnary var value
    = txOverwriteType var (singleType Ty.Number)
    . txConstrainType value (singleType Ty.Number)
equalityTestTx var lhs rhs
    = txOverwriteType var (singleType Ty.Boolean)

orderingTestTx var lhs rhs
    = txOverwriteType var (singleType Ty.Boolean)
    . txConstrainEqualBaseTypes lhs rhs
    . txConstrainType lhs (LuaTypeSet [Ty.Number, Ty.String])
    . txConstrainType rhs (LuaTypeSet [Ty.Number, Ty.String])

instance Analysis SoftTypingAnalysis SoftTypingLattice where
    transfer _ AssignInstr  {..} = assignmentTx var value
    transfer _ ConstInstr   {..} = txOverwriteType var (singleType . luaConstantType $ constant)
    
    -- [| var |] = [| return-of func |], given [| func |] < function,
    -- and ensure that argument types work, also apply effects
    transfer _ CallInstr    {..} = \l -> 
     case txGetFunctionType func l of
      Just ft@(FunctionType ins [out] effs) | length ins == length args 
       -> -- Constrain arguments with input types; var with output type; run side-effects.
          -- Also fix func's type.
              foldr (>>>) id (zipWith txConstrainType args ins) 
          >>> txConstrainType var out
          >>> runFunctionEffects effs
          >>> txOverwriteType func (singleType $ Ty.Function ft)
             $ l
      _ -> bottom -- All bets are off. We lost all information.

    -- [| var |] = number (nat!) (fin?), given [| value |] is a sequence-coercible type
    transfer _ LengthInstr  {..} = txOverwriteType var (singleType Ty.Number)
    -- [| var |] = string, given [| lhs |], [| rhs |] both sequence-coercible
    transfer _ ConcatInstr  {..} = txOverwriteType var (singleType Ty.String)

    -- Given [| value |] < table, [| var |] = [| value.member |]
    transfer _ MemberInstr  {..} = \l -> txOverwriteType var (txGetTableMemberType value key l) l
     where key = Ty.StringKey $ unName member 
    -- Given [| value |] < table, [| var |] = [| value.index |]
    -- TODO?: figure out if index is constant and utilise that fact.
    transfer _ IndexInstr   {..} = txOverwriteType var top

    -- Given [| var |] < table, [| var.member |] = [| value |]
    transfer _ NewMemberInstr {..} = \l -> let key = Ty.StringKey $ unName member
                                               val = txGetType value l
                                            in txSetTableMemberType var key val l
    -- Given [| var |] < table, [| var.index |] = [| value |]
    -- TODO?: figure out if index is constant and utilise that fact.
    transfer _ NewIndexInstr {..} = \l -> txAddUnkownIndexType var (txGetType value l) l

    -- All given that [| lhs |] and [| rhs |] < number
    transfer _ AddInstr     {..} = numericArithTx var lhs rhs
    transfer _ SubInstr     {..} = numericArithTx var lhs rhs
    transfer _ MulInstr     {..} = numericArithTx var lhs rhs
    transfer _ PowInstr     {..} = numericArithTx var lhs rhs
    transfer _ DivInstr     {..} = numericArithTx var lhs rhs
    transfer _ ModInstr     {..} = numericArithTx var lhs rhs

    -- [| value |] < number
    transfer _ MinusInstr   {..} = numericArithTxUnary var value

    -- Note: operands of inequal types are inequal.
    -- Note: tables are always inequal, even where this breaks (anti)reflexivity.
    transfer _ EqInstr      {..} = equalityTestTx var lhs rhs
    transfer _ NotEqInstr   {..} = equalityTestTx var lhs rhs

    -- Given: both operand types are of equal type, and either a numbers or strings.
    transfer _ LessInstr    {..} = orderingTestTx var lhs rhs
    transfer _ GreaterInstr {..} = orderingTestTx var lhs rhs
    transfer _ LessEqInstr  {..} = orderingTestTx var lhs rhs
    transfer _ GreaterEqInstr {..} = orderingTestTx var lhs rhs

    -- Note: operands can be of any type; only false and nil are considered false.
    transfer _ NotInstr     {..} = txOverwriteType var $ singleType Ty.Boolean

    analysisKind _ = (MayAnalysis, ForwardAnalysis)
