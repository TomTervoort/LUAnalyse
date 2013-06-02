{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module LUAnalyse.Analysis.SoftTyping.Analysis where

import LUAnalyse.ControlFlow.Flow
import LUAnalyse.Framework.Framework
import LUAnalyse.Framework.Lattice
import LUAnalyse.Analysis.SoftTyping.Types (LuaType, LuaTypeSet (..))

import Utility (outerUnionWith)

import qualified Data.Map as M
import qualified LUAnalyse.Analysis.SoftTyping.Types as Ty

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
    
    -- TODO [| var |] = [| return-of method |], given [| method |] < function,
    -- and ensure that argument types work
    transfer _ CallInstr    {..} = id
    -- [| var |] = number (nat!) (fin?), given [| value |] is a sequence-coercible type
    transfer _ LengthInstr  {..} = txOverwriteType var (singleType Ty.Number)
    -- [| var |] = string, given [| lhs |], [| rhs |] both sequence-coercible
    transfer _ ConcatInstr  {..} = txOverwriteType var (singleType Ty.String)

    -- Given [| value |] < table, [| var |] = [| value.member |]
    transfer _ MemberInstr  {..} = id
    -- Given [| value |] < table, [| var |] = [| value.index |]
    transfer _ IndexInstr   {..} = id
    -- Given [| var |] < table, [| var.member |] = [| value |]
    transfer _ NewMemberInstr {..} = id
    -- Given [| var |] < table, [| var.index |] = [| value |]
    transfer _ NewIndexInstr {..} = id

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

    -- TODO isDistributive?
