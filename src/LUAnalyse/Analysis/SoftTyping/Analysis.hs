{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module LUAnalyse.Analysis.SoftTyping.Analysis where

import LUAnalyse.ControlFlow.Flow
import LUAnalyse.Framework.Framework
import LUAnalyse.Framework.Lattice
import LUAnalyse.Analysis.SoftTyping.Types
    (LuaType, LuaTypeSet (..)
    , FunctionType (..), FunctionEffects (..)
    , singleType)
import qualified LUAnalyse.Analysis.SoftTyping.Types as Ty

import Utility (outerUnionWith)

import Prelude hiding (all, foldr)
import Data.Foldable

import Data.Lens.Common
import Control.Arrow
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

    SoftTypingLattice p </ SoftTypingLattice q = all id $ outerUnionWith stlLeq q p
      where
        stlLeq :: Maybe LuaTypeSet -> Maybe LuaTypeSet -> Bool
        stlLeq Nothing _ = False
        stlLeq _ Nothing = True
        stlLeq (Just qq) (Just pp) = qq </ pp

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

txNotNil :: Variable -> SoftTypingLattice -> SoftTypingLattice
txNotNil var = txConstrainType var (Ty.ltsNil ^= bottom $ top)

txGetType :: Variable -> SoftTypingLattice -> LuaTypeSet
txGetType var (SoftTypingLattice l) = M.findWithDefault top var l
txGetType var SoftTypingLatticeTop = bottom

-- | When we know that two variables can only be used in situations where they
--   have the same type, we need to constrain their type sets by the greatest
--   set they have in common. No coercion is implied.
txConstrainEqualTypes :: Variable -> Variable -> SoftTypingLattice -> SoftTypingLattice
txConstrainEqualTypes lhs rhs l
  = let -- In case we keep track of more precise types, we need to simplify
        -- the type sets, so that the notion of "compatible" types is not
        -- limited by "having the same range of values".
        lhsTys = simplifyTypeSet $ txGetType lhs l
        rhsTys = simplifyTypeSet $ txGetType rhs l
        common = lhsTys `meet` rhsTys
    in txConstrainType lhs common . txConstrainType rhs common $ l

-- | Erase precise information (like ranges or properties of values, besides
--   types).
simplifyTypeSet :: LuaTypeSet -> LuaTypeSet
simplifyTypeSet = id

txReadTable :: Variable -> Variable -> LuaTypeSet -> SoftTypingLattice -> SoftTypingLattice
txReadTable dst tab key l
  = let tableType = txGetType tab l
    in    txOverwriteType dst (Ty.tableMemberType tableType key)
        . txConstrainType tab (singleType $ Ty.Table top)
        $ l

txWriteTable :: Variable -> Variable -> LuaTypeSet -> SoftTypingLattice -> SoftTypingLattice
txWriteTable src tab key l
  = let tableType = txGetType tab l
        srcType = txGetType src l
    in    txOverwriteType tab (Ty.advanceTableType tableType key srcType)
        . txConstrainType tab (singleType $ Ty.Table top)
        $ l

-- | Executes the side-effects of a function upon the soft-typing lattice.
runFunctionEffects :: FunctionEffects -> SoftTypingLattice -> SoftTypingLattice
runFunctionEffects EffectTop = const bottom
runFunctionEffects (FunctionEffects effs) = 
 foldr (>>>) id [txConstrainType var before >>> txOverwriteType var after 
                 | (var, (before, after)) <- M.assocs effs               ]


luaConstantType :: Constant -> LuaTypeSet
luaConstantType (FunctionConst _ref) = singleType $ Ty.Function top
luaConstantType (NumberConst _value) = singleType Ty.Number
luaConstantType (StringConst value) = Ty.constantStringType value
luaConstantType (BooleanConst value) = Ty.constantBooleanType value
luaConstantType TableConst = singleType $ Ty.Table Ty.emptyTableType
luaConstantType NilConst = singleType Ty.Nil

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
    . txConstrainEqualTypes lhs rhs
    . txConstrainType lhs (singleType Ty.Number `join` singleType Ty.String)
    . txConstrainType rhs (singleType Ty.Number `join` singleType Ty.String)

instance Analysis SoftTypingAnalysis SoftTypingLattice where
    transfer _ AssignInstr  {..} = assignmentTx var value
    transfer _ ConstInstr   {..} = txOverwriteType var (luaConstantType constant)
    
    -- [| var |] = [| return-of func |], given [| func |] < function,
    -- and ensure that argument types work, also apply effects
    transfer _ CallInstr    {..} = \l -> 
     case txGetType func l ^. Ty.ltsFunction of
      ft@(FunctionType ins [out] effs) | length ins == length args 
       -> -- Constrain arguments with input types; var with output type; run side-effects.
          -- Also fix func's type.
              foldr (>>>) id (zipWith txConstrainType args ins) 
          >>> txConstrainType var out
          >>> runFunctionEffects effs
          >>> txOverwriteType func (singleType $ Ty.Function ft)
             $ l
      FunctionBottom -> bottom -- The can't possibly be a function, so we stop here.
      FunctionTop -> bottom -- All bets are off. We lost all information, because
                            -- this function have any effect.
      FunctionType _ _ _ -> error "LUAnalyse.Analysis.SoftTyping#L157"

    -- [| var |] = number (nat!) (fin?), given [| value |] is a sequence-coercible type
    transfer _ LengthInstr  {..} = txOverwriteType var (singleType Ty.Number)
    -- [| var |] = string, given [| lhs |], [| rhs |] both sequence-coercible
    transfer _ ConcatInstr  {..} = txOverwriteType var (singleType Ty.String)

    -- Given [| value |] < table, [| var |] = [| value.member |]
    transfer _ MemberInstr  {..}
      = let key = Ty.constantStringType $ unName member
        in txReadTable var value key
    -- Given [| value |] < table, [| var |] = [| value[index] |]
    transfer _ IndexInstr   {..}
      = let keyIn = txGetType index
        in \l -> txReadTable var value (keyIn l) l

    -- Given [| var |] < table, [| var.member |] = [| value |]
    transfer _ NewMemberInstr {..}
      = let key = Ty.constantStringType $ unName member
        in txWriteTable value var key
    -- Given [| var |] < table, [| var[index] |] = [| value |]
    transfer _ NewIndexInstr {..}
      = let keyIn = txGetType index
        in \l -> txWriteTable value var (keyIn l) . txNotNil index $ l

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

    -- Given: both operand types are of equal type, and either numbers or strings.
    transfer _ LessInstr    {..} = orderingTestTx var lhs rhs
    transfer _ GreaterInstr {..} = orderingTestTx var lhs rhs
    transfer _ LessEqInstr  {..} = orderingTestTx var lhs rhs
    transfer _ GreaterEqInstr {..} = orderingTestTx var lhs rhs

    -- Note: operands can be of any type; only false and nil are considered false.
    transfer _ NotInstr     {..} = txOverwriteType var $ singleType Ty.Boolean

    analysisKind _ = (MayAnalysis, ForwardAnalysis)
