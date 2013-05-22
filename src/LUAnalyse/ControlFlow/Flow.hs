{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.ControlFlow.Flow where

import Data.Map hiding (map, member)

-- The flow graph.
type Flow = Map BlockReference Block

-- A block of instructions.
data Block = Block [Instruction] FlowInstruction
               deriving (Show)

-- TODO: Incorporate functions, with in and out values (arguments and return values)

-- Normal instructions.
data Instruction = ConstInstr {var :: Variable, constant :: Constant} -- a = constant

                 -- Special operators.
                 | CallInstr {var :: Variable, method :: Variable, args :: [Variable]} -- a = b(c, d, e)
                 | LengthInstr {var :: Variable, value :: Variable} -- a = #b
                 | ConcatInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b .. c
                 | MemberInstr {var :: Variable, value :: Variable, member :: Name} -- a = b.name
                 | IndexInstr {var :: Variable, value :: Variable, index :: Variable} -- a = b[c]
                 | NewMemberInstr {var :: Variable, member :: Name, value :: Variable} -- a.name = b
                 | NewIndexInstr {var :: Variable, index :: Variable, value :: Variable} -- a[name] = b
                 
                 -- Arithmetic operators.
                 | AddInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b + c
                 | SubInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b - c
                 | MulInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b * c
                 | DivInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b / c
                 | ModInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b % c
                 | PowInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b ^ c
                 | MinusInstr {var :: Variable, value :: Variable} -- a = -b
                 
                 -- Relational operators.
                 | EqInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b == c
                 | NotEqInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b ~= c
                 | LessInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b < c
                 | GreaterInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b > c
                 | LessEqInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b <= c
                 | GreaterEqInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b >= c
                 
                 -- Logical operators.
                 | NotInstr {var :: Variable, value :: Variable} -- a = not b
                    deriving (Show)

-- Flow instructions.
data FlowInstruction = JumpInstr {target :: BlockReference} -- goto block
                     | CondJumpInstr {target :: BlockReference, alternative :: BlockReference, cond :: Variable} -- if (a) { goto block }
                     | ReturnInstr -- return
                        deriving (Show)

-- Constants.
data Constant = NumberConst Double -- 10.1
              | StringConst String -- "abc"
              | BooleanConst Bool -- true
              | TableConst -- {}
              | NilConst -- nil
                 deriving (Show)
              -- TODO: Function ? ... ? {...} ?

-- A variable reference.
newtype Variable = Variable String deriving (Show)

-- A block reference.
type BlockReference = Int

-- A name.
newtype Name = Name String deriving (Show)
