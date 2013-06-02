{-# LANGUAGE Haskell2010 #-}

module LUAnalyse.ControlFlow.Flow where

import Data.Map hiding (map, member)

-- | Representation of a Lua program. Contains a program flow and denotes through which block in 
--   this flow the program is entered.
--data Program = Program {flow :: Flow, entry :: BlockReference}




-- | A label through which an instruction within a program can be identified.
data InstructionLabel = InstructionLabel BlockReference Int
                         deriving (Eq, Ord, Show)

-- A whole program.
data Program = Program {allFunctions :: Map FunctionReference Function, start :: FunctionReference}

-- A function.
data Function = Function {flow :: Flow, entry :: BlockReference, exit :: BlockReference, params :: [Variable], returnVar :: Variable}

-- The flow graph.
type Flow = Map BlockReference Block

-- A block of instructions.
data Block = Block [Instruction] FlowInstruction
               deriving (Show)

-- Normal instructions.
data Instruction = AssignInstr {var :: Variable, value :: Variable} -- a = b
                 | ConstInstr {var :: Variable, constant :: Constant} -- a = constant

                 -- Special operators.
                 | CallInstr {var :: Variable, method :: Variable, args :: [Variable]} -- a = b(c, d, e)
                 | LengthInstr {var :: Variable, value :: Variable} -- a = #b
                 | ConcatInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b .. c
                 | MemberInstr {var :: Variable, value :: Variable, member :: Name} -- a = b.name
                 | IndexInstr {var :: Variable, value :: Variable, index :: Variable} -- a = b[c]
                 | NewMemberInstr {var :: Variable, member :: Name, value :: Variable} -- a.name = b
                 | NewIndexInstr {var :: Variable, index :: Variable, value :: Variable} -- a[b] = c
                 
                 -- Arithmetic operators.
                 | AddInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b + c
                 | SubInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b - c
                 | MulInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b * c
                 | DivInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b / c
                 | PowInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b ^ c
                 | ModInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b % c
                 | MinusInstr {var :: Variable, value :: Variable} -- a = -b
                 
                 -- Relational operators.
                 | EqInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b == c
                 | NotEqInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b ~= c
                 | LessInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b < c
                 | GreaterInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b > c
                 | LessEqInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b <= c
                 | GreaterEqInstr {var :: Variable, lhs :: Variable, rhs :: Variable} -- a = b >= c
                 
                 -- Logical operators.
                 | NotInstr {var :: Variable, value :: Variable} -- a = not b
                    deriving (Show)

-- Flow instructions.
data FlowInstruction = JumpInstr {target :: BlockReference} -- goto block
                     | CondJumpInstr {target :: BlockReference, alternative :: BlockReference, cond :: Variable} -- if (a) { goto block }
                     | ReturnInstr -- return
                        deriving (Show)

-- Constants.
data Constant = FunctionConst FunctionReference -- function (a, b) ... end 
              | NumberConst Double -- 10.1
              | StringConst String -- "abc"
              | BooleanConst Bool -- true
              | TableConst -- {}
              | NilConst -- nil
                 deriving (Show)

-- A variable reference.
data Variable
    = Variable String
    | FreshVariable Int
    deriving (Eq, Ord)

instance Show Variable where
    show (Variable x) = x
    show (FreshVariable x) = '%' : show x

-- A block reference.
type BlockReference = Int

-- A function reference.
type FunctionReference = Int

-- A name.
newtype Name = Name String deriving (Eq, Show)
