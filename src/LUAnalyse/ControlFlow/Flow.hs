{-# LANGUAGE Haskell2010, DeriveDataTypeable #-}

module LUAnalyse.ControlFlow.Flow where

-- The flow graph.
type Flow = Block

-- A block of instructions.
newtype Block = Block [Instruction] FlowInstruction
                 deriving (Typeable, Data, Show)

-- TODO: Incorporate functions, with in and out values (arguments and return values)

-- Normal instructions.
data Instruction = AssignInstr {var :: Variable, value :: Variable} -- a = b, or just rename them ?
                 | ConstInstr {var :: Variable, value :: Constant} -- a = constant

                 -- Special operators.
                 | CallInstr {var :: Variable, method :: Name, args :: [Variable]} -- a = method(b, c, d)
                 | LengthInstr {var :: Variable, value :: Variable} -- a = #b
                 | ConcatInstr {var :: Variable, first :: Variable, second :: Variable} -- a = b .. c
                 | IndexInstr {var :: Variable, value :: Variable, index :: Name} -- a = b.name
                 | NewIndexInstr {var :: Variable, index :: Name, value :: Variable} -- a.name = b
                 
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
                    deriving (Typeable, Data, Show)

-- Flow instructions.
data FlowInstruction = JumpInstr {target :: Block} -- goto block
                     | CondJumpInstr {target :: Block, alternative :: Block, cond :: Variable} -- if (a) { goto block }
                        deriving (Typeable, Data, Show)

-- Constants.
data Constant = NumberConst Double -- 10.1
              | StringConst String -- "abc"
              | BooleanConst Bool -- true
              | TableConst -- {}
              | NilConst -- nil
              
              -- TODO: Function ? ... ? {...} ?

-- A variable reference.
newtype Variable = Variable String deriving (Typeable, Data, Show)

-- A name.
newtype Name = Name String deriving (Typeable, Data, Show)
