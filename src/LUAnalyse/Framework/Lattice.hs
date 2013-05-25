{-# LANGUAGE Haskell2010 #-}

-- | Definines a type class for lattices and operation upon them.
module LUAnalyse.Framework.Lattice (Lattice (..)) where

-- Operator precedences.
infix 4 </
infix 4 \>
infix 6 `join`
infix 6 `meet`

-- | An instance of this class forms a complete lattice in combination with a partial order 
--   relation represented by the operator (</). Since this operator is a class member, only one 
--   lattice can be defined per datatype.
class Eq a => Lattice a where
 -- | A partial order on the datatype a for which (a, </) is a lattice. It has a (probably 
 --   innefficient) default definition in terms of join; overriding this is recommended.
 (</) :: a -> a -> Bool
 x </ y = x `join` y == y

  -- | (</) with its arguments flipped.
 (\>) :: a -> a -> Bool
 (\>) = flip (</)

 -- | The lattice join operator. 
 join :: a -> a -> a

 -- | The lattice meet operator. 
 meet :: a -> a -> a

 -- | The lattice bottom. @bottom </@ any other value of type a.
 bottom :: a

 -- | The lattice top. @top \>@ any other value of type a.
 top :: a

 -- | Joins the elements of a list.
 union :: [a] -> a
 union = foldr join bottom

 -- | Meets the elements of a list.
 intersection :: [a] -> a
 intersection = foldr meet top



-- | Simple example instance of (bounded) integer lattice with relation <=.
instance Lattice Int where
 (</) = (<=)
 join = max
 meet = min
 bottom = minBound
 top = maxBound
 union = maximum
 intersection = minimum

