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
class Eq l => Lattice l where
 -- | A partial order on the datatype l for which (l, </) is a lattice. It has a (probably 
 --   innefficient) default definition in terms of join; overriding this is recommended.
 (</) :: l -> l -> Bool
 x </ y = x `join` y == y

  -- | (</) with its arguments flipped.
 (\>) :: l -> l -> Bool
 (\>) = flip (</)

 -- | The lattice join operator. 
 join :: l -> l -> l

 -- | The lattice meet operator. 
 meet :: l -> l -> l

 -- | The lattice bottom. @bottom </@ any other value of type a.
 bottom :: l

 -- | The lattice top. @top \>@ any other value of type a.
 top :: l

 -- | Joins the elements of a list.
 union :: [l] -> l
 union = foldr join bottom

 -- | Meets the elements of a list.
 intersection :: [l] -> l
 intersection = foldr meet top

-- | A Bool lattice that uses boolean implication as the relation. This means bottom is False and 
--   top is True.
instance Lattice Bool where
 a </ b = not a || b
 join = (||)
 meet = (&&)
 bottom = False
 top = True
