module MonoidOptional where
import Data.Monoid

data Optional a =
  Nada | Only a
  deriving (Eq, Show)

-- We need to instantiate Semigroup too, since (<>) was
-- moved from Monoid to Supergroup and mappend is just a
-- synonym for (<>)
instance Semigroup a => Semigroup (Optional a)
  where
    (<>) (Only a) (Only b) = Only (a <> b)
    (<>) (Only a) Nada     = Only a
    (<>) Nada     (Only b) = Only b
    (<>) Nada     Nada     = Nada

instance (Semigroup a, Monoid a) => Monoid (Optional a) where
  mempty = Nada

-- >>> Only (Sum 10) <> Only (Sum 20)
-- >>> Nada <> Only (Sum 10)
-- >>> Only (Product 20) <> Nada
-- >>> Nada <> Nada
-- >>> Only (Product 10) <> Only (Product 20)
-- Only (Sum {getSum = 30})
-- Only (Sum {getSum = 10})
-- Only (Product {getProduct = 20})
-- Nada
-- Only (Product {getProduct = 200})

