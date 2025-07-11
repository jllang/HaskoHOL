module Rules.Structural where

import Data.Set (Set)
import qualified Data.Set as Set

import Base (
    Formula,
    Proof (Malformed, QED),
    Rule,
    Theorem (Theorem, antecedents),
 )

{-
        H ⊢ A
   -------------
      H,B ⊢ A
-}
weaken :: Rule (Formula, Theorem)
weaken (f, t) = QED t{antecedents = Set.insert f t.antecedents}
