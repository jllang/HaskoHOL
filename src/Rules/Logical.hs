module Rules.Logical where

import Data.Set (Set)
import qualified Data.Set as Set

import Base (
    Antecedents,
    Formula (..),
    Proof (Malformed, QED),
    Rule,
    Theorem (..),
 )

{-

   -------------
      {A} ⊢ A
-}
hypothesis :: Rule Formula
hypothesis f = QED (Theorem (Set.singleton f) f)

{-

   -----------
      H ⊢ ⊤
-}
topIntro :: Rule Antecedents
topIntro as = QED $ Theorem as Top -- could be also `axiom Top`

{-
      H ⊢ A & ~A
   ----------------
      H ⊢ ⊥
-}
botIntro :: Rule Theorem
botIntro = \case
    Theorem as (a :& Not b) | a == b -> QED $ Theorem as Bot
    _ -> Malformed

{-
      H ⊢ ⊥
   ------------
      H ⊢ A
-}
botElim :: Rule (Formula, Theorem)
botElim (f, t) = case t of
    Theorem as Bot -> QED $ Theorem as f
    _ -> Malformed

{-
      H ⊢ A    H ⊢ B
   --------------------
        H ⊢ A & B
-}
conjIntro :: Rule (Theorem, Theorem)
conjIntro (a, b)
    | a.antecedents == b.antecedents =
        QED $ Theorem a.antecedents (a.consequent :& b.consequent)
    | otherwise = Malformed

{-
      H ⊢ A & B
   ----------------
      H ⊢ A
-}
conjElim1 :: Rule Theorem
conjElim1 = \case
    Theorem as (a :& b) -> QED $ Theorem as a
    _ -> Malformed

{-
      H ⊢ A & B
   ---------------
      H ⊢ B
-}
conjElim2 :: Rule Theorem
conjElim2 = \case
    Theorem as (a :& b) -> QED $ Theorem as b
    _ -> Malformed

{-
      H ⊢ A
   ---------------
      H ⊢ A | B
-}
disjIntro1 :: Rule (Formula, Theorem)
disjIntro1 (f, t) = QED $ t{consequent = t.consequent :| f}

{-
      H ⊢ B
   ---------------
      H ⊢ A | B
-}
disjIntro2 :: Rule (Formula, Theorem)
disjIntro2 (f, t) = QED $ t{consequent = f :| t.consequent}

{-
      H ⊢ A | B     H, A ⊢ C     H, B ⊢ C
   -----------------------------------------
                       H ⊢ C
-}
disjElim :: Rule (Theorem, Theorem, Theorem)
disjElim (Theorem as1 (a :| b), Theorem as2 c2, Theorem as3 c3)
    | c2 == c3 && as2 == Set.insert a as1 && as3 == Set.insert b as1 = QED $ Theorem as1 c2
disjElim _ = Malformed

{-
      H,A ⊢ B
   -----------------
        H ⊢ A > B
-}
implIntro :: Rule (Formula, Theorem)
implIntro (f, Theorem as g)
    | f `Set.member` as = QED $ Theorem (Set.delete f as) (f :> g)
    | otherwise = Malformed

{-
      H ⊢ A > B   H ⊢ A
   -----------------------
            H ⊢ B
-}
implElim :: Rule (Theorem, Theorem)
implElim (Theorem as (f :> g), Theorem bs h)
    | as == bs && f == h = QED $ Theorem as g
    | otherwise = Malformed
