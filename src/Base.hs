module Base where

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Text.Regex.TDFA

data Formula
    = -- | Universally true atom
      Top
    | -- | Universally false atom
      Bot
    | -- | Propositional atom
      Atom String
    | -- | Conjunction
      (:&) Formula Formula
    | -- | Disjunction
      (:|) Formula Formula
    | -- | Implication
      (:>) Formula Formula
    | -- | Not
      Not Formula
    deriving (Eq, Ord)

instance Show Formula where
    show = \case
        Top -> "⊤"
        Bot -> "⊥"
        Atom s -> '\'' : s
        a :& b -> show a ++ " & " ++ show b
        a :| b -> show a ++ " | " ++ show b
        a :> b -> show a ++ " > " ++ show b
        Not a -> show "~" ++ show a

instance IsString Formula where
    fromString s
        | s =~ "^[a-zA-Z-]+$" = Atom s
        | otherwise = error $ "Invalid atom \"" ++ s ++ "\""

{- | A set of arbitrary formula without assertion of validity. Antecedents may
be even contradictory to each other.
-}
type Antecedents = Set Formula

{- | A theorem is a set of antecedents under which it is justified to conclude
the consequent. Metatheoretically we could say that antecedents are assumptions
from which the consequent follows. However, the formal machinery requires
separating the metatheoretic implication @QED@ and logical implication @:>@.
The goal of Haskohol is that only true theorems can be constructed using axioms
and deduction rules.
-}
data Theorem = Theorem {antecedents :: Antecedents, consequent :: Formula}

instance Show Theorem where
    show (Theorem as f) = prefix ++ " ⊢ " ++ show f
      where
        prefix
            | Set.null as = "{}"
            | otherwise =
                "{ " ++ intercalate ", " (map show (Set.toList as)) ++ " }"

{- | A Proof produces a theorem if and only if it has been constructed by
correctly applying rules of inference. Deduction rules shall return @Malformed@
when applied illegally and are thus called malformed deductions. Anything that
produces a conclusion by applying deduction rules or axioms one or more times
is called a deduction in metatheory.
-}
data Proof
    = Malformed
    | QED Theorem

instance Show Proof where
    show = \case
        Malformed -> "Malformed deduction"
        QED t -> show t ++ " ∎"

type Axiom = Proof
type Rule a = a -> Proof

-- * Semantic Predicates

{- | A Proof is valid if it has been proven with a well-formed deduction. A
valid conclusion may depend on contextual assumptions (antecedents) to be true.
It might not be a universal property of logic in a category theoretical sense.
-}
valid :: Proof -> Bool
valid = \case
    QED _ -> True
    _ -> False

{- | A Proof is tautological if it is valid and has no antecedents (i.e.
non-discharged assumptions). Tautological conclusions are true no matter the
circumstances. In that sense, they are universal truths of logic itself.
-}
tautology :: Proof -> Bool
tautology = \case
    QED t -> Set.null t.antecedents
    _ -> False

{- | A malformed proof or a proof that concludes bottom is called inconsistent.
There are other forms of inconsistency such as the conclusion of the deduction
implying bottom. Unfortunately for a logic expressive enough, there is no way
of determining whether an arbitrary proof implies bottom or not. Hence, the
property of consistency is only semidecidable. This particular definition of
inconsistency does not give false positives but it may give a false negative,
e.g. when the consequent of the conclusion is @'A & ~A@ which implies botton by
the @botElim@ rule. The goal of Haskhol is to be consistent as a proof system
so that inconsistent conclusions are not constructible using the public API.
Proving the consistency of Haskhol is not possible in Haskell.
-}
inconsistent :: Proof -> Bool
inconsistent d
    | conclusion d == Bot = True
    | otherwise = False

{- | Returns the concluded formula or bottom if the conclusion was obtained by
a malformed deduction.
-}
conclusion :: Proof -> Formula
conclusion = \case
    QED t -> t.consequent
    _ -> Bot
