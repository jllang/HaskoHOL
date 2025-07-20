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

type Antecedents = Set Formula

data Theorem = Theorem {antecedents :: Antecedents, consequent :: Formula}

instance Show Theorem where
    show (Theorem as f) = prefix ++ " ⊢ " ++ show f
      where
        prefix
            | Set.null as = "{}"
            | otherwise =
                "{ " ++ intercalate ", " (map show (Set.toList as)) ++ " }"

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

valid :: Proof -> Bool
valid = \case
    QED _ -> True
    _ -> False

tautology :: Proof -> Bool
tautology = \case
    QED t -> Set.null t.antecedents
    _ -> False

inconsistent :: Proof -> Bool
inconsistent d
    | conclusion d == Bot = True
    | otherwise = False

conclusion :: Proof -> Formula
conclusion = \case
    QED t -> t.consequent
    _ -> Bot
