module Haskohol (
    Formula (..),
    Theorem,
    Proof,
    valid,
    tautology,
    inconsistent,
    conclusion,
    botElim,
    botIntro,
    conjElim1,
    conjElim2,
    conjIntro,
    hypothesis,
    implElim,
    implIntro,
    topIntro,
    weaken,
    andThen,
    andThenWith,
    appliedTo,
) where

import Data.Set (Set)
import qualified Data.Set as Set

import Base (
    Formula (..),
    Proof,
    Theorem,
    conclusion,
    inconsistent,
    tautology,
    valid,
 )

import Combinators (
    andThen,
    andThenWith,
    appliedTo,
 )

import Rules.Logical (
    botElim,
    botIntro,
    conjElim1,
    conjElim2,
    conjIntro,
    hypothesis,
    implElim,
    implIntro,
    topIntro,
 )

import Rules.Structural (
    weaken,
 )
