{-# LANGUAGE OverloadedStrings #-}
module Theorems where

import Data.Set (Set)
import qualified Data.Set as Set

import Haskohol

topElim :: Proof
topElim =
    let {- {⊤ > A} ⊢ ⊤ > A       -} t1 = hypothesis (Top :> "A")
        {- {T > A} ⊢ ⊤           -} t2 = topIntro (Set.singleton (Top :> "A"))
        {- {T > A} ⊢ A           -} t3 = implElim `appliedTo` (t1, t2)
    in  {- {}      ⊢ (T > A) > A -} t3 `andThenWith` (implIntro, (Top :> "A"))

commConj :: Proof
commConj =
    let {- {A & B} ⊢ A & B         -} t1 = hypothesis ("A" :& "B")
        {- {A & B} ⊢ A             -} t2 = t1 `andThen` conjElim1
        {- {A & B} ⊢ B             -} t3 = t1 `andThen` conjElim2
        {- {A & B} ⊢ B & A         -} t4 = conjIntro `appliedTo` (t3, t2)
     in {- {}      ⊢ A & B > B & A -} t4 `andThenWith` (implIntro, ("A" :& "B"))
