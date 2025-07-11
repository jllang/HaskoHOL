{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haskohol

import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = putStrLn "Hello, World!"

formula1 :: Formula
formula1 = Atom "p1" :& Atom "p2" -- (p1 & p2) informally for some propositional symbols p1, p2

proof2 :: Proof
proof2 =
    let {- {A}    ⊢ A     -} t1 = hypothesis "A"
        {- {B}    ⊢ B     -} t2 = hypothesis "B"
        {- {A, B} ⊢ A     -} t3 = t1 `andThenWith` (weaken, "B")
        {- {A, B} ⊢ B     -} t4 = t2 `andThenWith` (weaken, "A")
     in {- {A, B} ⊢ A & B -} conjIntro `appliedTo` (t1, t2)

proof3 :: Proof
proof3 =
    let {- {A & B} ⊢ A & B         -} t1 = hypothesis ("A" :& "B")
        {- {A & B} ⊢ A             -} t2 = t1 `andThen` conjElim1
        {- {A & B} ⊢ B             -} t3 = t1 `andThen` conjElim2
        {- {A & B} ⊢ B & A         -} t4 = conjIntro `appliedTo` (t3, t2)
     in {- {}      ⊢ A & B > B & A -} t4 `andThenWith` (implIntro, ("A" :& "B"))
