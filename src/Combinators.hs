module Combinators where

import Base (
    Proof (Malformed, QED),
    Rule,
    Theorem (Theorem),
 )

andThen :: Proof -> Rule Theorem -> Proof
andThen c f = case c of
    Malformed -> Malformed
    QED t -> f t

andThenWith :: Proof -> (Rule (a, Theorem), a) -> Proof
andThenWith t (r, x) = t `andThen` curry r x

appliedTo :: Rule (Theorem, Theorem) -> (Proof, Proof) -> Proof
appliedTo r = \case
    (QED t1, QED t2) -> r (t1, t2)
    _ -> Malformed
