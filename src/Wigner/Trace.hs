module Wigner.Trace(
    trace
    ) where

import Wigner.Expression
import Wigner.ExpressionHelpers


class Traceable a where
    trace :: a -> a

instance Traceable Expr where
    trace (Expr2by2 e11 e12 e21 e22) = trace e11 + trace e22
    trace expr@(Expr e) = mapOpFactors (makeExpr . toTrace) expr where
        toTrace opf@(MatProduct mats) = Trace opf
        toTrace _ = error "Not implemented: trace of operator product"
