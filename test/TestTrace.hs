module TestTrace(test_group) where

import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.Trace as T

import Data.Ratio

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit


g = D.matrix $ S.symbol "G"
h = D.matrix $ S.symbol "H"

test_trace = T.trace expr @?= result where
    expr = D.expr2by2 (2 * g * h) h g (h * g)
    result = 2 * T.trace (g * h) + T.trace (h * g)

test_group = testGroup "Trace" [
    testCase "trace" test_trace
    ]