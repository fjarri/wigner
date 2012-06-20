module TestTrace(test_group) where

import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.Trace as T
import Wigner.Expression
import Wigner.Texable

import Data.Ratio

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit


g = D.matrix $ S.symbol "G"
h = D.matrix $ S.symbol "H"

test_trace = T.trace expr @?= result where
    expr = D.expr2by2 (2 * g * h) h g (h * g)
    result = 2 * T.trace (g * h) + T.trace (h * g)

test_2by2_operations = (showTex expr) @?= (showTex result) where
    m1 = D.expr2by2 g h (-h) g
    m2 = transpose m1
    tg = transpose g
    th = transpose h
    expr = m1 * m2 + 2 * m1
    result = D.expr2by2
        (g * tg + h * th + 2 * g)
        (h * tg - g * th + 2 * h)
        (g * th - h * tg - 2 * h)
        (g * tg + h * th + 2 * g)

test_group = testGroup "Trace" [
    testCase "trace" test_trace,
    testCase "2by2_operations" test_2by2_operations
    ]
