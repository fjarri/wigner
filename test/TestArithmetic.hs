module TestArithmetic(test_group) where

import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D

import Data.Ratio

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

a1 = D.operatorIx S.a [S.ix_1]
a2 = D.operatorIx S.a [S.ix_2]
b1 = D.operatorIx S.b [S.ix_1]
b2 = D.operatorIx S.b [S.ix_2]
alpha = D.constant S.alpha
beta = D.constant S.beta
dalpha = D.differential S.alpha


test_group_terms = (expr1 + expr2) @?= result where
    expr1 = alpha * a1 * (2 - D.i) + a2 * 3
    expr2 = b1 * 4 + alpha * a1 * (1 + 2 * D.i)
    result = b1 * 4 + a2 * 3 + alpha * a1 * (3 + D.i)

test_remove_zero_terms = (expr1 + expr2 + expr3) @?= result where
    expr1 = beta * a1 * (fromRational (2 % 3) - D.i / 2) + a2 * 3
    expr2 = b1 * 4 - beta * a1 * (fromRational (-2 % 3) + D.i / 2)
    expr3 = b2 * 4 - beta * a1 * (fromRational (4 % 3) - D.i) - 2 * alpha * a1
    result = b1 * 4 + a2 * 3 + b2 * 4 - 2 * alpha * a1

test_zero_result = (expr1 + expr2) @?= 0 where
    expr1 = 2 * a1 - 2 * b1
    expr2 = 2 * b1 - 2 * a1

test_operator_exponentiation = expr1 * expr2 @?= result where
    expr1 = 2 * a1 * a1 * a2
    expr2 = 3 * a2 * a2 * a1
    result = 6 * a1^2 * a2^3 * a1

test_combine_differentials = expr1 * expr2 @?= result where
    expr1 = alpha * beta * dalpha
    expr2 = dalpha ^ 2 * beta
    result = alpha * beta * (dalpha ^ 3) * beta


test_group = testGroup "Addition" [
    testCase "group_terms" test_group_terms,
    testCase "remove_zero_terms" test_remove_zero_terms,
    testCase "zero_result" test_zero_result,
    testCase "operator_exponentiation" test_operator_exponentiation,
    testCase "combine_differentials" test_combine_differentials
    ]
