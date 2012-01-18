import qualified Wigner.Symbols as S
import qualified Wigner.DefineOpExpr as DO
import qualified Wigner.DefineFuncExpr as DF
import Wigner.Texable

import Data.Ratio

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

a1 = DO.operatorIx S.a [S.ix_1]
a2 = DO.operatorIx S.a [S.ix_2]
b1 = DO.operatorIx S.b [S.ix_1]
b2 = DO.operatorIx S.b [S.ix_2]
alpha = DO.constant S.alpha
beta = DO.constant S.beta

test_group_terms = (expr1 + expr2) @?= result where
    expr1 = alpha * a1 * (2 - DO.i) + a2 * 3
    expr2 = b1 * 4 + alpha * a1 * (1 + 2 * DO.i)
    result = b1 * 4 + a2 * 3 + alpha * a1 * (3 + DO.i)

test_remove_zero_terms = (expr1 + expr2 + expr3) @?= result where
    expr1 = beta * a1 * (fromRational (2 % 3) - DO.i / 2) + a2 * 3
    expr2 = b1 * 4 - beta * a1 * (fromRational (-2 % 3) + DO.i / 2)
    expr3 = b2 * 4 - beta * a1 * (fromRational (4 % 3) - DO.i) - 2 * alpha * a1
    result = b1 * 4 + a2 * 3 + b2 * 4 - 2 * alpha * a1

test_zero_result = (expr1 + expr2) @?= 0 where
    expr1 = 2 * a1 - 2 * b1
    expr2 = 2 * b1 - 2 * a1

test_operator_exponentiation = expr1 * expr2 @?= result where
    expr1 = 2 * a1 * a1 * a2
    expr2 = 3 * a2 * a2 * a1
    result = 6 * a1^2 * a2^3 * a1


tg_addition = testGroup "Addition" [
        testCase "group_terms" test_group_terms,
        testCase "remove_zero_terms" test_remove_zero_terms,
        testCase "zero_result" test_zero_result,
        testCase "operator_exponentiation" test_operator_exponentiation
    ]
tests = [tg_addition]

main = defaultMain tests
