import qualified Wigner.Symbols as S
import qualified Wigner.DefineOpExpr as DO
import qualified Wigner.DefineFuncExpr as DF
import Wigner.Complex
import Wigner.Texable

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

a1 = DO.operatorIx S.a [S.ix_1]
a2 = DO.operatorIx S.a [S.ix_2]
b1 = DO.operatorIx S.b [S.ix_1]
b2 = DO.operatorIx S.b [S.ix_2]

test_group_terms = (expr1 + expr2) @?= result where
    expr1 = a1 * 2 + a2 * 3
    expr2 = b1 * 4 + a1 * 2
    result = b1 * 4 + a2 * 3 + a1 * 4

test_remove_zero_terms = (expr1 + expr2) @?= result where
    expr1 = a1 * 2 + a2 * 3
    expr2 = b1 * 4 - a1 * 2
    result = b1 * 4 + a2 * 3

tg_addition = testGroup "Addition" [
        testCase "group_terms" test_group_terms,
        testCase "remove_zero_terms" test_remove_zero_terms
    ]
tests = [tg_addition]

main = defaultMain tests
