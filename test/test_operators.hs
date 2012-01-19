import qualified Wigner.Symbols as S
import qualified Wigner.DefineOpExpr as DO
import Wigner.Expression
import Wigner.OperatorAlgebra

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


test_symmetric_to_normal = (toNormalProduct (asSymmetricProduct expr)) @?= result where
    da1 = dagger a1
    expr = da1 ^ 2 * a1 ^ 2
    result = (da1 ^ 2 * a1 ^ 2 + da1 * a1 * da1 * a1 + da1 * a1 ^ 2 * da1 +
        a1 * da1 ^ 2 * a1 + a1 * da1 * a1 * da1 + a1 ^ 2 * da1 ^ 2) / 6



tg_operators = testGroup "Operator Algebra" [
        testCase "symmetric_to_normal" test_symmetric_to_normal
    ]
tests = [tg_operators]

main = defaultMain tests
