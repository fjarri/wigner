module TestOperators(test_group) where

import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import Wigner.Expression
import Wigner.OperatorAlgebra

import Data.Ratio

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

a1 = D.operatorIx S.a [S.ix_1]
da1 = dagger a1
a2 = D.operatorIx S.a [S.ix_2]
b1 = D.operatorIx S.b [S.ix_1]
b2 = D.operatorIx S.b [S.ix_2]
alpha = D.constant S.alpha
beta = D.constant S.beta


test_define_symmetric = expr1 @?= expr2 where
    expr1 = D.symmetric $ da1 ^ 2 * a1 ^ 2
    expr2 = D.symmetric $ a1 ^ 2 * da1 ^ 2

test_normal_to_normal = (toNormalProduct expr) @?= expr where
    expr = da1 ^ 2 * a1 ^ 2

test_symmetric_to_normal = (toNormalProduct expr) @?= result where
    expr = D.symmetric $ da1 ^ 2 * a1 ^ 2
    result = (da1 ^ 2 * a1 ^ 2 + da1 * a1 * da1 * a1 + da1 * a1 ^ 2 * da1 +
        a1 * da1 ^ 2 * a1 + a1 * da1 * a1 * da1 + a1 ^ 2 * da1 ^ 2) / 6

test_symmetric_to_symmetric = (toSymmetricProduct bosonicCommutationRelation expr) @?= expr where
    expr = D.symmetric $ da1 ^ 2 * a1 ^ 2

test_normal_to_symmetric = (toSymmetricProduct bosonicCommutationRelation expr) @?= result where
    expr = da1 * a1
    result = D.symmetric (da1 * a1) - D.one / 2

test_normal_to_symmetric_2 = (toSymmetricProduct bosonicCommutationRelation expr) @?= result where
    expr = da1 * da1 * a1 * a1
    result = D.symmetric (da1 ^ 2 * a1 ^ 2) -
        2 * D.symmetric (da1 * a1) + D.one / 2

test_normal_to_symmetric_3 = (toSymmetricProduct bosonicCommutationRelation expr) @?= result where
    expr = da1 * a1 * da1 * a1
    result = D.symmetric (da1 ^ 2 * a1 ^ 2) - D.symmetric (da1 * a1)

test_normal_to_symmetric_4 = (toSymmetricProduct bosonicCommutationRelation expr) @?= result where
    [j, k, l, m] = map (S.index . S.symbol) ["j", "k", "l", "m"]
    [aj, ak, al, am] = map (D.operatorIx S.a) [[j], [k], [l], [m]]
    daj = dagger aj
    dal = dagger al
    [delta_kl, delta_lm, delta_jm, delta_jk] =
        map makeIndexDelta [(k, l), (l, m), (j, m), (j, k)]

    expr = daj * ak * dal * am
    result = D.symmetric (daj * dal * ak * am +
        (delta_kl * daj * am - delta_lm * daj * ak - delta_jm * dal * ak - delta_jk * dal * am) / 2 +
        delta_jk * delta_lm / 4 - delta_jm * delta_kl / 4)


test_group = testGroup "Operators" [
    testCase "define_symmetric" test_define_symmetric,
    testCase "normal_to_normal" test_normal_to_normal,
    testCase "symmetric_to_normal" test_symmetric_to_normal,
    testCase "symmetric_to_symmetric" test_symmetric_to_symmetric,
    testCase "normal_to_symmetric" test_normal_to_symmetric,
    testCase "normal_to_symmetric_2" test_normal_to_symmetric_2,
    testCase "normal_to_symmetric_3" test_normal_to_symmetric_3,
    testCase "normal_to_symmetric_4" test_normal_to_symmetric_4
    ]
