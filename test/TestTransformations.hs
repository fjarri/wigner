module TestTransformations(test_group) where

import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.Transformations as T
import Wigner.Expression
import Wigner.Complex
import Wigner.Texable
import Wigner.Deltas

import Data.Ratio
import qualified Data.Map as M

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit


s_psi = S.symbol "\\Psi"
corr = M.fromList [(s_psi, s_psi)]

s_rho = S.symbol "\\rho"
rho = D.operator s_rho

x = Func (Element S.x [] [])
x' = Func (Element S.x' [] [])

types_list = [(S.ix_j, x), (S.ix_k, x), (S.ix_j, x'), (S.ix_k, x'),
    (S.ix_1, x), (S.ix_2, x)]
factory f (i, v) = f s_psi [i] [v]

[psi_j, psi_k, psi_j', psi_k', psi1, psi2] = map (factory D.operatorFuncIx) types_list
[f_psi_j, f_psi_k, f_psi_j', f_psi_k', f_psi1, f_psi2] = map (factory D.functionIx) types_list
[d_psi_j, d_psi_k, d_psi_j', d_psi_k', d_psi1, d_psi2] = map (factory D.differentialFuncIx) types_list

djk = makeIndexDelta (S.ix_j, S.ix_k)
dxx = makeVariableDelta (x, x)
dxx' = makeVariableDelta (x, x')
cdxx' = makeVariableDelta (x', x)
dxx'' = makeVariableDelta (x', x')

commutator x y = x * y - y * x
lossTerm x = 2 * x * rho * dagger x - dagger x * x * rho - rho * dagger x * x
transform = T.wignerTransformation corr s_rho


test_linear = fpe @?= result where
    hamiltonian = dagger psi_j * psi_k
    fpe = transform $ commutator hamiltonian rho
    result = -d_psi_j * f_psi_k + conjugate (d_psi_k * f_psi_j)

nonlinear_low_order = d_psi_j * (
        -f_psi_j' * f_psi_k * conjugate f_psi_k' +
        djk * dxx'' * f_psi_k / 2 +
        dxx' * f_psi_j' / 2
    ) +
    conjugate d_psi_j' * (
        conjugate f_psi_j * f_psi_k * conjugate f_psi_k' -
        djk * dxx * conjugate f_psi_k' / 2 -
        dxx' * conjugate f_psi_j / 2
    ) +
    d_psi_k' * (
        -f_psi_j' * conjugate f_psi_j * f_psi_k +
        djk * dxx * f_psi_j' / 2 +
        cdxx' * f_psi_k / 2
    ) +
    conjugate d_psi_k * (
        f_psi_j' * conjugate f_psi_j * conjugate f_psi_k' -
        djk * dxx'' * conjugate f_psi_j / 2 -
        cdxx' * conjugate f_psi_k' / 2
    )
nonlinear_high_order = d_psi_j * conjugate d_psi_j' * d_psi_k' * f_psi_k / 4 -
    d_psi_j * conjugate d_psi_j' * conjugate d_psi_k * conjugate f_psi_k' / 4 +
    d_psi_k' * conjugate d_psi_k * d_psi_j * f_psi_j' / 4 -
    d_psi_k' * conjugate d_psi_k * conjugate d_psi_j' * conjugate f_psi_j / 4

test_nonlinear = fpe @?= nonlinear_low_order + nonlinear_high_order where
    hamiltonian = dagger psi_j * dagger psi_k' * psi_j' * psi_k
    fpe = transform $ commutator hamiltonian rho

test_nonlinear_truncated = fpe @?= nonlinear_low_order where
    hamiltonian = dagger psi_j * dagger psi_k' * psi_j' * psi_k
    fpe = T.truncateDifferentials 2 (transform $ commutator hamiltonian rho)

analytical_tests = map createAnalyticalTest [
    ("psi1", psi1), ("psi1^2", psi1 ^ 2),
    ("psi1 * psi2", psi1 * psi2), ("psi1^3", psi1 ^ 3)]
createAnalyticalTest (name, expr) = testCase name tc_func where
    calculated = transform $ lossTerm expr
    analytical = T.wignerOfLossTerm corr expr
    tc_func = calculated @?= analytical

test_group = testGroup "Expectations" [
        testCase "linear" test_linear,
        testCase "nonlinear" test_nonlinear,
        testCase "nonlinear_truncated" test_nonlinear_truncated,
        testGroup "Analytical formula for loss terms" analytical_tests
    ]
