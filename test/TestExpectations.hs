module TestExpectations(test_group) where

import qualified Wigner.Symbols as S
import qualified Wigner.DefineOpExpr as DO
import qualified Wigner.DefineFuncExpr as DF
import Wigner.Expression
import Wigner.Complex
import Wigner.Texable
import qualified Wigner.Expectations as E

import Data.Ratio

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

eval = E.evaluateExpectations E.wignerExpectation S.default_map

a1 = DO.operatorIx S.a [S.ix_1]
a2 = DO.operatorIx S.a [S.ix_2]
b1 = DO.operatorIx S.b [S.ix_1]
b2 = DO.operatorIx S.b [S.ix_2]

alpha1 = DF.constantIx S.alpha [S.ix_1]
alpha2 = DF.constantIx S.alpha [S.ix_2]
beta1 = DF.constantIx S.beta [S.ix_1]
beta2 = DF.constantIx S.beta [S.ix_2]

e_idt = S.symbol "e^{i \\Delta \\theta}"
e_m_idt = S.symbol "e^{-i \\Delta \\theta}"
cos_t = S.symbol "\\cos(\\theta)"
sin_t = S.symbol "\\sin(\\theta)"

op_e_idt = DO.constant e_idt
op_e_m_idt = DO.constant e_m_idt
op_cos_t = DO.constant cos_t
op_sin_t = DO.constant sin_t

jAX = ((dagger a2) * a1 * op_e_idt + (dagger a1) * a2 * op_e_m_idt) / 2
jAY = ((dagger a2) * a1 * op_e_idt - (dagger a1) * a2 * op_e_m_idt) / (2 * DO.i)
jAZ = ((dagger a2) * a2 - (dagger a1) * a1) / 2
jAT = op_cos_t * jAZ + op_sin_t * jAX
a2a1 = (dagger a2) * a1
a1a1 = (dagger a1) * a1

test_a2a1 = replaced_expectations @?= result where
    marked_expectations = E.expectation a2a1
    replaced_expectations = eval marked_expectations
    result = DF.expectation (conjugate alpha2 * alpha1)

test_a1a1 = replaced_expectations @?= result where
    marked_expectations = E.expectation a1a1
    replaced_expectations = eval marked_expectations
    result = DF.expectation (conjugate alpha1 * alpha1) - DF.one / 2

test_deltaSquared_jAZ = showTex replaced_expectations @?= showTex result where

    marked_expectations = E.deltaSquared jAZ
    replaced_expectations = eval marked_expectations

    result = (
        -DF.one / 2 + -- differs from Python version
        DF.expectation (alpha1 ^ 2 * (conjugate alpha1) ^ 2) +
        DF.expectation (alpha2 ^ 2 * (conjugate alpha2) ^ 2) -
        2 * DF.expectation (alpha1 * conjugate alpha1 * alpha2 * conjugate alpha2) -
        (DF.expectation (alpha2 * conjugate alpha2) -
        DF.expectation (alpha1 * conjugate alpha1)) ^ 2) / 4

test_deltaSquared_jAX = showTex replaced_expectations @?= showTex result where
    f_e_idt = DF.constant e_idt
    f_e_m_idt = DF.constant e_m_idt
    f_e_2idt = f_e_idt ^ 2
    f_e_2m_idt = f_e_m_idt ^ 2

    marked_expectations = E.deltaSquared jAX
    replaced_expectations = eval marked_expectations

    result = (
        (DF.expectation (alpha1 ^ 2 * (conjugate alpha2) ^ 2) * f_e_2idt +
        DF.expectation ((conjugate alpha1) ^ 2 * alpha2 ^ 2) * f_e_2m_idt) / 2 +
        DF.expectation (conjugate alpha1 * alpha1 * conjugate alpha2 * alpha2)) / 2 -
        ((DF.expectation (alpha1 * conjugate alpha2) * f_e_idt +
        DF.expectation (conjugate alpha1 * alpha2) * f_e_m_idt) / 2 ) ^ 2


test_group = testGroup "Expectations" [
        testCase "deltaSquared_jAX" test_deltaSquared_jAX,
        testCase "deltaSquared_jAZ" test_deltaSquared_jAZ,
        testCase "a2a1" test_a2a1,
        testCase "a1a1" test_a1a1
    ]
