module TestExpectations(test_group) where

import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.Expectations as E
import Wigner.Expression
import Wigner.Complex
import Wigner.Texable

import Data.Ratio

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

eval = E.evaluateExpectations E.wignerExpectation S.default_map

a1 = D.operatorIx S.a [S.ix_1]
a2 = D.operatorIx S.a [S.ix_2]
b1 = D.operatorIx S.b [S.ix_1]
b2 = D.operatorIx S.b [S.ix_2]

alpha1 = D.constantIx S.alpha [S.ix_1]
alpha2 = D.constantIx S.alpha [S.ix_2]
beta1 = D.constantIx S.beta [S.ix_1]
beta2 = D.constantIx S.beta [S.ix_2]

e_idt = D.constant $ S.symbol "(e^{i \\Delta \\theta})"
e_m_idt = D.constant $ S.symbol "(e^{-i \\Delta \\theta})"
cos_t = D.constant $ S.symbol "\\cos(\\theta)"
sin_t = D.constant $ S.symbol "\\sin(\\theta)"

jAX = ((dagger a2) * a1 * e_idt + (dagger a1) * a2 * e_m_idt) / 2
jAY = ((dagger a2) * a1 * e_idt - (dagger a1) * a2 * e_m_idt) / (2 * D.i)
jAZ = ((dagger a2) * a2 - (dagger a1) * a1) / 2
jAT = cos_t * jAZ + sin_t * jAX
a2a1 = (dagger a2) * a1
a1a1 = (dagger a1) * a1

test_a2a1 = replaced_expectations @?= result where
    marked_expectations = E.expectation a2a1
    replaced_expectations = eval marked_expectations
    result = E.asExpectation (conjugate alpha2 * alpha1)

test_a1a1 = replaced_expectations @?= result where
    marked_expectations = E.expectation a1a1
    replaced_expectations = eval marked_expectations
    result = E.asExpectation (conjugate alpha1 * alpha1) - D.one / 2

test_deltaSquared_jAZ = showTex replaced_expectations @?= showTex result where

    marked_expectations = E.deltaSquared jAZ
    replaced_expectations = eval marked_expectations

    result = (
        -D.one / 2 + -- differs from Python version
        E.asExpectation (alpha1 ^ 2 * (conjugate alpha1) ^ 2) +
        E.asExpectation (alpha2 ^ 2 * (conjugate alpha2) ^ 2) -
        2 * E.asExpectation (alpha1 * conjugate alpha1 * alpha2 * conjugate alpha2) -
        (E.asExpectation (alpha2 * conjugate alpha2) -
        E.asExpectation (alpha1 * conjugate alpha1)) ^ 2) / 4

test_deltaSquared_jAX = showTex replaced_expectations @?= showTex result where

    marked_expectations = E.deltaSquared jAX
    replaced_expectations = eval marked_expectations

    result =
        (
            -D.one / 4 * e_idt * e_m_idt + -- differs from Python version
            (
                E.asExpectation (alpha1 ^ 2 * (conjugate alpha2) ^ 2) * e_idt ^ 2 +
                E.asExpectation ((conjugate alpha1) ^ 2 * alpha2 ^ 2) * e_m_idt ^ 2
            ) / 2 +
            E.asExpectation (conjugate alpha1 * alpha1 * conjugate alpha2 * alpha2) * e_idt * e_m_idt
        ) / 2 -
        ((E.asExpectation (alpha1 * conjugate alpha2) * e_idt +
        E.asExpectation (conjugate alpha1 * alpha2) * e_m_idt) / 2 ) ^ 2


test_group = testGroup "Expectations" [
        testCase "deltaSquared_jAX" test_deltaSquared_jAX,
        testCase "deltaSquared_jAZ" test_deltaSquared_jAZ,
        testCase "a2a1" test_a2a1,
        testCase "a1a1" test_a1a1
    ]
