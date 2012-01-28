import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import TestArithmetic
import TestOperators
import TestExpectations
import TestTransformations

tests = [
    TestArithmetic.test_group,
    TestOperators.test_group,
    TestExpectations.test_group,
    TestTransformations.test_group
    ]

main = defaultMain tests
