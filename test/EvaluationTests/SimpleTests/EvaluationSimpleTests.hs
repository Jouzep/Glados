module EvaluationTests.SimpleTests.EvaluationSimpleTests (evalSimpleTestList) where
import Test.HUnit

import EvaluationTests.SimpleTests.EvaluationSimpleAdd
import EvaluationTests.SimpleTests.EvaluationSimpleMultiply
import EvaluationTests.SimpleTests.EvaluationSimpleSub
import EvaluationTests.SimpleTests.EvaluationSimpleDivide
import EvaluationTests.SimpleTests.EvaluationSimpleModulo
import EvaluationTests.SimpleTests.EvaluationSimpleEqual
import EvaluationTests.SimpleTests.EvaluationSimpleNotEqual
import EvaluationTests.SimpleTests.EvaluationSimpleLessThan
import EvaluationTests.SimpleTests.EvaluationSimpleLessThanOrEqual
import EvaluationTests.SimpleTests.EvaluationSimpleGreaterThan
import EvaluationTests.SimpleTests.EvaluationSimpleGreaterThanOrEqual
import EvaluationTests.SimpleTests.EvaluationSimpleAnd
import EvaluationTests.SimpleTests.EvaluationSimpleOr
import EvaluationTests.SimpleTests.EvaluationSimpleCond

evalSimpleTestList :: Test
evalSimpleTestList = 
    TestList 
    [
        evalSimpleAddTests,
        evalSimpleAndTests,
        evalSimpleDivideTests,
        evalSimpleEqualTests,
        evalSimpleGreaterThanTests,
        evalSimpleGreaterThanOrEqualTests,
        evalSimpleLessThanOrEqualTests,
        evalSimpleLessThanTests,
        evalSimpleModuloTests,
        evalSimpleMultiplyTests,
        evalSimpleNotEqualTests,
        evalSimpleOrTests,
        evalSimpleSubTests,
        evalSimpleCondTests
    ]