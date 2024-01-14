module VMTests.Index (vmTestList) where
import Test.HUnit


import VMTests.VmOperatorAddTest
import VMTests.VmOperatorDivideTest
import VMTests.VmOperatorEqTest
import VMTests.VmOperatorLessTest
import VMTests.VmOperatorMultiplyTest
import VMTests.VmOperatorSubtractTest
import VMTests.VmConditionTest
import VMTests.VmEnvTest
import VMTests.VmFunctionTest

vmTestList :: Test
vmTestList = 
    TestList
    [
        vmTestOperatorAdd,
        vmTestOperatorDivide,
        vmTestOperatorEq,
        vmTestOperatorLess,
        vmTestOperatorMultiply,
        vmTestOperatorSubtract,
        vmTestCondition,
        vmTestEnvironment,
        vmTestFunction
    ]

