module VMTests.VmOperatorSubtractTest (vmTestOperatorSubtract) where
import Test.HUnit
import VirtualMachine.VMConstants
import VirtualMachine.StackMachine

vmTestOperatorSubtract :: Test
vmTestOperatorSubtract = TestList [subtractionTest1, subtractionTest2, subtractionTest3, subtractionTest4, subtractionTest5]

-- Test case 1: Simple subtraction
subtractionTest1 :: Test
subtractionTest1 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Subtraction Test #1" expected result
  where
    program = [Push (IntVal 8), Push (IntVal 3), Push (OpVal Subtract1), Call, Ret]
    expected = Right (IntVal (-5))

-- Test case 2: Subtraction with negative numbers
subtractionTest2 :: Test
subtractionTest2 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Subtraction Test #2" expected result
  where
    program = [Push (IntVal (-5)), Push (IntVal 8), Push (OpVal Subtract1), Call, Ret]
    expected = Right (IntVal (13))

-- Test case 3: Subtraction with zero
subtractionTest3 :: Test
subtractionTest3 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Subtraction Test #3" expected result
  where
    program = [Push (IntVal 7), Push (IntVal 0), Push (OpVal Subtract1), Call, Ret]
    expected = Right (IntVal (-7))

-- Test case 4: Subtraction with large numbers
subtractionTest4 :: Test
subtractionTest4 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Subtraction Test #4" expected result
  where
    program = [Push (IntVal 2000), Push (IntVal 1000), Push (OpVal Subtract1), Call, Ret]
    expected = Right (IntVal (-1000))

-- Test case 5: Subtraction with a mix of positive and negative numbers
subtractionTest5 :: Test
subtractionTest5 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Subtraction Test #5" expected result
  where
    program = [Push (IntVal (-10)), Push (IntVal 15), Push (OpVal Subtract1), Call, Ret]
    expected = Right (IntVal (25))
