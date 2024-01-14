module VMTests.VmOperatorDivideTest (vmTestOperatorDivide) where
import Test.HUnit
import VirtualMachine.VMConstants
import VirtualMachine.StackMachine

vmTestOperatorDivide :: Test
vmTestOperatorDivide = TestList [divisionTest1, divisionTest2, divisionTest3, divisionTest4, divisionTest5]

-- Test case 1: Simple division
divisionTest1 :: Test
divisionTest1 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Division Test #1" expected result
  where
    program = [Push (IntVal 15), Push (IntVal 15), Push (OpVal Divide1), Call, Ret]
    expected = Right (IntVal 1)

-- Test case 2: Division by zero
divisionTest2 :: Test
divisionTest2 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Division Test #2" expected result
  where
    program = [Push (IntVal 0), Push (IntVal 1), Push (OpVal Divide1), Call, Ret]
    expected = Left "Error: division by zero"

-- Test case 3: Division with negative numbers
divisionTest3 :: Test
divisionTest3 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Division Test #3" expected result
  where
    program = [ Push (IntVal 4),Push (IntVal (-20)), Push (OpVal Divide1), Call, Ret]
    expected = Right (IntVal (-5))

-- Test case 4: Division with large numbers
divisionTest4 :: Test
divisionTest4 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Division Test #4" expected result
  where
    program = [Push (IntVal 200),Push (IntVal 1000), Push (OpVal Divide1), Call, Ret]
    expected = Right (IntVal 5)

-- Test case 5: Division with a mix of positive and negative numbers
divisionTest5 :: Test
divisionTest5 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Division Test #5" expected result
  where
    program = [Push (IntVal 3), Push (IntVal (-15)),Push (OpVal Divide1), Call, Ret]
    expected = Right (IntVal (-5))