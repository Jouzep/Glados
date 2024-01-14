module VMTests.VmOperatorAddTest (vmTestOperatorAdd) where
import Test.HUnit
import VirtualMachine.VMConstants
import VirtualMachine.StackMachine

vmTestOperatorAdd :: Test
vmTestOperatorAdd = TestList [additionTest1, additionTest2, additionTest3, additionTest4, additionTest5]

-- Test case 1: Simple addition
additionTest1 :: Test
additionTest1 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Addition Test #1" expected result
  where
    program = [Push (IntVal 2), Push (IntVal 3), Push (OpVal Add1), Call, Ret]
    expected = Right (IntVal 5)

-- Test case 2: Addition with negative numbers
additionTest2 :: Test
additionTest2 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Addition Test #2" expected result
  where
    program = [Push (IntVal (-5)), Push (IntVal 8), Push (OpVal Add1), Call, Ret]
    expected = Right (IntVal 3)

-- Test case 3: Addition with zero
additionTest3 :: Test
additionTest3 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Addition Test #3" expected result
  where
    program = [Push (IntVal 0), Push (IntVal 7), Push (OpVal Add1), Call, Ret]
    expected = Right (IntVal 7)

-- Test case 4: Addition with large numbers
additionTest4 :: Test
additionTest4 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Addition Test #4" expected result
  where
    program = [Push (IntVal 1000), Push (IntVal 2000), Push (OpVal Add1), Call, Ret]
    expected = Right (IntVal 3000)

-- Test case 5: Addition with a mix of positive and negative numbers
additionTest5 :: Test
additionTest5 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Addition Test #5" expected result
  where
    program = [Push (IntVal (-10)), Push (IntVal 15), Push (OpVal Add1), Call, Ret]
    expected = Right (IntVal 5)