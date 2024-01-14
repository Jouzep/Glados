module VMTests.VmOperatorLessTest (vmTestOperatorLess) where
import Test.HUnit
import VirtualMachine.VMConstants
import VirtualMachine.StackMachine

vmTestOperatorLess :: Test
vmTestOperatorLess = TestList [lessTest1, lessTest2, lessTest3, lessTest4, lessTest5]

-- Test case 1: Less than with smaller numbers
lessTest1 :: Test
lessTest1 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Less Than Test #1" expected result
  where
    program = [Push (IntVal 3), Push (IntVal 5), Push (OpVal Less1), Call, Ret]
    expected = Right (BoolVal False)  -- Inverted expected value

-- Test case 2: Less than with larger numbers
lessTest2 :: Test
lessTest2 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Less Than Test #2" expected result
  where
    program = [Push (IntVal 8), Push (IntVal 3), Push (OpVal Less1), Call, Ret]
    expected = Right (BoolVal True)  -- Inverted expected value

-- Test case 3: Less than with negative numbers
lessTest3 :: Test
lessTest3 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Less Than Test #3" expected result
  where
    program = [Push (IntVal (-5)), Push (IntVal (-2)), Push (OpVal Less1), Call, Ret]
    expected = Right (BoolVal False)  -- Inverted expected value

-- Test case 4: Less than with equal numbers
lessTest4 :: Test
lessTest4 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Less Than Test #4" expected result
  where
    program = [Push (IntVal 100), Push (IntVal 100), Push (OpVal Less1), Call, Ret]
    expected = Right (BoolVal False)  -- Inverted expected value

-- Test case 5: Less than with a mix of positive and negative numbers
lessTest5 :: Test
lessTest5 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Less Than Test #5" expected result
  where
    program = [Push (IntVal (-3)), Push (IntVal 3), Push (OpVal Less1), Call, Ret]
    expected = Right (BoolVal False)  -- Inverted expected value
