module VMTests.VmOperatorEqTest (vmTestOperatorEq) where
import Test.HUnit
import VirtualMachine.VMConstants
import VirtualMachine.StackMachine

vmTestOperatorEq :: Test
vmTestOperatorEq = TestList [eqTest1, eqTest2, eqTest3, eqTest4, eqTest5]

-- Test case 1: Equality with equal numbers
eqTest1 :: Test
eqTest1 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Equality Test #1" expected result
  where
    program = [Push (IntVal 5), Push (IntVal 5), Push (OpVal Eq1), Call, Ret]
    expected = Right (BoolVal True)

-- Test case 2: Equality with different numbers
eqTest2 :: Test
eqTest2 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Equality Test #2" expected result
  where
    program = [Push (IntVal 10), Push (IntVal 3), Push (OpVal Eq1), Call, Ret]
    expected = Right (BoolVal False)

-- Test case 3: Equality with negative numbers
eqTest3 :: Test
eqTest3 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Equality Test #3" expected result
  where
    program = [Push (IntVal (-5)), Push (IntVal (-5)), Push (OpVal Eq1), Call, Ret]
    expected = Right (BoolVal True)

-- Test case 4: Equality with large numbers
eqTest4 :: Test
eqTest4 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Equality Test #4" expected result
  where
    program = [Push (IntVal 1000), Push (IntVal 1000), Push (OpVal Eq1), Call, Ret]
    expected = Right (BoolVal True)

-- Test case 5: Equality with a mix of positive and negative numbers
eqTest5 :: Test
eqTest5 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Equality Test #5" expected result
  where
    program = [Push (IntVal (-3)), Push (IntVal 3), Push (OpVal Eq1), Call, Ret]
    expected = Right (BoolVal False)
