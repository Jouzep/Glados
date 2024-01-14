module VMTests.VmOperatorMultiplyTest (vmTestOperatorMultiply) where
import Test.HUnit
import VirtualMachine.VMConstants
import VirtualMachine.StackMachine

vmTestOperatorMultiply :: Test
vmTestOperatorMultiply = TestList [multiplicationTest1, multiplicationTest2, multiplicationTest3, multiplicationTest4, multiplicationTest5]

-- Test case 1: Simple multiplication
multiplicationTest1 :: Test
multiplicationTest1 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Multiplication Test #1" expected result
  where
    program = [Push (IntVal 3), Push (IntVal 4), Push (OpVal Multiply1), Call, Ret]
    expected = Right (IntVal 12)

-- Test case 2: Multiplication with zero
multiplicationTest2 :: Test
multiplicationTest2 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Multiplication Test #2" expected result
  where
    program = [Push (IntVal 7), Push (IntVal 0), Push (OpVal Multiply1), Call, Ret]
    expected = Right (IntVal 0)

-- Test case 3: Multiplication with negative numbers
multiplicationTest3 :: Test
multiplicationTest3 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Multiplication Test #3" expected result
  where
    program = [Push (IntVal (-5)), Push (IntVal 8), Push (OpVal Multiply1), Call, Ret]
    expected = Right (IntVal (-40))

-- Test case 4: Multiplication with large numbers
multiplicationTest4 :: Test
multiplicationTest4 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Multiplication Test #4" expected result
  where
    program = [Push (IntVal 100), Push (IntVal 200), Push (OpVal Multiply1), Call, Ret]
    expected = Right (IntVal 20000)

-- Test case 5: Multiplication with a mix of positive and negative numbers
multiplicationTest5 :: Test
multiplicationTest5 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Multiplication Test #5" expected result
  where
    program = [Push (IntVal (-3)), Push (IntVal 5), Push (OpVal Multiply1), Call, Ret]
    expected = Right (IntVal (-15))
