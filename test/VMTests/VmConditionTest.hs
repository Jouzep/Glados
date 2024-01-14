module VMTests.VmConditionTest (vmTestCondition) where
import Test.HUnit
import VirtualMachine.VMConstants
import VirtualMachine.StackMachine

vmTestCondition :: Test
vmTestCondition = TestList [conditionTest1, conditionTest2, conditionTest3, conditionTest4, conditionTest5]

-- Test case 1: JumpIfFalse with true condition
conditionTest1 :: Test
conditionTest1 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Condition Test #1" expected result
  where
    program = [Push (BoolVal True),  (JumpIfFalse (IntVal 5)), Push (IntVal 10), Ret]
    expected = Right (IntVal 10)  -- Inverted expected value

-- Test case 2: JumpIfFalse with false condition
conditionTest2 :: Test
conditionTest2 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Condition Test #2" expected result
  where
    program = [Push (BoolVal False),  (JumpIfFalse (IntVal 5)), Push (IntVal 20), Ret, Ret, Ret, Ret, Push(IntVal 30), Ret]
    expected = Right (IntVal 30) 

-- Test case 3: JumpIfFalse with negative number condition
conditionTest3 :: Test
conditionTest3 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Condition Test #3" expected result
  where
    program = [Push (BoolVal False),  (JumpIfFalse (IntVal 3)), Push (IntVal 30), Ret, Ret, Push (IntVal 31), Ret]
    expected = Right (IntVal 31) 

-- Test case 4: JumpIfFalse with zero condition
conditionTest4 :: Test
conditionTest4 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Condition Test #4" expected result
  where
    program = [Push (BoolVal False),  (JumpIfFalse (IntVal 2)), Push (IntVal 40), Ret, Push (IntVal 20), Ret]
    expected = Right (IntVal 20) 

-- Test case 5: JumpIfFalse with mix of conditions
conditionTest5 :: Test
conditionTest5 = TestCase $ do
  let result = exec [] [] program []
  assertEqual "Condition Test #5" expected result
  where
    program = [Push (BoolVal True),  (JumpIfFalse (IntVal 4)), Push (IntVal 50), Ret]
    expected = Right (IntVal 50) 
