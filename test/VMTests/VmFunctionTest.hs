module VMTests.VmFunctionTest (vmTestFunction) where
import Test.HUnit
import VirtualMachine.VMConstants
import VirtualMachine.StackMachine

vmTestFunction :: Test
vmTestFunction = TestList [functionCallTest1, functionCallTest2, functionCallTest3, functionCallTest4, functionCallTest5, functionCallTest6, functionCallTest7]

-- Test case 1: Function call with arguments
functionCallTest1 :: Test
functionCallTest1 = TestCase $ do
  let result = exec env args program []
  assertEqual "Function Call Test #1" expected result
  where
    program = [Push (FuncVal [PushEnv "x", PushEnv "y", Push (OpVal Add1),Call, Ret]), Call, Ret]
    expected = Right (IntVal 30)
    env = [("x", Push (IntVal 10)), ("y", Push (IntVal 20))]
    args = []

functionCallTest2 :: Test
functionCallTest2 = TestCase $ do
  let result = exec [] args program []
  assertEqual "Function Call Test #2" expected result
  where
    program = [Push (IntVal 5), Push (IntVal 10),Push (FuncVal [PushArg 0, PushArg 0, Push (OpVal Add1),Call, Ret]), Call, Ret]
    expected = Right (IntVal 20)
    args = []

functionCallTest3 :: Test
functionCallTest3 = TestCase $ do
  let result = exec [] args program []
  assertEqual "Function Call Test #3" expected result
  where
    program = [Push (IntVal 5), Push (IntVal 10),Push (FuncVal [PushArg 1, PushArg 0, Push (OpVal Add1),Call, Ret]), Call, Ret]
    expected = Right (IntVal 15)
    args = []

functionCallTest4 :: Test
functionCallTest4 = TestCase $ do
  let result = exec [] args program []
  assertEqual "Function Call Test #4" expected result
  where
    program = [Push (IntVal 5), Push (IntVal 10),Push (FuncVal [PushArg 1, PushArg 1, Push (OpVal Add1),Call, Ret]), Call, Ret]
    expected = Right (IntVal 10)
    args = []

functionCallTest5 :: Test
functionCallTest5 = TestCase $ do
  let result = exec [] args program []
  assertEqual "Function Call Test #5" expected result
  where
    program = [PushArg 0, Push (IntVal 10), Push (OpVal Add1),Call, Ret]
    expected = Right (IntVal 15)
    args = [(IntVal 5), (IntVal 50), (IntVal(-5))]

functionCallTest6 :: Test
functionCallTest6 = TestCase $ do
  let result = exec [] args program []
  assertEqual "Function Call Test #6" expected result
  where
    program = [PushArg 1, Push (IntVal 10), Push (OpVal Add1),Call, Ret]
    expected = Right (IntVal 60)
    args = [(IntVal 5), (IntVal 50), (IntVal(-5))]

functionCallTest7 :: Test
functionCallTest7 = TestCase $ do
  let result = exec [] args program []
  assertEqual "Function Call Test #7" expected result
  where
    program = [PushArg 2, Push (IntVal 10), Push (OpVal Add1),Call, Ret]
    expected = Right (IntVal 5)
    args = [(IntVal 5), (IntVal 50), (IntVal(-5))]