module VMTests.VmEnvTest (vmTestEnvironment) where
import Test.HUnit
import VirtualMachine.VMConstants
import VirtualMachine.StackMachine

vmTestEnvironment :: Test
vmTestEnvironment = TestList [envTest1, envTest2, envTest3, envTest4, envTest5]

-- Test case 1: Accessing environment variable "y"
envTest1 :: Test
envTest1 = TestCase $ do
  let result = exec env [] program []
  assertEqual "Environment Test #1" expected result
  where
    program = [PushEnv "y", Ret]
    expected = Right (IntVal 20)
    env = [("x", Push (IntVal 10)), ("y", Push (IntVal 20))]

-- Test case 2: Accessing environment variable "x"
envTest2 :: Test
envTest2 = TestCase $ do
  let result = exec env [] program []
  assertEqual "Environment Test #2" expected result
  where
    program = [PushEnv "x", Ret]
    expected = Right (IntVal 10)
    env = [("x", Push (IntVal 10)), ("y", Push (IntVal 20))]

-- Test case 3: Accessing environment variable "z" (not present)
envTest3 :: Test
envTest3 = TestCase $ do
  let result = exec env [] program []
  assertEqual "Environment Test #3" expected result
  where
    program = [PushEnv "z", Ret]
    expected = Left "Function z not found"
    env = [("x", Push (IntVal 10)), ("y", Push (IntVal 20))]

-- Test case 4: Accessing environment variable with complex value
envTest4 :: Test
envTest4 = TestCase $ do
  let result = exec env [] program []
  assertEqual "Environment Test #4" expected result
  where
    program = [PushEnv "func", Ret]
    expected = Right (FuncVal [Push (IntVal 5), Push (IntVal 10), Push (OpVal Add1), Ret])
    env = [("func", Push (FuncVal [Push (IntVal 5), Push (IntVal 10), Push (OpVal Add1), Ret]))]

-- Test case 5: Accessing environment variable with operator value
envTest5 :: Test
envTest5 = TestCase $ do
  let result = exec env [] program []
  assertEqual "Environment Test #5" expected result
  where
    program = [PushEnv "x", PushEnv "y", Push (OpVal Add1), Call, Ret]
    expected = Right (IntVal 30)
    env = [("x", Push (IntVal 10)), ("y", Push (IntVal 20))]
