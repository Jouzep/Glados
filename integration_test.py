import subprocess
import unittest

class Color:
    RED = '\033[91m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    PURPLE = '\033[95m'
    MAGENTA = '\033[96m'
    ORANGE = '\033[38;5;208m'
    END = '\033[0m'

def run_haskell_executable(input_data, Expected):
    try:
        result = subprocess.run(['./glados'], input=input_data, capture_output=True, text=True, check=True)
        assert result.stdout.strip().split("\n")[-1] == Expected
    except subprocess.CalledProcessError as e:
        # print(f"Error running Haskell executable: {e}")
        return f"{Color.RED}Failed{Color.END}", f"Error running Haskell executable: {e}"
    except AssertionError as e:
        # print(e)
        return f"{Color.RED}Failed{Color.END}", result.stdout.strip().split("\n")[-1]
    return f"{Color.GREEN}Success{Color.END}", result.stdout.strip().split("\n")[-1]

test_suites = [
    {"Name": "Simple define", "Input": "(define foo 21))", "Expected": 'ListOfAst [Define,Var (AstSymb "foo"),Var (AstInt 21)]'},
    {"Name": "Simple operation on define", "Input": "(define foo 21)\n(* foo 2)", "Expected": "Var (AstInt 42)"},
    {"Name": "Operation 1", "Input": "(+ 5 5)", "Expected": "Var (AstInt 10)"},
    {"Name": "Operation 2", "Input": "(* 5 5)", "Expected": "Var (AstInt 25)"},
    {"Name": "Operation 3", "Input": "(/ 5 5)", "Expected": "Var (AstInt 1)"},
    {"Name": "Operation 4", "Input": "(- 5 5)", "Expected": "Var (AstInt 0)"},
    {"Name": "Operation 5", "Input": "(% 5 5)", "Expected": "Var (AstInt 0)"},
    {"Name": "Lambda 1", "Input": "((lambda (a b) (+ a b)) 1 2)", "Expected": "Var (AstInt 3)"},
    {"Name": "Lamdba 2", "Input": "(define add(lambda (a b)(+ a b)))\n(add 3 4)", "Expected": "Var (AstInt 7)"},
    {"Name": "Function 1", "Input": "(define (add a b)(+ a b))\n(add 3 4)", "Expected": "Var (AstInt 7)"},
    {"Name": "Condition 1", "Input": "(if #t 1 2)", "Expected": "Var (AstInt 1)"},
    {"Name": "Condition 2", "Input": "(if #f 1 2)", "Expected": "Var (AstInt 2)"},
    {"Name": "Condition 3", "Input": "(define foo 42)\n(if (< foo 10)(* foo 3)(/ foo 2))", "Expected": "Var (AstInt 21)"},
    {"Name": "BuiltIn 1", "Input": "(+ (* 2 3) (/ 10 2))", "Expected": "Var (AstInt 11)"},
    {"Name": "Condition 1", "Input": "(if #t 1 2)", "Expected": "Var (AstInt 1)"},
    {"Name": "Condition 1", "Input": "(if #t 1 2)", "Expected": "Var (AstInt 1)"},
    
]
if __name__ == "__main__":
    failed = []
    for index, test in enumerate(test_suites):
        status, message = run_haskell_executable(test["Input"], test["Expected"])
        name = test["Name"]  # Corrected line
        print(f"[{status}] {name}: {message}")
        print(f"\t{test}")
        if (status == f"{Color.RED}Failed{Color.END}"):
            print(f"\tBut got '{message}'")
            failed.append(name)
    print(f"{Color.ORANGE}Total tests: {len(test_suites)}, {Color.END}{Color.GREEN}Passed: {len(test_suites) - len(failed)}, {Color.END}{Color.RED}Failed: {len(failed)}{Color.END}")
    print(f"{Color.RED}Failed tests: {failed}{Color.END}")
    if failed:
        exit(1)
    exit(0)