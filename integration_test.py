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
    {"Name": "Define Function, call, and return", "Input": "test1", "Expected": '1'},
    {"Name": "Define Function, call, and return operation", "Input": "test2", "Expected": '2'},
    {"Name": "Define Function, call, and return multiple operations", "Input": "test3", "Expected": '4'},

]

def read_file(file_path):
    try:
        with open(f"test/FunctionnalTests/{file_path}", 'r') as file:
            content = file.read()
            return content
    except:
        return "File not found"

if __name__ == "__main__":
    failed = []
    for index, test in enumerate(test_suites):

        content = read_file(test["Input"])
        if (content == "File not found"):
            print(f"[{Color.RED}Failed{Color.END}]: File not found {test['Input']}")
            failed.append(name)
            continue
        status, message = run_haskell_executable(content, test["Expected"])
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