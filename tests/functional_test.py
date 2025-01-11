#!/usr/bin/python3

import os
import sys
import subprocess

Makefile_path = os.path.join(os.path.dirname(__file__), '../Makefile')
Binary_name = "pushswap_checker"
tests = {}

class Test:
    def __init__(self, name, line, description, stdout_expected, stderr_expected, compare, action):
        self.name = name
        self.line = line
        self.description = description
        self.stdout_expected = stdout_expected
        self.stderr_expected = stderr_expected
        self.subprocess = subprocess.run(
            action,
            shell=True,
            capture_output=True,
            text=True
        )
        self.output = self.subprocess.stdout
        self.error = self.subprocess.stderr
        self.status = compare(self.expected, self.output)
        
    def compare_output(self, expected, output):
        return expected == output
            

def get_line_number():
    """Get the line number of the function that called this function."""
    return f"{__file__}:{sys._getframe(1).f_lineno}"

def add_test(test_name, status, line=None):
    global tests
    tests[f"{test_name} - {line}"] = status

def makefile_cmd(cmd=None):
    """Execute a Makefile command and return its code."""
    full_cmd = f"make {cmd}" if cmd else "make"
    result = subprocess.run(
        f"cd .. && {full_cmd}",
        shell=True,
        capture_output=True,
        text=True
    )
    return result.returncode, result.stdout, result.stderr

def test_makefile():
    """Test different Makefile targets."""
    test = os.path.exists(Makefile_path)
    assert test, error_messages("A0", "Makefile exists", "Makefile does not exist")
    add_test("A0", test, get_line_number())
    
    makefile_commands = [
        "all",
        "clean",
        "fclean",
        "re",
        "test",
        "tests_run",
        "tests_re",
        "Bonus",
        "Bonus_re",
        "Bonus_tests_run",
        None  # Equivalent to just "make" or "make all"
    ]
    for i, cmd in enumerate(makefile_commands, 1):
        print(f"Testing Makefile | Command: {cmd or 'make'} | Expected: 0")
        code, stdout, stderr = makefile_cmd(cmd)
        assert code == 0, error_messages(f"A{i}", "0", code)
        add_test(f"A{i}", True if code == 0 else False, get_line_number())
        if stdout:
            print(stdout)
        if stderr:
            print(stderr)
    print("Testing Makefile | Command: undefined | Expected: 2")
    code, stdout, stderr = makefile_cmd("undefined")
    i+=1
    assert code != 0, error_messages(f"A{i}", "non-zero", code)
    add_test(f"A{i}", True if code != 0 else False, get_line_number())
    print(stdout or stderr)
    
def error_messages(test, expected, actual):
    return f"Test {test} failed. Expected: {expected}, got: {actual}"

def test_main():
    print("Testing main | Operators: pa pa pb | Ints: 1 2 3 4 5 | Expected: KO: ([2,3,4,5],[1])")
    process_1 = subprocess.run(
        f'cd .. && echo "pa pa pb" | ./{Binary_name} 1 2 3 4 5',
        shell=True,
        capture_output=True,
        text=True
    )
    assert process_1.returncode == 0, error_messages("B1", "0", process_1.returncode)
    assert process_1.stdout == "KO: ([2,3,4,5],[1])\n", error_messages("B1", "KO: ([2,3,4,5],[1])\n", process_1.stdout)
    add_test("B1", process_1.returncode == 0 and process_1.stdout == "KO: ([2,3,4,5],[1])\n", get_line_number())
    
    print("Testing main | Operators: undefined | Ints: 1 2 3 4 5 | Expected: return 84")
    process_2 = subprocess.run(
        f'cd .. && echo undefined | ./{Binary_name} 1 2 3 4 5',
        shell=True,
        capture_output=True,
        text=True
    )
    assert process_2.returncode == 84, error_messages("B2", "84", process_2.returncode)
    assert process_2.stdout == "", error_messages("B2", "", process_2.stdout)
    add_test("B2", process_2.returncode == 84 and process_2.stdout == "", get_line_number())
    
    print("Testing main | Operators: pa pa pb | Ints: undefined | Expected: return 84")
    process_3 = subprocess.run(
        f'cd .. && echo "pa pa pb" | ./{Binary_name}',
        shell=True,
        capture_output=True,
        text=True
    )
    assert process_3.returncode == 84, error_messages("B3", "84", process_3.returncode)
    assert process_3.stdout == "", error_messages("B3", "", process_3.stdout)
    add_test("B3", process_3.returncode == 84 and process_3.stdout == "", get_line_number())
    
    print("Testing main | Operators: sa pb pb pb sa pa pa pa | Ints: 2 1 3 6 5 8")
    process_4 = subprocess.run(
        f'cd .. && echo "sa pb pb pb sa pa pa pa" | ./{Binary_name} 2 1 3 6 5 8',
        shell=True,
        capture_output=True,
        text=True
    )
    assert process_4.returncode == 0, error_messages("B4", "0", process_4.returncode)
    assert process_4.stdout == "OK\n", error_messages("B4", "OK", process_4.stdout)
    add_test("B4", process_4.returncode == 0 and process_4.stdout == "OK\n", get_line_number())
    
    print ("Testing main | Operators: sa pb pb pb | Ints: 2 1 3 6 5 8 | Expected: KO: ([6,5,8],[3,2,1])")
    process_5 = subprocess.run(
        f'cd .. && echo "sa pb pb pb" | ./{Binary_name} 2 1 3 6 5 8',
        shell=True,
        capture_output=True,
        text=True
    )
    assert process_5.returncode == 0, error_messages("B5", "0", process_5.returncode)
    assert process_5.stdout == "KO: ([6,5,8],[3,2,1])\n", error_messages("B5", "KO: ([6,5,8],[3,2,1])", process_5.stdout)
    add_test("B5", process_5.returncode == 0 and process_5.stdout == "KO: ([6,5,8],[3,2,1])\n", get_line_number())

def prRed(skk): print("\033[91m {}\033[00m" .format(skk), end="")
def prGreen(skk): print("\033[92m {}\033[00m" .format(skk), end="")

if __name__ == "__main__":
    test_makefile()
    makefile_cmd("re")
    test_main()
    makefile_cmd("fclean") 
    if all(tests.values()):
        prGreen(f"All {len(tests)} ")
        print(f"tests passed!")
    else:
        prRed(f"{list(tests.values()).count(False)}")
        print(f"/{len(tests.values())} tests failed !")
    for test, status in tests.items():
        if status:
            prGreen(f"Test {test} passed!\n")
        if not status:
            prRed(f"Test {test} failed!\n")
