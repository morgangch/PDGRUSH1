#!/usr/bin/python3

import os
import sys
import subprocess
from argcomplete import debug

Makefile_path = os.path.join(os.path.dirname(__file__), '../Makefile')
Binary_name = "pushswap_checker"
tests = []

def text_diff(text1, text2):
    """Return the difference between two strings."""
    diff = ""
    for i, (char1, char2) in enumerate(zip(text1, text2)):
        if char1 != char2:
            diff += f"Position {i}: {char1} != {char2}\n"
    return diff

class Test:
    def __init__(self, name, line, description, stdout_expected, stderr_expected:int, action, run=True, result_sout=None, result_serr=None):
        self.name = name
        self.line = line
        self.description = description
        self.stdout_expected = self.readable_output(stdout_expected)
        self.stderr_expected:int = stderr_expected
        self.result_sout = self.readable_output(result_sout) if result_sout else ""
        self.result_serr = result_serr
        self.output = None
        self.error:int = 0
        self.status:bool = None
        if run:
            self.start_subprocess(action)
        else:
            self.get_status(result_sout, result_serr)
            self.start_test()
        
    def compare_output(self, expected, output):
        return expected == output
    
    def start_subprocess(self, cmd):
        self.subprocess = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True
        )
        self.output = self.readable_output(self.subprocess.stdout)
        self.error = int(self.subprocess.returncode)
        self.status = self.get_status(self.output, self.subprocess.returncode)
        self.start_test()
   
    def debug_compare_output(self, expected, output):
        if expected != output:
            print(f"Expected: {expected}\nOutput: {output}")
            #print(text_diff(expected, output))
            print("Repr expected:", repr(expected))
            print("Repr output:", repr(output))
        else:
            print("Expected output and actual output don't differ.")
   
    def get_status(self, output=None, error:int=None):
        if output is None:
            output = self.output
        if error is None:
            error = self.error
        self.status = self.compare_output(
            self.stdout_expected, output
        ) and self.compare_output(
            int(self.stderr_expected), error
        )
        if not self.status:
            self.debug_compare_output(self.stdout_expected, output)
        return self.status
        
    def readable_output(self, output):
        return output.replace("\n", "").replace("\t", "").replace("\r", "").replace("\v", "").replace("\f", "").replace(" ", "").replace("\b", "").replace("\a", "").replace("\0", "").replace("'", "").replace('"', "")
    
    def start_test(self):
        print(f"Testing {self.name} | {self.description} | \nExpected: \t{self.stdout_expected if self.stdout_expected else 'Nothing'} - {self.stderr_expected}")
        print(f"Output: \t{self.output if self.output else 'Nothing'} - {self.error} | Status: {'Passed' if self.status else 'Failed'}\n")
        add_test(self)

def get_line_number():
    """Get the line number of the function that called this function."""
    return f"{__file__}:{sys._getframe(1).f_lineno}"

def add_test(test:Test):
    global tests
    tests.append(test)

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
    tests = [Test(
        name = "A0",
        line = get_line_number(),
        description = "Makefile exists",
        stdout_expected = "",
        stderr_expected = 0,
        action = "cd .. && make > /dev/null",
        run=True
    )]
    
    makefile_commands = [
        ("all", 0, ""),
        ("clean", 0, ""),
        ("fclean", 0, ""),
        ("re", 0, ""),
        ("test", 0, ""),
        ("tests_run", 0, ""),
        ("tests_re", 0, ""),
        ("Bonus", 0, ""),
        ("Bonus_re", 0, ""),
        ("Bonus_tests_run", 0, ""),
        (None, 0, "")  # Equivalent to just "make" or "make all"
    ]

    for i, (cmd, expected_code, expected_stdout) in enumerate(makefile_commands, 1):
        test_name = f"A{i}"
        test = Test(
            test_name,
            get_line_number(),
            f"Makefile {cmd} command test",
            expected_stdout,
            expected_code,
            f"cd .. && make {cmd} > /dev/null" if cmd else "cd .. && make > /dev/null",
            run=True
        )

    # Test the undefined command to ensure it fails
    print("Testing Makefile | Command: undefined | Expected: 2")
    undefined_test = Test(
        f"A{i+1}",
        get_line_number(),
        "2",
        "",
        2,
        "cd .. && make undefined",
        run=True
    )
    tests.append(undefined_test)
    
def error_messages(test, expected, actual):
    return f"Test {test} failed. Expected: {expected}, got: {actual}"

def test_main():
    test_cases = [
        {
            "name": "B1",
            "description": "Operators: pa pa pb | Ints: 1 2 3 4 5 | Expected: KO: ([2,3,4,5],[1])",
            "cmd": f'cd .. && echo "pa pa pb" | ./{Binary_name} 1 2 3 4 5',
            "expected_stdout": "KO: ([2,3,4,5],[1])",
            "expected_returncode": 0
        },
        {
            "name": "B2",
            "description": "Operators: undefined | Ints: 1 2 3 4 5 | Expected: return 84",
            "cmd": f'cd .. && echo undefined | ./{Binary_name} 1 2 3 4 5',
            "expected_stdout": "",
            "expected_returncode": 84
        },
        {
            "name": "B3",
            "description": "Operators: pa pa pb | Ints: empty | Expected: return OK",
            "cmd": f'cd .. && echo "pa pa pb" | ./{Binary_name}',
            "expected_stdout": "OK\n",
            "expected_returncode": 0
        },
        {
            "name": "B4",
            "description": "Operators: sa pb pb pb sa pa pa pa | Ints: 2 1 3 6 5 8",
            "cmd": f'cd .. && echo "sa pb pb pb sa pa pa pa" | ./{Binary_name} 2 1 3 6 5 8',
            "expected_stdout": "OK\n",
            "expected_returncode": 0
        },
        {
            "name": "B5",
            "description": "Operators: sa pb pb pb | Ints: 2 1 3 6 5 8 | Expected: KO: ([6,5,8],[3,2,1])",
            "cmd": f'cd .. && echo "sa pb pb pb" | ./{Binary_name} 2 1 3 6 5 8',
            "expected_stdout": "KO: ([6,5,8],[3,2,1])\n",
            "expected_returncode": 1
        }
    ]
    
    # Crée et exécute chaque test en utilisant la classe Test
    for test_case in test_cases:
        Test(
            name=test_case["name"], 
            line=get_line_number(),
            description=test_case["description"], 
            stdout_expected=test_case["expected_stdout"], 
            stderr_expected=test_case["expected_returncode"],
            action=test_case["cmd"], 
            run=True
        )
        
def prRed(skk): print("\033[91m {}\033[00m" .format(skk), end="")
def prGreen(skk): print("\033[92m {}\033[00m" .format(skk), end="")

if __name__ == "__main__":
    # Lancer les tests de compilation et de nettoyage
    test_makefile()
    makefile_cmd("re")
    
    # Lancer les tests principaux
    test_main()
    
    # Nettoyer après les tests
    makefile_cmd("fclean") 
    
    # Vérifier si tous les tests sont passés
    passed_tests = [test for test in tests if test.status]
    failed_tests = [test for test in tests if not test.status]
    
    # Affichage du résultat global
    if len(failed_tests) == 0:
        prGreen(f"All {len(passed_tests)} tests passed!")
    else:
        prRed(f"{len(failed_tests)}")
        print(f"/{len(tests)} tests failed!")

    # Affichage détaillé de chaque test
    for test in tests:
        if test.status:
            prGreen(f"Test {test.name} passed!")
            print(f" ({test.description})")
        else:
            prRed(f"Test {test.name} failed!")
            print(f" ({test.description})\nAt line {test.line}")
