#!/usr/bin/python3

import os
import sys
import subprocess
from argcomplete import debug
import pyperclip

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
        self.action = action
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
        passed = "\033[92m Passed \033[00m"
        failed = "\033[91m Failed \033[00m"
        print(f"Testing {self.name} | {self.description} | \nExpected: \t{self.stdout_expected if self.stdout_expected else 'Nothing'} - {self.stderr_expected}")
        print(f"Output: \t{self.output if self.output else 'Nothing'} - {self.error} | Status: {passed if self.status else failed}")
        add_test(self)
        
    def display_status(self):
        if self.status:
            print(f"Test {self.name}", end=" ")
            prGreen("passed!")
            print(f" ({self.description})")
        else:
            print(f"Test {self.name}", end=" ")
            prRed(f"failed!")
            print(f" ({self.description})\nAt line {self.line}")

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
        ("all", 0, "", get_line_number()),
        ("clean", 0, "", get_line_number()),
        ("fclean", 0, "", get_line_number()),
        ("re", 0, "", get_line_number()),
        ("test", 0, "", get_line_number()),
        ("tests_run", 0, "", get_line_number()),
        ("tests_re", 0, "", get_line_number()),
        ("Bonus", 0, "", get_line_number()),
        ("Bonus_re", 0, "", get_line_number()),
        ("Bonus_tests_run", 0, "", get_line_number()),
        (None, 0, "", get_line_number()) # Equivalent to just "make" or "make all"
    ]

    for i, (cmd, expected_code, expected_stdout, line) in enumerate(makefile_commands, 1):
        test_name = f"A{i}"
        test = Test(
            name=test_name,
            line=line,
            description=f"Makefile {cmd} command test",
            stdout_expected=expected_stdout,
            stderr_expected=expected_code,
            action=f"cd .. && make {cmd} > /dev/null" if cmd else "cd .. && make > /dev/null",
            run=True
        )

    # Test the undefined command to ensure it fails
    print("Testing Makefile | Command: undefined | Expected: 2")
    undefined_test = Test(
        name=f"A{i+1}",
        line=get_line_number(),
        description="Makefile undefined command test | Invalid Make command",
        stdout_expected="",
        stderr_expected=2,
        action="cd .. && make undefined",
        run=True
    )
    tests.append(undefined_test)
    
def error_messages(test, expected, actual):
    return f"Test {test} failed. Expected: {expected}, got: {actual}"

def test_main():
    test_cases = [
        {
            "name": "B1",
            "line": get_line_number(),
            "description": "Operators: pa pa pb | Ints: 1 2 3 4 5 | Expected: KO: ([2,3,4,5],[1])",
            "cmd": f'cd .. && echo "pa pa pb" | ./{Binary_name} 1 2 3 4 5',
            "expected_stdout": "KO: ([2,3,4,5],[1])",
            "expected_returncode": 0
        },
        {
            "name": "B2",
            "line": get_line_number(),
            "description": "Operators: undefined | Ints: 1 2 3 4 5 | Expected: return 84",
            "cmd": f'cd .. && echo undefined | ./{Binary_name} 1 2 3 4 5',
            "expected_stdout": "",
            "expected_returncode": 84
        },
        {
            "name": "B3",
            "line": get_line_number(),
            "description": "Operators: pa pa pb | Ints: empty | Expected: return OK",
            "cmd": f'cd .. && echo "pa pa pb" | ./{Binary_name}',
            "expected_stdout": "OK\n",
            "expected_returncode": 0
        },
        {
            "name": "B4",
            "line": get_line_number(),
            "description": "Operators: sa pb pb pb sa pa pa pa | Ints: 2 1 3 6 5 8",
            "cmd": f'cd .. && echo "sa pb pb pb sa pa pa pa" | ./{Binary_name} 2 1 3 6 5 8',
            "expected_stdout": "OK\n",
            "expected_returncode": 0
        },
        {
            "name": "B5",
            "line": get_line_number(),
            "description": "Operators: sa pb pb pb | Ints: 2 1 3 6 5 8 | Expected: KO: ([6,5,8],[3,2,1])",
            "cmd": f'cd .. && echo "sa pb pb pb" | ./{Binary_name} 2 1 3 6 5 8',
            "expected_stdout": "KO: ([6,5,8],[3,2,1])\n",
            "expected_returncode": 0
        },
        {
            "name": "B6",
            "line": get_line_number(),
            "description": "Operators: pb pa | Ints: 4 3 2 1 | Expected: OK",
            "cmd": f'cd .. && echo "pb pa" | ./{Binary_name} 4 3 2 1',
            "expected_stdout": "KO: ([4,3,2,1],[])\n",
            "expected_returncode": 0
        },
        {
            "name": "B7",
            "line": get_line_number(),
            "description": "Operators: ra ra rra | Ints: 3 2 1 | Expected: KO: ([1,3,2],[])",
            "cmd": f'cd .. && echo "ra ra rra" | ./{Binary_name} 3 2 1',
            "expected_stdout": "KO: ([2,1,3],[])\n",
            "expected_returncode": 0
        },
        {
            "name": "B8",
            "line": get_line_number(),
            "description": "Operators: pb pb ra ra pa pa | Ints: 1 2 3 4 | Expected: OK",
            "cmd": f'cd .. && echo "pb pb ra ra pa pa" | ./{Binary_name} 1 2 3 4',
            "expected_stdout": "OK\n",
            "expected_returncode": 0
        },
        {
            "name": "B9",
            "line": get_line_number(),
            "description": "Operators: sb sa sb | Ints: 1 2 3 4 | Expected: OK",
            "cmd": f'cd .. && echo "sb sa sb" | ./{Binary_name} 1 2 3 4',
            "expected_stdout": "KO: ([2,1,3,4],[])\n",
            "expected_returncode": 0
        },
        
        {
            "name": "B10",
            "line": get_line_number(),
            "description": "Operators: pb pb sa pb sa pa pa pa | Ints: 5 4 3 2 1 | Expected: OK",
            "cmd": f'cd .. && echo "pb pb sa pb sa pa pa pa" | ./{Binary_name} 5 4 3 2 1',
            "expected_stdout": "KO: ([5,4,2,1,3],[])\n",
            "expected_returncode": 0
        },
        {
            "name": "B11",
            "line": get_line_number(),
            "description": "Operators: pb pb pb pb | Ints: 6 5 4 3 2 1 | Expected: KO: ([3,2,1],[6,5,4])",
            "cmd": f'cd .. && echo "pb pb pb pb" | ./{Binary_name} 6 5 4 3 2 1',
            "expected_stdout": "KO: ([2,1],[3,4,5,6])\n",
            "expected_returncode": 0
        },
        {
            "name": "B12",
            "line": get_line_number(),
            "description": "Operators: sa pb ra pb rra pa pa | Ints: 9 8 7 6 5 4 | Expected: OK",
            "cmd": f'cd .. && echo "sa pb ra pb rra pa pa" | ./{Binary_name} 9 8 7 6 5 4',
            "expected_stdout": "KO: ([8,7,9,6,5,4],[])\n",
            "expected_returncode": 0
        },
        {
            "name": "B13",
            "line": get_line_number(),
            "description": "Operators: sss | Ints: 2 1 | Expected: Error 84",
            "cmd": f'cd .. && echo "sss" | ./{Binary_name} 2 1',
            "expected_stdout": "",
            "expected_returncode": 84
        },
        {
            "name": "B14",
            "line": get_line_number(),
            "description": "Operators: sb | Ints: empty | Expected: OK",
            "cmd": f'cd .. && echo "sb" | ./{Binary_name}',
            "expected_stdout": "OK\n",
            "expected_returncode": 0
        },
        {
            "name": "B15",
            "line": get_line_number(),
            "description": "Operators: pb sa | Ints: a b c | Expected: Error 84",
            "cmd": f'cd .. && echo "pb sa" | ./{Binary_name} a b c',
            "expected_stdout": "",
            "expected_returncode": 84
        },
        {
            "name": "B16",
            "line": get_line_number(),
            "description": "Operators: sa | Ints: 2147483647 -2147483648 | Expected: OK",
            "cmd": f'cd .. && echo "sa" | ./{Binary_name} 2147483647 -2147483648',
            "expected_stdout": "OK\n",
            "expected_returncode": 0
        },
        {
            "name": "B17",
            "line": get_line_number(),
            "description": "Operators: sa | Ints: 9223372036854775807 -9223372036854775808 | Expected: OK 0",
            "cmd": f'cd .. && echo "sa" | ./{Binary_name} 9223372036854775807 -9223372036854775808',
            "expected_stdout": "OK",
            "expected_returncode": 0
        }
    ]
    
    # Crée et exécute chaque test en utilisant la classe Test
    for test_case in test_cases:
        Test(
            name=test_case["name"], 
            line=test_case["line"],
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
        prGreen(f"All {len(passed_tests)} tests passed!\n")
    else:
        prRed(f"{len(failed_tests)}")
        print(f"/{len(tests)} tests failed!")

    # Affichage détaillé de chaque test
    for i, test in enumerate(tests, 0):
        if test.status:
            prGreen(f"{i}. {test.name}")
        else:
            prRed(f"{i}. {test.name}")
            """ 
            if input("\nGet the full command output and input? (y/n): \n") == "y":
                print("Command output:", test.output)
                print("Command error:", test.error)
                print("Command expected output:", test.stdout_expected)
                print("Command expected error:", test.stderr_expected)
                print("Command result output:", test.result_sout)
                print("Command result error:", test.result_serr)
                pyperclip.copy(test.action.replace("cd .. && ", "")) """
        print("", end=" ")      
        test.display_status()
