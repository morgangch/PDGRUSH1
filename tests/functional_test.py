#!/usr/bin/python3

import os
import sys
import subprocess

Makefile_path = os.path.join(os.path.dirname(__file__), '../Makefile')
Binary_name = "pushswap_checker"

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
    assert os.path.exists(Makefile_path), "Makefile not found"
    
    makefile_commands = [
        "all",
        "clean",
        "fclean",
        "re",
        "test",
        "tests_run",
        "tests_re",
        "bonus",
        "bonus_re",
        None  # Equivalent to just "make"
    ]
    
    for i, cmd in enumerate(makefile_commands):
        print(f"Testing Makefile | Command: {cmd or 'make'} | Expected: 0")
        code, stdout, stderr = makefile_cmd(cmd)
        assert code == 0, error_messages(f"A{i}", "0", code)
        if stdout:
            print(stdout)
        if stderr:
            print(stderr)
    
    print("Testing Makefile | Command: undefined | Expected: 2")
    code, stdout, stderr = makefile_cmd("undefined")
    assert code != 0, error_messages("A_invalid", "non-zero", code)
    print(stdout or stderr)
    
def error_messages(test, expected, actual):
    return f"Test {test} failed. Expected: {expected}, got: {actual}"

def test_main():
    print("Testing main | Operators: pa pa pb | Ints: 1 2 3 4 5 | Expected: OK")
    process_1 = subprocess.run(
        f'cd .. && echo "pa pa pb" | ./{Binary_name} 1 2 3 4 5',
        shell=True
    )
    assert process_1.returncode == 0, error_messages("B1", "0", process_1.returncode)
    
    print("Testing main | Operators: undefined | Ints: 1 2 3 4 5 | Expected: return 84")
    process_2 = subprocess.run(
        f'cd .. && echo undefined | ./{Binary_name} 1 2 3 4 5',
        shell=True,
        capture_output=True,
        text=True
    )
    assert process_2.returncode == 84, error_messages("B2", "84", process_2.returncode)
    
    print("Testing main | Operators: pa pa pb | Ints: undefined | Expected: return 84")
    process_3 = subprocess.run(
        f'cd .. && echo "pa pa pb" | ./{Binary_name}',
        shell=True,
        capture_output=True,
        text=True
    )
    assert process_3.returncode == 84, error_messages("B3", "84", process_3.returncode)

if __name__ == "__main__":
    test_makefile()
    makefile_cmd("re")
    test_main()
    print("All tests passed!")
