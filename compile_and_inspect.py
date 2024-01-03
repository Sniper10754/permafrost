#!/usr/bin/env python3
import subprocess
import tempfile
import pathlib
import sys

if __name__ == "__main__":
    file_to_compile = None

    if len(sys.argv) == 2:
        file_to_compile = sys.argv[1]
    else:
        print(f"Usage: {sys.argv[0]} [PATH]")
        exit(0)

    with tempfile.NamedTemporaryFile() as output_temp_file:
        result = subprocess.run([
            "cargo",
            "r",
            "--quiet",
            "--bin",
            "frostbite-compiler-driver",
            "--",
            "compile",
            str(
                pathlib.Path(file_to_compile)
                    .absolute()
                ),
            "-o",
            str(
                pathlib.Path(output_temp_file.name)
                    .absolute()
            )
        ])

        if result.returncode != 0:
            exit(1)

        subprocess.run([
            "cargo",
            "r",
            "--quiet",
            "--bin",
            "frostbite-disassembler",
            "--",
            "disassemble",
            str(
                pathlib.Path(output_temp_file.name)
                    .absolute()
            )
        ])

