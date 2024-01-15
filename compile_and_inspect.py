#!/usr/bin/env python3
import os
import subprocess
import tempfile
import pathlib
import sys

DEBUG_ENV_OPTIONS = dict(
    RUST_LOG = os.environ.get("RUST_LOG") or "trace",
    RUST_BRACKTRACE = os.environ.get("RUST_BRACKTRACE") or "1",
)

PROC_ENV_AND_DEBUG_ENV = {
    **dict(os.environ),
    **DEBUG_ENV_OPTIONS,
}

if __name__ == "__main__":
    file_to_compile = None

    if len(sys.argv) == 2:
        file_to_compile = sys.argv[1]
    else:
        print(f"Usage: {sys.argv[0]} [PATH]")
        exit(1)

    with tempfile.NamedTemporaryFile() as output_temp_file:
        result = subprocess.run([
            "cargo",
            "r",
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
            )],
            env = PROC_ENV_AND_DEBUG_ENV
        )

        if result.returncode != 0:
            exit(1)

        result = subprocess.run([
            "cargo",
            "r",
            "--bin",
            "frostbite-disassembler",
            "--",
            "disassemble",
            str(
                pathlib.Path(output_temp_file.name)
                    .absolute()
            )],
            env = PROC_ENV_AND_DEBUG_ENV
        )

        if result.returncode != 0:
            exit(1)
