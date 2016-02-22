#!/usr/bin/env python


import os
import sys
import subprocess
from subprocess import run
import json
import tempfile

import assembler as asm

assemblerPath = os.path.join(os.path.abspath(sys.path[0]), './assembler.py')

def main():
    filename = sys.argv[1]

    assembled = run([assemblerPath, filename], stdout=subprocess.PIPE).stdout

    raw_pointerinfo = run([assemblerPath, filename,  '--pointers'], stdout=subprocess.PIPE).stdout
    pointers = json.loads(raw_pointerinfo.decode())

    pointerstring = ""
    for pointer in pointers:
        pointerstring += asm.toWord(pointer)

    pointerstring += 'ZZZZ'

    final = pointerstring + assembled.decode().rstrip()
    print(final)


main()
