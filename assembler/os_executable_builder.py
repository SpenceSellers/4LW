#!/usr/bin/env python


import os
import sys
import subprocess
from subprocess import run
import json
import tempfile

import asm2 as asm

assemblerPath = os.path.join(os.path.abspath(sys.path[0]), './asm2.py')
ospath = 'os1.4lwa'

def main():
    filename = sys.argv[1]

    linkfile = tempfile.NamedTemporaryFile()
    
    linkinfo = run([assemblerPath, ospath, '-S'], stdout=subprocess.PIPE).stdout
    linkfile.write(linkinfo)
    linkfile.flush()
    
    
    assembled = run([assemblerPath, filename, '-l', linkfile.name], stdout=subprocess.PIPE).stdout

    raw_pointerinfo = run([assemblerPath, filename, '-l', linkfile.name, '--pointers'], stdout=subprocess.PIPE).stdout
    pointers = json.loads(raw_pointerinfo.decode())

    pointerstring = ""
    for pointer in pointers:
        pointerstring += asm.toWord(pointer)

    pointerstring += 'ZZZZ'

    final = pointerstring + assembled.decode().rstrip()
    print(final)


main()
