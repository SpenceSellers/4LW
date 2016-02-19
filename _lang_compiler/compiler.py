#!/usr/bin/env python3

import langparse
import ast
import sys

def compile(progstring):
    parsed = langparse.program.parseString(progstring)[0]
    c = ast.Context();
    return parsed.emit_top_level(c);

def compile_file(path):
    f = open(path, 'r')
    progstring = f.read()
    return compile(progstring)

def main():
    filename = sys.argv[1]
    print(compile_file(filename))

main()
