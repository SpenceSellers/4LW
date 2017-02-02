#!/usr/bin/env python3

import langparse
import ast
import sys

def compile(progstring, fname = None):
    return compile_and_emit_context(progstring)[0]

def compile_and_emit_context(progstring, fname = "Root"):
    parsed = langparse.program.parseString(progstring)[0]
    c = ast.Context(name=fname)
    return (parsed.emit_top_level(c), c)

def compile_with_parent_context(progstring, context, fname = "Root"):
    parsed = langparse.program.parseString(progstring)[0]
    c = ast.Context(name=fname, parent=context)
    result = parsed.emit_top_level(c)
    context.inherit(c)
    return result

def compile_file(path):
    return compile_file_and_emit_context(path)[0]

def compile_file_and_emit_context(path):
    f = open(path, 'r')
    progstring = f.read()
    return compile_and_emit_context(progstring, fname = path)

def compile_file_with_parent_context(path, context):
    f = open(path, 'r')
    progstring = f.read()
    return compile_with_parent_context(progstring, context, fname = path)


def main():
    filename = sys.argv[1]
    print(compile_file(filename))

if __name__ == '__main__':
    main()
