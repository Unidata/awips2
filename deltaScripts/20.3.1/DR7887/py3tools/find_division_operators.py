#!/awips2/python/bin/python
#
# find_divs.py
# Finds division operators in python files.
#
# Author: tgurney

from __future__ import print_function

import ast
import os
import sys
import traceback

if len(sys.argv) < 2:
    print("Usage: " + sys.argv[0] + " DIR...")
    print()
    print("Finds and prints all division operators in Python files in the specified DIRs.")
    print("This program rejects Python files with invalid syntax. " +
            "A traceback will get printed to stderr for every such file.")
    sys.exit(1)

def parse_file(path): 
    with open(path) as f:
        contents = f.read()
        tree = ast.parse(contents, os.path.abspath(path))
        last_lineno = None
        lines = None
        for node in ast.walk(tree):
            if hasattr(node, 'lineno'):
                last_lineno = node.lineno
            if isinstance(node, ast.Div):
                if not lines:
                    lines = contents.split('\n')
                print(path + ':' + str(last_lineno) + ':' + lines[last_lineno-1])

for arg in sys.argv[1:]:
    for root, _, filenames in os.walk(arg):
        for filename in filenames:
            if filename.endswith('.py'):
                try:
                    parse_file(os.path.join(root, filename))
                except Exception as e:
                    sys.stderr.write(traceback.format_exc() + '\n')
