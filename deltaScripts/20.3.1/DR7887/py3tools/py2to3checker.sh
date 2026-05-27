#!/bin/bash

# Python 2 to 3 compatibility checker script
#
# Author: tgurney

path="${1}"

if [[ "$1" == "" || "$2" != "" ]]; then
    echo Usage: $0 FILE
    echo Check for possible Python 2 to 3 compatibility issues in the specified
    echo FILE. FILE may be a single Python file, or a directory. If a directory,
    echo then all .py files in that directory tree will be checked.
    echo
    echo You will get best results by running 2to3 on your files before running
    echo this, as this has little to no overlap with 2to3\'s fixers.
    echo 
    echo Recommend dumping stdout and stderr of this script to a file for easier
    echo review of the results.
    echo 
    echo When checking for division operators, this script rejects Python files
    echo that have invalid syntax. A traceback will be printed to stderr for
    echo every such file. The division operator checking requires a python3
    echo interpreter to exist on the \$PATH.
    exit 1
fi


find_pattern() {
    if [[ -d "${path}" ]]; then
        find "${path}" -type f -regex ".*\.py$" -exec grep -ERHn "$1" '{}' \;
    else
        grep -ERHn "$1" "${path}"
    fi
}

divscript=$(mktemp || exit 1)
cleanup_exit() {
    rm -f "${divscript}"
    exit 0
}
trap cleanup_exit SIGTERM
trap cleanup_exit SIGINT

cat > "${divscript}" << 'EOF'
from __future__ import print_function

import ast
import os
import sys
import traceback

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

arg = sys.argv[1]

if os.path.isfile(arg):
    try:
        parse_file(arg)
    except Exception as e:
        sys.stderr.write(traceback.format_exc() + '\n')
    sys.exit(0)

for root, _, filenames in os.walk(arg):
    for filename in filenames:
        if filename.endswith('.py'):
            try:
                parse_file(os.path.join(root, filename))
            except Exception as e:
                sys.stderr.write(traceback.format_exc() + '\n')
EOF

echo "----------------------------------------"
echo "Modules exceptions, sets, popen2 are gone -- rewrite your code to remove the use of these:"
echo
find_pattern "^\s*import.*[, ]\s*(exceptions|sets|popen2)"
find_pattern "^\s*from\s*(exceptions|sets|popen2)\s*import "
echo "----------------------------------------"
echo "Many methods have been removed from the string module -- rewrite your code to"
echo "remove the use of these. Most are now methods on str objects."
echo 
find_pattern "[^A-Za-z0-9_]string\s*.\s*(ato[fil]|capitalize|expandtabs|r?find|r?index|count|lower|r?split|splitfields|join|joinfields|[lr]?strip|swapcase|translate|upper|[lr]just|center|zfill|replace)\s*\("
echo "----------------------------------------"
echo "Files that use the subprocess module -- make sure you are correctly"
echo "handling the bytes objects returned from subprocess methods:"
echo
find_pattern "^\s*from\s*subprocess\s*import "
find_pattern "[^A-Za-z0-9_]subprocess\s*[.]"
echo "----------------------------------------"
echo "Built-in 'file' type is gone. Use the 'open' built-in function instead:"
echo
find_pattern "[^A-Za-z0-9_]file\s*\("
echo "----------------------------------------"
echo "'Scientific' and 'pupynere' libraries have been removed. Replace with netcdf4:"
echo
find_pattern "^\s*import.*[, ]\s*(pupynere|scientific)"
find_pattern "^\s*from\s*(scientific|pupynere)\s*import "
echo "----------------------------------------"
echo "Uses of time.mktime require a tuple. Make sure the argument is a tuple:"
echo
find_pattern "^\s*from\s*time\s*import.*[, ]mktime"
find_pattern "time\s*[.]\s*mktime\("
echo "----------------------------------------"
echo "numpy.getbuffer is gone; replace with builtin memoryview:"
echo
find_pattern "^\s*from\s*numpy\s*import.*[, ]getbuffer"
find_pattern "(numpy|np)\s*[.]\s*getbuffer\("
echo "----------------------------------------"
echo "list.sort method uses key functions instead of comparison functions."
echo "Can use functools.cmp_to_key to assist in fixing these:"
echo
find_pattern "[.]\s*sort\s*\([^)]" | grep -Ev "[.]\s*sort\s*\(\s*(key|reverse)\s*="
echo "----------------------------------------"
echo "Builtin 'sorted' method uses key functions instead of comparison functions."
echo "Can use functools.cmp_to_key to assist in fixing these:"
echo
find_pattern "[^A-Za-z0-9_.]sorted\s*\(\s*.*?," | grep -Ev "sorted\s*\(.*?,\s*key\s*="
echo "----------------------------------------"
echo "types.FileType is gone, replace with io.IOBase:"
echo
find_pattern "^\s*from\s*types\s*import.*[, ]FileType"
find_pattern "types\s*[.]\s*FileType"
echo "----------------------------------------"
echo "pickle dump/load returns bytes, make sure you are handling them as bytes and not strings"
echo
find_pattern "from\s*(cP|p)ickle\s*import.*[, ](dumps?|loads?)"
find_pattern "(cP|p)ickle\s*.\s*(dumps?|loads?)\("
echo "----------------------------------------"
echo "Built-in 'exec' function behaves differently from the old 'exec' statement."
echo "You will have to make changes to your code, as exec'd code can no longer"
echo "modify the value of variables in the caller's namespace."
echo "See the migration guide for more details."
echo
find_pattern "[^A-Za-z0-9_]exec\s*\("
echo "----------------------------------------"
echo "Division operator usages - check these to make sure one or the other of"
echo "the operands is a float, and if not, change to //"
echo
python3 ${divscript} ${path}
cleanup_exit
