#!/bin/bash

# Replace lines like:
#   for a in b.keys():
#   for a in list(b.keys()):
#   for a in list(b.values()):
#   for a in list(b.items()):
#
# With the simpler alternative:
#   for a in b:
#   for a in b.values():
#   for a in b.items():
#
# Not necessary for Python 3 compatibility, but may improve performance.
#
# WARNING: This may change the semantics of your code! Example, code that
# iterates over dictionary keys and modifies the dictionary from within the
# loop body:
#
# for a in list(b.keys()):
#     if should_delete(a):
#         del b[a]
#
# If you remove the list() call from the above code, it will fail with a
# RuntimeError.  Review the output of this script (e.g. using 'diff' to compare
# the changed source file with the original) and make sure that any for-loops
# that are updated by the script do not add or remove keys from the dictionary.
# If they do, you must add the list() call back in to those for-loops.
# 
# Author: tgurney

file="$1"

if [[ ! -f "${file}" ]] ; then
   echo "Usage: $0 FILE"
   echo
   echo "Replaces constructs in the Python source FILE like 'for a in b.keys():'"
   echo "or 'for a in list(b.keys()):' with simpler logic. This works on 'for',"
   echo "'if', and 'elif' statements, and applies to keys(), values(), and items()."
   echo 
   echo "This script creates a copy of the original file with a .bak extension "
   echo "if it changes the specified file."
   exit 1
fi

tmpfile=$(mktemp || exit 1)
cleanup_exit() {
    rm -f "${tmpfile}"
    exit 1
}
trap cleanup_exit SIGINT
trap cleanup_exit SIGTERM

perl -x "${0}" "${file}" > "${tmpfile}"
diff -q "${file}" "${tmpfile}" >/dev/null
if [[ "$?" == "1" ]]; then
    now=$(date "+%Y%m%d_%H%M%S")
    bakfile="${file}.${now}.bak"
    cp -a "${file}" "${bakfile}" && mv "${tmpfile}" "${file}"
    if [[ -x "${bakfile}" ]]; then
        chmod +x "${file}"
    fi
    echo "${file}: updated. Original is at ${bakfile}"
else
    echo "${file}: no changes"
fi
rm -f "${tmpfile}"
exit 0


#!perl

use strict;
use warnings;

while (<>) {
    if (/^(\s*)(for|if|elif)( .* in )(.*)\.\s*keys\s*\(\s*\)\s*(:)/) {
        # for a in b.keys(): -> for a in b:
        print $1 . $2 . $3 . $4 . $5 . "\n";
    } elsif (/^(\s*)(for|if|elif)( .* in )\s*list\s*\((.*)\s*\.\s*keys\s*\(\s*\)\s*\)\s*(:)/) {
        # for a in list(b.keys()): -> for a in b:
        print $1 . $2 . $3 . $4 . $5 . "\n";
    } elsif (/^(\s*)(for|if|elif)( .* in )\s*list\s*\((.*)\s*(\.\s*values\s*\(\s*\))\s*\)\s*(:)/) {
        # for a in list(b.values()): -> for a in b.values():
        print $1 . $2 . $3 . $4 . $5 . $6 . "\n";
    } elsif (/^(\s*)(for|if|elif)( .* in )\s*list\s*\((.*)\s*(\.\s*items\s*\(\s*\))\s*\)\s*(:)/) {
        # for a in list(b.items()): -> for a in b.items():
        print $1 . $2 . $3 . $4 . $5 . $6 . "\n";
    } else {
        print;
    }
}
