#! /bin/bash

# THIS SCRIPT SHOULD BE RUN BY A FOCAL POINT, NOT ON INSTALL
#
# This script will scan either the specified directory, or the USER, SITE, and WORKSTATION
# level files for python files that are comparing a variable to a string literal. 
# 
# Any files that are output from this should specify the full path and line number where
# the issue is occurring.

# USAGE: stringLiteralCompCheck.sh <PATH_TO_SCAN>
#   Running the script with no arguments will scan for SITE, USER, and WORKSTATION level
#   files located at /awips2/edex/utilities/*/{site, user, workstation} that would 
#   produce an error upon compilation
#

# The SyntaxWarning we are looking for is first reported in python 3.8
PYTHON=python3.8

if [ -z "$1" ]
then
  #No argument provided, looking through SITE, USER, and WORKSTATION level python files
  pathlist=("site" "user" "workstation")
  pathPrefix="/awips2/edex/data/utility/*/"
else
  pathlist=( $1 )
  pathPrefix=""
fi

for i in "${pathlist[@]}"
do
  path="$pathPrefix$i"
  echo ""
  echo "Looking for instances of 'is' with a literal in $path"
  echo ""
#This will grab instances where 'is' or 'is not' is followed by either single or double quotes not appearing in comments, then attempt to compile it to see if an error is generated
  error=$(find $path -name '*.py' | xargs grep '^[[:blank:]]*[^[:blank:]#]'| grep -P 'is [\047\042]|is not [\047\042]' | awk '{print substr($1, 1, length($1)-1)}' | xargs $PYTHON -B -m py_compile 2>&1)
 
  while IFS= read -r line; do
    echo $line | sed -n "/with a literal/p"
  done <<< "$error"
done

echo ""
echo "The above files produced a warning when attempting to compile the Python code."
echo "These instances should be investigated and resolved. Using 'is' when comparing"
echo "to a literal can work on accident, but is not guaranteed."
echo ""
