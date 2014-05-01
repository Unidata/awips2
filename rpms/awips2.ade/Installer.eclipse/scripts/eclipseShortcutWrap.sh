#!/bin/bash

# This Script Should Only Be Run By The ADE Eclipse Shortcuts.
# Any User That Attempts To Run This Script Manually May
# Encounter Unexpected Behavior.

dir=${0%/*}
if [ "${dir}" = "$0" ]; then
   dir="."
fi

cd ${dir}

# Attempt To Run The Eclipse Script.

./eclipse.sh
RC="$?"

if [ ! "${RC}" = "0" ]; then
   sleep 50
   exit 1
fi

COUNT=5
echo -n "This Terminal Will Close In ..."
while [ ! "${COUNT}" = "0" ]
do
   echo -n " ${COUNT}"
   let COUNT=COUNT-1
   sleep 2
done
