#!/bin/bash

# This script will kill any running AlertViz and/or
# CAVE processes whenever the user logs off.

if [ ! -f ${HOME}/vizUtility.log ]; then
   touch ${HOME}/vizUtility.log
else
   echo "" >> ${HOME}/vizUtility.log
fi

# Find all CAVE processes.
echo "Searching for cave processes." >> ${HOME}/vizUtility.log
for pid in `ps aux | grep [c]ave | awk '{print $2}'`;
do
   kill -9 ${pid}
   echo "Killing 'cave' process with pid ${pid}." >> ${HOME}/vizUtility.log
done

# Find the alertviz.sh script.
echo "Searching for the alertviz.sh script." >> ${HOME}/vizUtility.log
for pid in `ps aux | grep [a]lertviz.sh | awk '{print $2}'`;
do
   kill -9 ${pid}
   echo "Killing 'alertviz.sh' process with pid ${pid}." >> ${HOME}/vizUtility.log
done

# Find the AlertViz process.
echo "Searching for the alertviz process." >> ${HOME}/vizUtility.log
for pid in `ps aux | grep [a]lertviz | awk '{print $2}'`;
do
   kill -9 ${pid}
   echo "Killing 'alertviz' process with pid ${pid}." >> ${HOME}/vizUtility.log
done
echo "FINISHED" >> ${HOME}/vizUtility.log

exit 0
