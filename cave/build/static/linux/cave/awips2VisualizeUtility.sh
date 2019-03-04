#!/bin/bash
#
#
#

# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# July 10 2013 DR 16111   dhuffman    Initial creation
#
# 
# @author dhuffman
# @version 1.0



# This script will kill any running AlertViz and/or Cave
# processes when a user logs off.

if [ ! -f /tmp/vizUtility.log ]; then
    touch /tmp/vizUtility.log
else
    echo "" > /tmp/vizUtility.log
fi

date >> /tmp/vizUtility.log

function findAlertvizProcesses {
# Find all the alertviz processes.
echo "Searching for alertviz processes." >> /tmp/vizUtility.log
zpid=` ps u -u $(whoami)| grep '[a]lertviz' | awk '{print $2}' `
npid=` echo $zpid | wc -w `
if [ $npid -le 0 ]
then
    echo "There are no alertviz processes found." >> /tmp/vizUtility.log
    date >> /tmp/vizUtility.log
fi
}

function findAlertvizShProcesses {
# Find all the alertviz.sh processes.
echo "Searching for alertviz.sh processes." >> /tmp/vizUtility.log
zpid=` ps u -u $(whoami) | grep '[a]lertviz.sh' | awk '{print $2}'  `
npid=` echo $zpid | wc -w `
if [ $npid -le 0 ]
then
    echo "There are no alertviz.sh processes found." >> /tmp/vizUtility.log
    date >> /tmp/vizUtility.log
fi
}

function findCaveProcesses {
# Find all the Cave processes.
echo "Searching for cave processes." >> /tmp/vizUtility.log
zpid=` ps u -u $(whoami) | grep '[c]ave' | awk '{print $2}' `
npid=` echo $zpid | wc -w `
if [ $npid -le 0 ]
then
    echo "There are no cave processes found." >> /tmp/vizUtility.log
    date >> /tmp/vizUtility.log
fi
}


# First let's attempt to kill the processes quickly which will work if the computer is not burdened.
findAlertvizShProcesses
for pid in $zpid 
do
    echo "Attempting to kill 'alertviz.sh' process with pid ${pid}." >> /tmp/vizUtility.log
    kill ${pid}  2>> /tmp/vizUtility.log
done

findAlertvizProcesses
for pid in $zpid 
do
    echo "Attempting to kill 'alertviz' process with pid ${pid}." >> /tmp/vizUtility.log
    kill ${pid}  2>> /tmp/vizUtility.log
done

findCaveProcesses
for pid in $zpid 
do
    echo "Attempting to kill 'cave' process with pid ${pid}." >> /tmp/vizUtility.log
    kill ${pid}  2>> /tmp/vizUtility.log
done


# Second let's be resolute in our assurances that these processes are killed.
# Please review the paperwork included in DR 16111 for an unabridged explanation.
findAlertvizShProcesses
# Lets loop until we are sure all the alertviz.sh processes are killed or we
# have looped too many times.
ntoomany=2002
while [[ $npid -ne 0  &&  $ntoomany -ne 0 ]]
do
    for pid in $zpid 
    do
        echo "Attempting to kill 'alertviz.sh' process with pid ${pid}." >> /tmp/vizUtility.log
        kill -9 ${pid}  2>> /tmp/vizUtility.log
    done
    npid=0
    ((ntoomany-=1))
    if [ $ntoomany -le 1 ]
    then
        echo "The kill alertviz portion of this script $0 has been unable preform its duties. 02" >> /tmp/vizUtility.log
        break
    fi
    sleep 1
    findAlertvizShProcesses
done

# Let's give the SIGTERM a chance if it has not had enough time yet.
sleep 1
findAlertvizProcesses
for pid in $zpid 
do
    echo "Attempting to kill 'alertviz' process with pid ${pid}." >> /tmp/vizUtility.log
    kill -9 ${pid}  2>> /tmp/vizUtility.log
done


findCaveProcesses
for pid in $zpid 
do
    echo "Attempting to kill 'cave' process with pid ${pid}." >> /tmp/vizUtility.log
    kill -9 ${pid}  2>> /tmp/vizUtility.log
done


date >> /tmp/vizUtility.log
echo >> /tmp/vizUtility.log


