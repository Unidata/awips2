#!/bin/sh
#
# -----------------------------------------------------------------------------
# File:  catRecordClimateData.sh
#
# First, checks that recordClimate is running.  If not, this script will hang!
#
# If running, then checks to see if the pipe and data files exist.  
# If so, then pipes the RecordClimate data files to the pipe files, which will
# feed the recordClimate program.  Otherwise, exits with an error message
# written to the log file for the current climate run.
#
#
# Modification History:
#
# 12/05/2002  OB2  Bob Morris        Use ~/climate/tmp not ~/adapt_apps/tmp
# 02/03/2003  OB2  Bob Morris        Modified pid logic, error messages, takes
#                                    log file argument, use default otherwise.
#
# Note:  If error messages are modified, climate.sh and display.sh may need
#        to be modified.  They "grep" the log file for errors from this script.
#
# -----------------------------------------------------------------------------

if [ $# = 1 ] 
then
   LOGFILE2=$1
else
   LOGFILE2=/awips/adapt/climate/tmp/tmp.txt
   echo "catRecordClimateData.sh: Using default log file $LOGFILE2" >> $LOGFILE2
fi

# Set the following so that `ps -eo pid,args ...` below works in HP-UX.
export UNIX95=1

pid=`ps -eo pid,args | grep -E 'adapt[/].*recordClimate' | awk '{print $1}'`

echo "" >> $LOGFILE2
echo "recordClimate process ID on DS = $pid" >> $LOGFILE2

if [ "$pid" != "" ]
then

  if [ ! -p /awips/adapt/climate/tmp/RecordClimateRawData.dat -o ! -p /awips/adapt/climate/tmp/RecordClimateStationInfo.dat ]
  then
     echo "" >> $LOGFILE2
     echo "***************************************************************" >> $LOGFILE2
     echo "* WARNING! recordClimate cannot run! Pipe files do not exist! *" >> $LOGFILE2
     echo "***************************************************************" >> $LOGFILE2
     exit 1
  fi

  if [ -p /awips/adapt/climate/tmp/RecordClimateRawData.dat -a -p /awips/adapt/climate/tmp/RecordClimateStationInfo.dat -a \
       -f /awips/adapt/climate/tmp/RecordClimateRawData1.dat -a -f /awips/adapt/climate/tmp/RecordClimateStationInfo1.dat ]
  then
     cat /awips/adapt/climate/tmp/RecordClimateRawData1.dat >> /awips/adapt/climate/tmp/RecordClimateRawData.dat
     cat /awips/adapt/climate/tmp/RecordClimateStationInfo1.dat >> /awips/adapt/climate/tmp/RecordClimateStationInfo.dat
     exit 0
  fi

else
   echo "" >> $LOGFILE2
   echo "*********************************************" >> $LOGFILE2
   echo "* WARNING! recordClimate not running on ds! *" >> $LOGFILE2
   echo "*********************************************" >> $LOGFILE2
fi

exit 1
