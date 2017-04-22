#!/bin/bash
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
#------------------------------------------------------------------------
# Test Script for AFOS PIL Trigger
# Logs execution and product contents into a log file.
#------------------------------------------------------------------------
LOG_FILE=$EDEX_HOME/logs/afos-trigger.log
TIME_NOW=`date`
if [ -z $1 ]; then
   echo "$TIME_NOW: invalid process call, no AWIPS or AFOS PIL provided" >> $LOG_FILE
   exit 1
fi
AFOS_PIL=$1
if [ -z $FXA_DATA ]; then
   echo "$TIME_NOW: invalid configuration, \$FXA_DATA not in environment" >> $LOG_FILE
   exit 1
fi
FILE_PATH=$FXA_DATA/trigger/$AFOS_PIL
if [ ! -f $FILE_PATH ]; then
   echo "$TIME_NOW: unable to find file matching AWIPS or AFOS PIL: $AFOS_PIL" >> $LOG_FILE
   exit 1
fi
echo "$TIME_NOW: processing $AFOS_PIL" >> $LOG_FILE
echo "$TIME_NOW: file $FILE_PATH contents:" >> $LOG_FILE
cat $FILE_PATH >> $LOG_FILE
exit 0

