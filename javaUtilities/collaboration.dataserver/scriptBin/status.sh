#!/bin/bash

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
#
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Apr 03, 2014 2886       bgonzale    Initial creation
#
#


(cd $(dirname "$0")/..
PIDFILE=collabserver.pid

if [[ -e $PIDFILE ]]; then
   PIDID=`cat $PIDFILE`
   PROCESS=`ps --pid $PIDID -o args | grep -c "DataserverMain"`
   if [ $PROCESS -eq 1 ]; then
      echo "HTTP Collaboration Dataserver is running"
   else
      echo "HTTP Collaboration Dataserver is not running"
   fi
else
   echo "HTTP Collaboration Dataserver is not running"
fi
)