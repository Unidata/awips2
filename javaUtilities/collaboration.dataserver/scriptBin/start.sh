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
# Mar 03, 2014 2756       bclement    initial creation
# Mar 06, 2014 2756       bclement    changed classpath to reflect new lib structure
#                                     added log level change when debugging
#
#

if [[ $# > 0 && $1 == '-d' ]]
then
    dbArg='-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5118'
    logLevel='-Ddataserver.logging.level=DEBUG'
else
    # defaults
    dbArg=''
    logLevel=''
fi

(cd $(dirname "$0")/..
PIDFILE=collabserver.pid
if [[ -e $PIDFILE ]]
then
	echo "PID file already exists at $PIDFILE, exiting"
	exit 1
fi
nohup java $dbArg $logLevel -server -cp lib/uframe/*:lib/foss/* com.raytheon.collaboration.dataserver.DataserverMain &

echo $! > $PIDFILE
)
