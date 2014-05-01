#!/bin/sh

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

# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Feb 16, 2008            jelkins     - Initial creation

# waits until the program is not in use before executing it

# USAGE
# Create a link to this script.  The link name should be the name of the program
# to execute as a single instance and should be placed into a directory located
# one directory above the location of the actual program.

lockfile="/tmp/$(basename $0).tmp.lock"
program_location=($(which -a $(basename $0)))

# critical section checking
# http://www.davidpashley.com/articles/writing-robust-shell-scripts.html
while [ 1 ]
do
	if ( set -o noclobber; echo "$USER on PID $$" > "$lockfile") 2> /dev/null; 
	then
		trap 'rm -f "$lockfile"' INT TERM EXIT

		${program_location[1]} $@
		error_code=$?

		rm -f "$lockfile"
		trap - INT TERM EXIT
		exit $error_code
	else
        lock_id="$(cat $lockfile)"
        if [ ! "$lock_id" = "$prev_lock_id" ]
        then
                echo "Failed to acquire lockfile: $lockfile." 
                echo "Held by $lock_id"
                echo "Waiting to acquire lockfile ..."
                prev_lock_id="$lock_id"
        fi
	fi
done 
