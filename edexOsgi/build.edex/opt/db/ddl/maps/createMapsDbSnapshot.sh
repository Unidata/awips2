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
#
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Jan 23, 2017 6097       randerso    Removed unnecessary command line parameters.
# Apr 11, 2018 7140       tgurney     Use a2dbauth
# Jul  1, 2021 8544       tgurney     Remove PGBINDIR, not needed anymore
# Mar 28, 2025 2037812    smoorthy    Define pghost appropriately for db calls.
##
if [ $# -lt 1 ] ; then
    echo
    echo usage: `basename $0` [outfile]
    echo "       outfile   - optional output file (default=maps.db"
    echo "example: `basename $0` /tmp/maps.db"
    exit -1
fi


# Set PGHOST appropriately:
# - If we're already on "dv1", set it to dv1.
# - If we're on any other host with postgres running, set it 
#   to "localhost".
# - If we're on any other host without postgres running, set it
#   to "dv1".

export PGHOST='localhost'

host=$(hostname)

#extract first part of hostname of the form dv1-xxx.xxx
IFS="-"
hostTemp=''
for part in $host
do
    hostTemp=$part
    break
done
unset IFS


# if we're on dv1, just set "PGHOST" to dv1
if [ $hostTemp == 'dv1' ] ; then
    export PGHOST=$hostTemp
fi

#check if postgres is running on this host. Should be many 
#processes with "postgres" in the name.
numPostgres=$(ps -ef | grep postgres | wc -l)

#choosing 3 for safety, since the "grep" is atleast 1
if [ $numPostgres -lt 3 ] ; then
   export PGHOST='dv1'
fi


PGUSER=awipsadmin
PGPORT=5432

if [ -z $1 ] ; then
    OUTFILE=maps.db
else
    OUTFILE=${1}
fi

a2dbauth pg_dump -Fc maps -U ${PGUSER} -p ${PGPORT} > $OUTFILE
