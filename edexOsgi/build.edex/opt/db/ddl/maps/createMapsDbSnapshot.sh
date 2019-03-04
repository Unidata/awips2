#!/bin/sh
##
#
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# 01/23/2017    #6097     randerso    Removed unnecessary command line parameters.
# 
##
if [ $# -lt 1 ] ; then
    echo
    echo usage: `basename $0` [outfile]
    echo "       outfile   - optional output file (default=maps.db"
    echo "example: `basename $0` /tmp/maps.db"
    exit -1
fi

PGBINDIR=/awips2/postgresql/bin
PGUSER=awipsadmin
PGPORT=5432

if [ -z $1 ] ; then
    OUTFILE=maps.db
else
    OUTFILE=${1}
fi

${PGBINDIR}/pg_dump -Fc maps -U ${PGUSER} -p ${PGPORT} > $OUTFILE
