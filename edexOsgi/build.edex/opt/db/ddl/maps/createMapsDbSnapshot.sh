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
if [ $# -lt 1 ] ; then
    echo
    echo usage: `basename $0` installDir [dbUser [dbPort] [outfile]]
    echo "       installDir- directory path to awips installation"
    echo "       dbUser    - optional database user id"
    echo "       dbPort    - optional database port number"
    echo "       outfile   - optional output file"
    echo "example: `basename $0` /awips2 awips 5432"
    exit -1
fi

PGBINDIR=${1}/postgresql/bin

if [ -z $2 ] ; then
    PGUSER=awips
else
    PGUSER=${2}
fi

if [ -z $3 ] ; then
    PGPORT=5432
else
    PGPORT=${3}
fi

if [ -z $4 ] ; then
    OUTFILE=maps.db
else
    OUTFILE=${4}
fi

${PGBINDIR}/pg_dump -Fc maps -U ${PGUSER} -p ${PGPORT} > $OUTFILE
