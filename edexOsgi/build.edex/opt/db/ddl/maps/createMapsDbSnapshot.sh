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
# 01/23/2017    #6097     randerso    Removed unnecessary command line parameters.
# 04/11/2018    #7140     tgurney     Use a2dbauth
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

a2dbauth ${PGBINDIR}/pg_dump -Fc maps -U ${PGUSER} -p ${PGPORT} > $OUTFILE
