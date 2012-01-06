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
# $1 = USER_HOME
# $2 = INSTALL_PATH
# $3 = databasePort
# $4 = database_files_home
# $5 = databaseUsername
# $6 = databaseGroup

echo "export LD_LIBRARY_PATH=\"$2/lib:\$LD_LIBRARY_PATH\" ##Added By EDEX Installer" >> $1/.bashrc
echo "export PATH=\"$2/bin:\$PATH\" ##Added By EDEX Installer" >> $1/.bashrc

perl -p -i -e "s/#port = 5432/port = ${3}/g" ${2}/share/sql/postgresql.conf

SETUPDIR=$2/share/sql

if [ "$5" == '$databaseUsername' ];
  then
    ${SETUPDIR}/setup_developer.sh $2 $3 $4
  else
    ${SETUPDIR}/setup_server.sh $2 $3 $4 $5 $6
fi


