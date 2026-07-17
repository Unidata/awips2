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
# creates the radar_spatial table from a shapefile


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

echo "Generating new radar_spatial table"
psql -U awips -d metadata -c "DELETE FROM radar_spatial"
echo "Deleted contents of radar_spatial table"
shp2pgsql -s 4326 -a -w $@ awips.radar_spatial > radarSpatial.sql
echo "Done generating radar_spatial table"
