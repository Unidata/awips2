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

echo "Generating new radar_spatial table"
psql -U awips -d metadata -c "DELETE FROM radar_spatial"
echo "Deleted contents of radar_spatial table"
shp2pgsql -s 4326 -a -w $@ awips.radar_spatial > radarSpatial.sql
echo "Done generating radar_spatial table"
