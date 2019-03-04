#!/bin/bash
##
##
# creates the radar_spatial table from a shapefile

echo "Generating new radar_spatial table"
psql -U awips -d metadata -c "DELETE FROM radar_spatial"
echo "Deleted contents of radar_spatial table"
shp2pgsql -s 4326 -a -w $@ awips.radar_spatial > radarSpatial.sql
echo "Done generating radar_spatial table"
