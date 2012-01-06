#!/bin/bash

export DELTA_BUILD="11.4"
export DELTA_ID="A2DropRadarSCANA2"
export DELTA_DESC="Removing Any Radar/SCAN Data from the Database and HDF5"

function purgeData()
{
   echo "Removing $3 from database..."
   psql --command "drop table $3 CASCADE" --dbname metadata --username awips > /dev/null
   psql --command "update plugin_info set initialized=False where name like '%$3%'" --dbname metadata --username awips > /dev/null
   # Ensure that the data directory exists.
   if [ ! -d $1 ]; then
      echo "$1 does not exist."
      return 0
   fi

   # Purge the existing data
   echo "Removing $3 hdf5 data..."
   rm -rf $1/*
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      return 1
   fi
}

function runUpdate()
{
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      return 0
   fi

   if [ "${COMPONENT_INSTALL}" = "" ]; then
      return 1
   fi

   RADAR_DATA_DIR="${COMPONENT_INSTALL}/edex/data/hdf5/radar"
   RADAR_SQL="delete from radar;"
   NAME="radar"
   purgeData "$RADAR_DATA_DIR" "$RADAR_SQL" "$NAME" 

   SCAN_DATA_DIR="${COMPONENT_INSTALL}/edex/data/hdf5/scan"
   SCAN_SQL="delete from scan;"
   NAME="scan"
   purgeData "$SCAN_DATA_DIR" "$SCAN_SQL" "$NAME"

   return 0
}
