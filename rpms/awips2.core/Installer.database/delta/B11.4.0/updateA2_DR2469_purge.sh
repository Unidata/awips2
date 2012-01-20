#!/bin/bash

export DELTA_BUILD="11.4"
export DELTA_ID="updateA2_DR2469_purge"
export DELTA_DESC="remove unused columns in plugin_info and purge satellite, sfcobs, bufrsigwx, and bufrmos tables"

export DELTA_RUN_USER="awips"

function runUpdate()
{
   local HDF5_DIR="/awips2/edex/data/hdf5"

   # find psql.
   local PSQL_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}\n' awips2-psql`

   local PSQL="${PSQL_INSTALL}/bin/psql -U awips -d metadata -c"

   ################################################################
   # Remove the unused columns from the plugin_info table
   ################################################################   
   ${PSQL} "ALTER TABLE awips.plugin_info DROP COLUMN version;"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      return 1
   fi
   ${PSQL} "ALTER TABLE awips.plugin_info DROP COLUMN retentiontime;"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      return 1
   fi

   ################################################################
   # Delete all satellite data due to HDF5 path changes
   ################################################################
   ${PSQL} "DELETE FROM awips.satellite;"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      return 1
   fi
   rm -rf ${HDF5_DIR}/satellite

   ################################################################
   # Delete all sfcobs data due to HDF5 path changes
   ################################################################
   ${PSQL} "DELETE FROM awips.sfcobs;"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      return 1
   fi
   rm -rf ${HDF5_DIR}/sfcobs

   ################################################################
   # Delete all bufrsigwx data due to HDF5 path changes
   ################################################################
   ${PSQL} "DELETE FROM awips.bufrsigwx;"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      return 1
   fi
   rm -rf ${HDF5_DIR}/bufrsigwx

   ################################################################
   # Delete all bufrmos data due to HDF5 path changes
   ################################################################
   ${PSQL} "DELETE FROM awips.bufrmos;"
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      return 1
   fi
   rm -rf ${HDF5_DIR}/bufrmos

   ################################################################
   # Rename grib files
   ################################################################
   cd ${HDF5_DIR}/grib
   find . -name "*.h5" -exec bash -c "mv \$1 \`echo \$1 | sed s/00-FH-/-FH-/\`" -- {} \;

   return 0
}
