#!/bin/bash

export DELTA_BUILD="11.4"
export DELTA_ID="update_32811_gfe"
export DELTA_DESC="purge existing gfe hdf5 files; uninitialize gfe in plugin_info."

export DELTA_RUN_USER="awips"

function runUpdate()
{
   local PSQL_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}\n' awips2-psql`

   local PSQL="${PSQL_INSTALL}/bin/psql -U awips -d metadata -c"
   local HDF5_DIR="/awips2/edex/data/hdf5/gfe"

   # remove existing gfe hdf5 files
   cd ${HDF5_DIR}
   local COUNT=`ls -1 | wc -l`
   if [ "${COUNT}" -gt 0 ]; then
      for dir in `ls -1 ${HDF5_DIR}`;
      do
         if [ ! "${dir}" = "climo" ] &&
            [ ! "${dir}" = "hlsTopo" ]; then

            if [ -d ${HDF5_DIR}/${dir} ]; then
               rm -rf ${HDF5_DIR}/${dir}/*
            fi

         fi
      done
   fi

   # re-initialize plugin_info
   ${PSQL} "UPDATE plugin_info SET initialized='False' WHERE name='gfe';" > /dev/null 2>&1

   return 0
}
