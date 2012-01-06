#!/bin/bash
# This script will remove Canadian information from the grib and grib_models table; the associated hdf5
# files will also be removed. This script was created for DR #11773.
#
# This update needs to be performed with build 11.9.0-7.
#

PSQL="/awips2/psql/bin/psql"
SQL_DELETE_CMD1="DELETE FROM awips.grib WHERE modelinfo_id IN (SELECT id FROM awips.grib_models WHERE modelname IN('Canadian-NH','Canadian-Reg'));"
SQL_DELETE_CMD2="DELETE from awips.grib_models WHERE modelname IN('Canadian-NH','Canadian-Reg');"

if [ ! -f ${PSQL} ]; then
   echo "ERROR: The psql executable does not exist."
   echo "FATAL: Update Failed!"
   exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

echo "INFO: Removing Canadian records from awips.grib."
${PSQL} -U awips -d metadata -c "${SQL_DELETE_CMD1}"
if [ $? -ne 0 ]; then
   echo "FATAL: Update Failed!"
   exit 1
fi
echo "INFO: Removing Canadian records from awips.grib_models."
${PSQL} -U awips -d metadata -c "${SQL_DELETE_CMD2}"
if [ $? -ne 0 ]; then
   echo "FATAL: Update Failed!"
   exit 1
fi

echo ""
echo "INFO: Deleting Canadian HDF5 Grib files."
if [ -d /awips2/edex/data/hdf5/grib ]; then
   if [ -d /awips2/edex/data/hdf5/grib/Canadian-NH ]; then
      echo "INFO: Removing '/awips2/edex/data/hdf5/grib/Canadian-NH'."
      rm -rf /awips2/edex/data/hdf5/grib/Canadian-NH
      if [ $? -ne 0 ]; then
         echo "ERROR: Failed to remove '/awips2/edex/data/hdf5/grib/Canadian-NH'."
         echo "FATAL: Update failed."
         exit 1
      fi
   fi
   if [ -d /awips2/edex/data/hdf5/grib/Canadian-Reg ]; then
      echo "INFO: Removing '/awips2/edex/data/hdf5/grib/Canadian-Reg'."
      rm -rf /awips2/edex/data/hdf5/grib/Canadian-Reg
      if [ $? -ne 0 ]; then
         echo "ERROR: Failed to remove '/awips2/edex/data/hdf5/grib/Canadian-Reg'."
         echo "FATAL: Update failed."
         exit 1
      fi
   fi
fi

echo "INFO: The update was successful."
exit 0
