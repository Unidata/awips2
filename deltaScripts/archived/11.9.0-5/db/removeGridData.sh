#!/bin/bash

PSQL="/awips2/psql/bin/psql"

if [ ! -f ${PSQL} ]; then
echo "ERROR: The PSQL executable does not exist - ${PSQL}."
echo "FATAL: Update Failed!"
exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

echo "Removing grid data in database"
${PSQL} -d metadata -U awips -c "truncate table grib, grib_models, gridcoverage"

if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

echo ""
echo "Removing grid hdf5 data"
rm -rf /awips2/edex/data/hdf5/grib

echo ""
echo "INFO: The update was successfully applied."

exit 0
