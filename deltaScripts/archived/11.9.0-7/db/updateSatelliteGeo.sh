#!/bin/bash
# This script will add a new entry to the satellite_geostationary_positions table
# in the metadata database.
#
# This update needs to be performed with build 11.9.0-7.
#

PSQL="/awips2/psql/bin/psql"
SQL_SELECT1="SELECT COUNT(*) FROM awips.satellite_geostationary_positions WHERE satelliteName = 'GOES-15(P)';"
SQL_INSERT_CMD1="INSERT INTO awips.satellite_geostationary_positions (satelliteName,height,latitude,longitude) VALUES ('GOES-15(P)',35794,0,-135);"

if [ ! -f ${PSQL} ]; then
   echo "ERROR: The psql executable does not exist."
   echo "FATAL: Updated failed!"
   exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

echo "INFO: Determining if Record '1' needs to be inserted: ('GOES-15(P)',35794,0,-135)."
RCOUNT=`${PSQL} -U awips -d metadata --no-align --field-separator ', ' \
	--pset footer --tuples-only -c "${SQL_SELECT1}"`
if [ $? -ne 0 ]; then
   echo "FATAL: Update Failed!"
   exit 1
fi
if [ ${RCOUNT} -eq 0 ]; then
   echo "INFO: Inserting first additional record: satelliteName = 'GOES-15(P)'."
   
   ${PSQL} -U awips -d metadata -c "${SQL_INSERT_CMD1}"
   if [ $? -ne 0 ]; then
      echo "FATAL: Update Failed!"
      exit 1
   fi
else
   echo "INFO: A record with satelliteName='GOES-15(P)' is already present."
fi

echo "INFO: The update was successful."
exit 0
