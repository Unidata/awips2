#!/bin/bash
# This script will add two new entries to the satellite_creating_entities
# table in the metadata database.
#
# This update needs to be performed with build 11.9.0-7.
#

PSQL="/awips2/psql/bin/psql"
SQL_SELECT1="SELECT COUNT(*) FROM awips.satellite_creating_entities WHERE entityid = 17;"
SQL_INSERT_CMD1="INSERT INTO awips.satellite_creating_entities VALUES (17,'GOES-14(O)');"
SQL_SELECT2="SELECT COUNT(*) FROM awips.satellite_creating_entities WHERE entityid = 18;"
SQL_INSERT_CMD2="INSERT INTO awips.satellite_creating_entities VALUES (18,'GOES-15(P)');"

if [ ! -f ${PSQL} ]; then
   echo "ERROR: The psql executable does not exist."
   echo "FATAL: Update failed!"
   exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

echo "INFO: Determining if Record '1' needs to be inserted: (17,'GOES-14(O)')."
RCOUNT=`${PSQL} -U awips -d metadata --no-align --field-separator ',' \
	--pset footer --tuples-only -c "${SQL_SELECT1}"`
if [ $? -ne 0 ]; then
   echo "FATAL: Update Failed!"
   exit 1
fi
if [ ${RCOUNT} -eq 0 ]; then
   echo "INFO: Inserting first additional record: entityid = 17."

   ${PSQL} -U awips -d metadata -c "${SQL_INSERT_CMD1}"
   if [ $? -ne 0 ]; then
      echo "FATAL: Update Failed!"
      exit 1
   fi
else
   echo "INFO: A record with entityid=17 is already present."
fi

echo "INFO: Determining if Record '2' needs to be inserted: (18,'GOES-15(P)')."
RCOUNT=`${PSQL} -U awips -d metadata --no-align --field-separator ',' \
	--pset footer --tuples-only -c "${SQL_SELECT2}"`
if [ $? -ne 0 ]; then
   echo "FATAL: Updated Failed!"
   exit 1
fi
if [ ${RCOUNT} -eq 0 ]; then
   echo "INFO: Inserting second additional record: entityid = 18."

   ${PSQL} -U awips -d metadata -c "${SQL_INSERT_CMD2}"
   if [ $? -ne 0 ]; then
      echo "FATAL: Update Failed!"
      exit 1
   fi
else
   echo "INFO: A record with entityid=18 is already present."
fi

echo "INFO: The update was successful."
exit 0
