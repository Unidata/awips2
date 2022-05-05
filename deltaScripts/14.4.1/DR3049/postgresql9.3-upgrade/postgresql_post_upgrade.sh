#!/bin/bash

source settings.sh

# make a temporary update to the postgresql.conf file for the db vacuums
pushd . > /dev/null 2>&1
cd ${AWIPS2_DATA_DIRECTORY}

# backup the existing postgresql.conf
cp postgresql.conf postgresql.conf.orig
if [ $? -ne 0 ]; then
	echo "Failed to create a temporary backup of postgresql.conf!"
	exit 1
fi

# create an updated postgresql.conf
sed '/vacuum_freeze_table_age/d' postgresql.conf > postgresql.conf.update
if [ $? -ne 0 ]; then
	echo "Failed to update postgresql.conf!"
	exit 1
fi

echo "vacuum_freeze_table_age = 0" >> postgresql.conf.update
if [ $? -ne 0 ]; then
	echo "Failed to update postgresql.conf!"
	exit 1
fi

mv postgresql.conf.update postgresql.conf
if [ $? -ne 0 ]; then
	echo "Failed to update postgresql.conf!"
	exit 1
fi

popd > /dev/null 2>&1

# start PostgreSQL
/sbin/service edex_postgres start
if [ $? -ne 0 ]; then
	echo "ERROR: Failed to start PostgreSQL."
	exit 1
fi
sleep 20

# look at the standard PostgreSQL database exports
for database_export in `ls -1 *.db`;
do
	echo "Restoring database '${database_export}' ..."
	${PG_RESTORE} -C -d postgres -U ${POSTGRESQL_USER} ${database_export} >> errors.txt 2>&1
	
	database=`basename ${database_export} .db`
	
	# vaccum the database
	${VACUUMDB} -d ${database} -U ${POSTGRESQL_USER} -p ${POSTGRESQL_PORT} -vz >> errors.txt 2>&1  
done
echo ""
# look at the postgis enabled PostgreSQL database exports
for database_export in `ls -1 *.db.postgis`;
do
	echo "Performing a postgis restoration of database '${database_export}' ..."

	# restore the database
	${PG_RESTORE} -C -d postgres -U ${POSTGRESQL_USER} ${database_export} >> errors.txt 2>&1
	# vacuum the database.

	database=`basename ${database_export} .db.postgis`

	# vaccum the database
	${VACUUMDB} -d ${database} -U ${POSTGRESQL_USER} -p ${POSTGRESQL_PORT} -vz >> errors.txt 2>&1

	/bin/bash run_postgis_upgrade.sh ${database}
done

# stop PostgreSQL
/sbin/service edex_postgres stop
if [ $? -ne 0 ]; then
	echo "WARNING: Failed to stop PostgreSQL."
else
   sleep 20
fi

# restore the original postgresql.conf
pushd . > /dev/null 2>&1
cd ${AWIPS2_DATA_DIRECTORY}

cp -f postgresql.conf.orig postgresql.conf
if [ $? -ne 0 ]; then
	echo "Failed to restore postgresql.conf. Original version is: ${AWIPS2_DATA_DIRECTORY}/postgresql.conf.orig"
	exit 0
fi

rm -f postgresql.conf.orig
if [ $? -ne 0 ]; then
	echo "Failed to remove backup postgresql.conf. Backup is: ${AWIPS2_DATA_DIRECTORY}/postgresql.conf.orig"
fi

popd > /dev/null 2>&1

exit 0