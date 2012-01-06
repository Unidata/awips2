##!/bin/sh

echo "**************************************************************"
echo " Starting AWIPS on DB Server"
echo "**************************************************************"
echo "Getting variables from env.txt"
. env.txt

echo "----Checking to make sure we can see the installer on awipscm----"

echo "----Starting Postgres----"
echo "/etc/init.d/edex_postgres_${POSTGRESPORT} start"
/etc/init.d/edex_postgres_${POSTGRESPORT} start
sleep 5

echo "----Checking to see if the Postgres Process has started----"
echo "PID='/sbin/pidof postmaster'"
PID=`/sbin/pidof postmaster`
if [ "$PID" = "" ]
then
	echo "----Postgres PID not found----"
	echo "----Sending Warning Email----"
	echo "`uname -n`: At `date`, EDEX DB Server Auto Start script failed to start Postgres--before continuing the install--the db server should be checked." > email.txt
	mail -s "Warning EDEX DB Server start failed on `uname -n`" ${EMAILLIST} < email.txt
else 
	echo "Postgres PID: $PID"
fi

echo "----Starting IRT----"
/etc/init.d/edex_irt start
sleep 20

echo "----Checking to see if the IRT Process has started----"
echo "PID='/sbin/pidof $EDEXINSTALLFOLDER/bin/python RoutingTableSvc.py'"
PID=`/sbin/pidof ${EDEXINSTALLFOLDER}/bin/python RoutingTableSvc.py`
if [ "$PID" = "" ]
then
	echo "----IRT Server PID not found----"
	echo "----Sending Warning Email----"
	echo "`uname -n`: At `date`, EDEX DB Server Auto Start script failed to start the IRT Server--before continuing the install--the db server should be checked." > email.txt
	mail -s "Warning EDEX DB Server start failed on `uname -n`" ${EMAILLIST} < email.txt
else
	echo "IRT PID: $PID"
fi

echo "----Importing IHFS Data----"
$EDEXINSTALLFOLDER/bin/psql -q -U ${EDEXUSER} -p ${POSTGRESPORT} -d ${IHFS} < ${IHFS_SQL_FILE} 
# rm ${IHFS_SQL_FILE}
echo "----IHFS Import Complete----"

echo "**********************************************************"
echo "Finished Starting AWIPS on DB Server"
echo "**********************************************************"
