#!/bin/bash

SQL_SCRIPT="removeDeliveryOptions.sql"
# ensure that the sql script is present
if [ ! -f ${SQL_SCRIPT} ]; then
   echo "ERROR: the required sql script - ${SQL_SCRIPT} was not found."
   echo "FATAL: the update has failed!"
   exit 1
fi

echo "INFO: update started - removing delivery options from Subscription Manager"

# Update subscription manager configuration files
for DIRECTORY in `find /awips2/edex/data/utility/cave_static/ -type d -name subscriptionManagerConfig`
do
   for FILE in `find $DIRECTORY -type f -name *.xml`
   do
      # Comments out the Delivery/Notify column entry, in case it needs to be restored (rollback scenario)
      sed -i 's/\(.*\)<column\(.*Delivery.*\)\/>/\1<!-- column\2\/-->/' $FILE
      
      # Make sure each sed command succeeds
      if [ $? -ne 0 ]; then
        echo "FATAL: the update has failed!"
        exit 1
      fi
      
      # Delete the md5 file
      rm $FILE.md5
   done
done

# Update the database
/awips2/psql/bin/psql -U awips -d metadata -f ${SQL_SCRIPT}
if [ $? -ne 0 ]; then
   echo "FATAL: the update has failed!"
   exit 1
fi

echo "INFO: the update has completed successfully!"

exit 0
