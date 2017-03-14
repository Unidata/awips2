#!/bin/bash

echo "INFO: update started - removing delivery options from Subscription Manager localization files"

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

echo "INFO: the update has completed successfully!"

exit 0
