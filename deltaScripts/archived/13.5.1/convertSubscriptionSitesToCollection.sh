#!/bin/bash

SQL_SCRIPT="getSubscriptionSite.sql"

# ensure that the sql script is present
if [ ! -f ${SQL_SCRIPT} ]; then
   echo "ERROR: the required sql script - ${SQL_SCRIPT} was not found."
   echo "FATAL: the update has failed!"
   exit 1
fi

echo "INFO: update started - converting Subscription sites to a collection"

# run the update
OUTPUT=`/awips2/psql/bin/psql -U awips -d metadata -f ${SQL_SCRIPT}`
SITE=`echo $OUTPUT | sed 's/.*{\(.*\)}.*/\1/g'`
if [ $? -ne 0 ]; then
   echo "FATAL: the update has failed!"
   exit 1
fi

# If we were able to find a subscription with a site, then this update will run.
# Otherwise, there are no subscriptions to convert.
if test "${SITE#*awips}" != "$SITE"
then
   echo "INFO: there are no subscriptions in the database, not applying update."
   exit 0
fi

SQL_SCRIPT="convertSubscriptionSitesToCollection.sql"

# ensure that the sql script is present
if [ ! -f ${SQL_SCRIPT} ]; then
   echo "ERROR: the required sql script - ${SQL_SCRIPT} was not found."
   echo "FATAL: the update has failed!"
   exit 1
fi

# Copy the sql file, replacing the SITE token with the actual site
cat ${SQL_SCRIPT} | sed "s/@@SITE@@/${SITE}/g" > /tmp/${SQL_SCRIPT}

# run the update
/awips2/psql/bin/psql -U awips -d metadata -f /tmp/${SQL_SCRIPT}
if [ $? -ne 0 ]; then
   echo "FATAL: the update has failed!"
   exit 1
fi

# Remove temporary file
rm /tmp/${SQL_SCRIPT}

echo "INFO: the update has completed successfully!"

exit 0
