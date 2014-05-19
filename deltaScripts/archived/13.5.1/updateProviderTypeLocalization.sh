#!/bin/bash

XSLT_SCRIPT="updateProviderType.xsl"
# ensure that the xslt script is present
if [ ! -f ${XSLT_SCRIPT} ]; then
   echo "ERROR: the required xslt script - ${XSLT_SCRIPT} was not found."
   echo "FATAL: the update has failed!"
   exit 1
fi

echo "INFO: update started - updating ProviderType to be a class proper in localization files"

# Update subscription manager configuration files
for FILE in `find /awips2/edex/data/utility/common_static -iname \*-harvester.xml`
do
    cp $FILE $FILE.bak
    xsltproc ${XSLT_SCRIPT} ${FILE}.bak > ${FILE}
 
   # Make sure each command succeeds
   if [ $? -ne 0 ]; then
     echo "FATAL: the update has failed!"
     exit 1
   fi
 
   # Delete the md5 file
   rm $FILE.md5
done

echo "INFO: the update has completed successfully!"

exit 0
