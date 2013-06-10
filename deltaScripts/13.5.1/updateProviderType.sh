#!/bin/bash

XSLT_SCRIPT="updateProviderType.xsl"
# ensure that the xslt script is present
if [ ! -f ${XSLT_SCRIPT} ]; then
   echo "ERROR: the required xslt script - ${XSLT_SCRIPT} was not found."
   echo "FATAL: the update has failed!"
   exit 1
fi

echo "INFO: update started - updating ProviderType to be a class proper"

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

# Dump the provider rows from the database for modification
PROVIDER_ROWS=/tmp/provider_rows.tmp
psql -U awips -d metadata -c "\copy (select key, stringvalue from ebxml.value where stringvalue like '%<provider %') To '${PROVIDER_ROWS}'";

# Get old separator
OIFS=$IFS

IFS=$'\n'
for f in `cat ${PROVIDER_ROWS}`
do
   IFS=$'\t'
   arr2=( $f )

   KEY=${arr2[0]}
   XML_FILE=/tmp/${KEY}.xml

   # Write out database contents
   echo "${arr2[1]}" > ${XML_FILE}
   # Remove carriage returns
   sed -i 's/\\n//g' ${XML_FILE}

   # Run the xslt transform on the tmp file
   xsltproc ${XSLT_SCRIPT} ${XML_FILE} > ${XML_FILE}.new

   # Insert the new xml into the database
   NEW_XML=`cat ${XML_FILE}.new`
   psql -U awips -d metadata -c "UPDATE ebxml.value SET stringvalue = '${NEW_XML}' WHERE key = '${KEY}'"
done

# Restore old separator
IFS=$OIFS

echo "INFO: the update has completed successfully!"

exit 0
