#!/bin/bash
# DR #3318 - this update awips.metadata.grid_info and needs to run on dx1.

echo 'INFO: Update gridinfo'

/awips2/psql/bin/psql -U awips -d metadata -c "update grid_info set
datasetid='GEFS' where datasetid='gefs'"

if [ $? -ne 0 ] ; then
	echo 'ERROR unable to update database'
	exit 1
fi

echo 'INFO: Update gridinfo successful.'

echo 'INFO: Has renameGefs.py been run on dx2?'



