#! /bin/bash
#
# This delta script should be run on dv3 prior to starting EDEX.
# It updates the schema of the gfe_gridlocation table for 20.3.1.
# It needs to be run on an EDEX processing server so it can access the EDEX Postgres SSL certs. 
#
# This script requires the UpdateGridLocation.jar file to be located
# in the same directory as the script.
#

# source the edex setup.env file to get DB_HOST
source /awips2/edex/bin/setup.env

dir=`dirname $0`
java -jar ${dir}/UpdateGridLocation.jar -host ${DB_HOST} -certDirPath /awips2/edex/conf/db/auth/