#!/bin/bash

export DELTA_BUILD="11.4"
export DELTA_ID="updateA2_DR7018_ActiveTable1"
export DELTA_DESC="Expand ActiveTableRecord.overviewtext hibernate field."

export DELTA_RUN_USER="awips"

function runUpdate()
{
   local PSQL_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}\n' awips2-psql`

   local PSQL="${PSQL_INSTALL}/bin/psql -U awips -d metadata -c" > /dev/null 2>&1

   ${PSQL} "ALTER TABLE activetable ALTER COLUMN overviewtext TYPE text;"

   return 0
}
