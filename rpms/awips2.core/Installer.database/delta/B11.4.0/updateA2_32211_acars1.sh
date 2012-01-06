#!/bin/bash

export DELTA_BUILD="11.4"
export DELTA_ID="updateA2_32211_acars1"
export DELTA_DESC="drop acarssoundinglayer and acarssounding tables due to schema change"

export DELTA_RUN_USER="awips"

function runUpdate()
{
   local PSQL_INSTALL=`rpm -q --queryformat '%{INSTALLPREFIX}\n' awips2-psql`

   local PSQL="${PSQL_INSTALL}/bin/psql -U awips -d metadata -c"


   # Drop acarssoundinglayer
   ${PSQL} "drop table acarssoundinglayer;" > /dev/null 2>&1

   # Drop acarssounding
   ${PSQL} "drop table acarssounding;" > /dev/null 2>&1

   # Update plugin_info
   ${PSQL} "update plugin_info set initialized=false where name='acarssounding';" > /dev/null 2>&1


   return 0
}
