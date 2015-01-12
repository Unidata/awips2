#!/bin/bash
# -----------------------------------------------------------------
# ! script to load stations tables into the NCEP database
# !
# ! $1 = install directory
# ! $2 = DB port number
# ! $3 = username
# ! $4 = script directory
# ! $5 = log file path
# !
# -----------------------------------------------------------------
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxAviationforecastsBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxCPCProductsBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxFlashFloodBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxHPCHeatIndexBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxHPCProductsBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxMarineBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxMasterAndGuiProducts.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxMOSBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxNHCProductsBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxObservedDataBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxPtfcstProductsBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxPublicProductsBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxReconCARCAHBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxSPCProductsBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxTropicalPacificBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxVolcanoProductsBulletinTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNwxAdminMessagesBulletinTables.sql  >> ${5} 2>&1
