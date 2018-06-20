#!/bin/bash
# -----------------------------------------------------------------
# ! script to create the NCEP database
# !
# ! $1 = install directory
# ! $2 = DB port number
# ! $3 = username
# ! $4 = script directory
# ! $5 = log file path
# !
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/createNcepConfigTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/createNcepSatTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/createNcepStnsTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/createNcepNwxTables.sql  >> ${5} 2>&1
#${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/createNcepNcgribTables.sql  >> ${5} 2>&1
# Populating config tables
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadConfigClo.sql  >> ${5} 2>&1
# Populating ncgrib tables
#${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadVcrdgrib1.sql  >> ${5} 2>&1
# Populating sat tables
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNcSat.sql  >> ${5} 2>&1
# Populating stns tables
${4}/loadNcepStns.sh ${1} ${2} ${3} ${4} ${5} 2>&1
# Populating nwx tables
${4}/loadNcepNwx.sh ${1} ${2} ${3} ${4} ${5} 2>&1
