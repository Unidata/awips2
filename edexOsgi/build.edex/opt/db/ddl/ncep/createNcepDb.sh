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
echo -----------------------------------------------------
echo  \| Creating NCEP database and tables...
echo -----------------------------------------------------

${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/createNcepConfigTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/createNcepSatTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/createNcepStnsTables.sql  >> ${5} 2>&1
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/createNcepNwxTables.sql  >> ${5} 2>&1
#${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/createNcepNcgribTables.sql  >> ${5} 2>&1
echo -----------------------------------------------------
echo \| Populating NCEP database and tables...
echo -----------------------------------------------------

echo -----------------------------------------------------
echo \| Populating config tables...
echo -----------------------------------------------------
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadConfigClo.sql  >> ${5} 2>&1

#echo -----------------------------------------------------
#echo \| Populating ncgrib tables...
#echo -----------------------------------------------------
#${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadVcrdgrib1.sql  >> ${5} 2>&1

echo -----------------------------------------------------
echo \| Populating sat tables...
echo -----------------------------------------------------
${1}/bin/psql -d ncep -U ${3} -q -p ${2} -f ${4}/loadNcSat.sql  >> ${5} 2>&1

echo -----------------------------------------------------
echo \| Populating stns tables...
echo -----------------------------------------------------
${4}/loadNcepStns.sh ${1} ${2} ${3} ${4} ${5}

echo -----------------------------------------------------
echo \| Populating nwx tables...
echo -----------------------------------------------------
${4}/loadNcepNwx.sh ${1} ${2} ${3} ${4} ${5}

