#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from modelsounding, obs, profiler,
#            sfcobs, svrwx, tcg, tcsq

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=modelsounding

# table and constraint names from SoundingSite.
echo "INFO: Start update of ${table} dataURI columns."
col=reportType
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

table=profiler
# table and constraint names from ProfilerObs.
echo "INFO: Start update of ${table} dataURI columns."
col=reportType
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

table=sfcobs
# table and constraint names from ObsCommon
echo "INFO: Start update of ${table} dataURI columns."
col=reportType
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
col=corIndicator
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "ALTER TABLE ${table} ALTER COLUMN ${col} TYPE varchar(1) ; "
${PSQL} -U awips -d metadata -c "UPDATE  ${table} SET ${col}='' where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

table=svrwx
# table and constraint names from SvrWxRecord.
echo "INFO: Start update of ${table} dataURI columns."
col=reportType
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

table=tcg
# table and constraint names from TropicalCycloneGuidance.
echo "INFO: Start update of ${table} dataURI columns."
col=producttype
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
col=modelName
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

table=tcs
# table and constraint names from TropicalCycloneSummary.
echo "INFO: Start update of ${table} dataURI columns."
col=producttype
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

table=obs
# table and constraint names from MetarRecord.
echo "INFO: Start update of ${table} dataURI columns."
col=reporttype
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "UPDATE  ${table} SET ${col}='null' where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
col=correction
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "UPDATE  ${table} SET ${col}='null' where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

