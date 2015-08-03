#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from modelsounding, obs, profiler,
#            sfcobs, svrwx, tcg, tcsq

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=bufrascat

# table and constraint names from AScatObs.
echo "INFO: Start update of ${table} dataURI columns."

col=windSpd
echo "INFO: Update ${table}'s ${col}"

# Default value from BUFRPointDataAdapter.
${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}=-9999.0 where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=satId
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

table=bufrhdw

# table and constraint names from .BufrHDWObs
echo "INFO: Start update of ${table} dataURI columns."

col=pressure
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=satType
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

table=bufrmthdw

# table and constraint names from BufrMTHDWObs.
echo "INFO: Start update of ${table} dataURI columns."

col=pressure
echo "INFO: Update ${table}'s ${col}"
# The IDecoderConstants.VAL_MISSING used by MTHDWDataAdapter.
${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}=-9999998 where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=satType
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

# No column updates only dataURI in @UniqueConstraint
#table=bufrquikscat

table=bufrssmi

# table and constraint names from BufrMTHDWObs.
echo "INFO: Start update of ${table} dataURI columns."

col=satid
echo "INFO: Update ${table}'s ${col}"
# The IDecoderConstants.VAL_MISSING used by SSMIDataAdapter.
${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}=-9999998 where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

# No column updates only dataURI in @UniqueConstraint
#table=bufrua
#table=fssobs
#table=ldad_manual
#table=ldadhydro

table=ldadmesonet

# table and constraint names from MesonetLdadRecord.
echo "INFO: Start update of ${table} dataURI columns."

col=reportType
echo "INFO: Update ${table}'s ${col}"

${PSQL} -U awips -d metadata -c "DELETE FROM ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=dataProvider
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

# No column updates only dataURI in @UniqueConstraint
#table=ldadprofiler

table=lsr

# table and constraint names form LocalStormReport.
echo "INFO: Start update of ${table} dataURI columns."
col=eventtype
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

table=madis

# table and constraint names from MadisRecord.
echo "INFO: Start update of ${table} dataURI columns."
col=provider
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=subProvider
echo "Info Update ${table}'s ${col}"
# When subProvider missing MadisDecoder already enters the string "null"; therefore should never have a NULL entry.
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=restriction
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

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

# No column updates only dataURI in @UniqueConstraint
#table=poessounding

#table=qc
# Columan already non-null
#col=qcType

table=vaa

# table and constraint names from VAARecord.
echo "INFO: Start update of ${table} dataURI columns."

col=advisorynumber
echo "INFO: Update ${table}'s ${col}"
# Value from VAAParser.
${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}='null' where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

# Tables with embedded SurfaceObsLocation
tables=("acarssounding" "bufrascat" "bufrhdw" "bufrmthdw" "bufrncwf" "bufrquikscat" "bufrssmi"
"bufrua" "fssobs" "goessounding" "ldad_manual" "ldadhydro" "ldadmesonet" "ldadprofiler" "lsr"
"madis" "modelsounding" "obs" "poessounding" "profiler" "qc" "sfcobs" "svrwx" "tcg" "tcs" "vaa")

# DataURI Embedded SurfaceObsLocation that are non-nullable
strCols=("stationId")
floatCols=("latitude" "longitude")

echo "INFO: Start update embedded SurfaceObsLocation's non-nullable columns"
for table in ${tables[@]} ; do
	echo "INFO: Updating ${table}"
	for col in ${strCols[@]} ; do
		${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}='null' where ${col} is NULL ; "
		updateNotNullCol ${table} ${col}
	done
	for col in ${floatCols[@]} ; do
		${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}='NaN' where ${col} is NULL ; "
		updateNotNullCol ${table} ${col}
	done
done
echo "INFO: Finish update embedded SurfaceObsLocation's non-nullable columns"
