#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from tables with embedded SurfaceObsLocation.

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh

# DataURI Embedded SurfaceObsLocation that are non-nullable
embStrCols=("stationid")
embFloatCols=("latitude" "longitude")
embClass="SurfaceObsLocation"

function updateEmbCols {
	table=${1}
	echo "INFO: Update embedded ${embClass} in table ${table}."
	for col in ${embStrCols[@]} ; do
		${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}='' where ${col} is NULL ; "
		updateNotNullCol ${table} ${col}
	done
	for col in ${embFloatCols[@]} ; do
		${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}='NaN' where ${col} is NULL ; "
		updateNotNullCol ${table} ${col}
	done
	echo "INFO: Finish update of embedded ${embClass} in table ${table}."
}

# table and constraint names from ACARSSoundingRecord
table=acarssounding
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}

# table and constraint names from AScatObs
table=bufrascat
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
echo "INFO: ${table} dataURI columns updated successfully"

col=windSpd
echo "INFO: Update ${table}'s ${col}"

${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}=-9999.0 where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=satId
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from BufrHDWObs
table=bufrhdw
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}

col=pressure
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=satType
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from BufrMTHDWObs.
table=bufrmthdw
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
echo "INFO: ${table} dataURI columns updated successfully"

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

# table and constraint names from BUFRncwf
table=bufrncwf
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from QUIKScatObs
table=bufrquikscat
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from SSMIScanData
table=bufrssmi
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}

col=satid
echo "INFO: Update ${table}'s ${col}"
# The IDecoderConstants.VAL_MISSING used by SSMIDataAdapter.
${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}=-9999998 where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from UAObs
table=bufrua
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from FSSObsRecord
table=fssobs
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from GOESSounding
table=goessounding
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from  ManualLdadRecord
table=ldad_manual
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from HydroLdadRecord
table=ldadhydro
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from MesonetLdadRecord.
table=ldadmesonet
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}

col=reportType
echo "INFO: Update ${table}'s ${col}"

${PSQL} -U awips -d metadata -c "DELETE FROM ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=dataProvider
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from ProfilerLdadObs.
table=ldadprofiler
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}

# table and constraint names form LocalStormReport.
table=lsr
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
col=eventtype
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from MadisRecord.
table=madis
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
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


# table and constraint names from SoundingSite.
table=modelsounding
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
col=reportType
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from MetarRecord.
table=obs
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
col=reporttype
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "UPDATE  ${table} SET ${col}='' where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
col=correction
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "UPDATE  ${table} SET ${col}='' where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from POESSounding.
table=poessounding
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from ProfilerObs.
table=profiler
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
col=reportType
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from QCRecord.
table=qc
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
# Column already non-null
#col=qcType
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from ObsCommon
table=sfcobs
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
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

# table and constraint names from SvrWxRecord.
table=svrwx
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
col=reportType
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from TropicalCycloneGuidance.
table=tcg
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
col=producttype
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
col=modelName
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"

# table and constraint names from TropicalCycloneSummary.
table=tcs
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}
col=producttype
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}
echo "INFO: ${table} dataURI columns updated successfully"


# table and constraint names from VAARecord.
table=vaa
echo "INFO: Start update of ${table} dataURI columns."
updateEmbCols ${table}

col=advisorynumber
echo "INFO: Update ${table}'s ${col}"
# Value from VAAParser.
${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}='' where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

# NCEP tables only need embedded SurfaceObsLocation updated.
tables=("ncpafm" "ncscd" "nctaf" "ncuair")

for table in ${tables[@]} ; do
	if tableExists ${table} ; then
		echo "INFO: Start update of ${table} dataURI columns."
		updateEmbCols ${table}
		echo "INFO: ${table} dataURI columns updated successfully"
	else 
		echo "WARN: Table ${table} does not exist."
	fi
done

echo "INFO: Finish update embedded SurfaceObsLocation's non-nullable columns"
