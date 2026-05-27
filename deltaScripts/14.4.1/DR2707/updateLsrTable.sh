#!/bin/bash

# eventType was stored as the numeric ID of an enum
# this does not cover all event types and storing the raw string is prefered

DBUSER="awips"
DBNAME="metadata"

TABLE_NAME='lsr'
COLUMN_NAME='eventtype'
BACKUP_NAME='eventtype_old'
DATAURI_COLUMN='datauri'
STATION_COLUMN='stationid'
LAT_COLUMN='latitude'
LON_COLUMN='longitude'

CONSTRAINT_NAME='latitude_longitude_stationId_refTime_forecastTime_eventType'
CONSTRAINT_COLUMNS='latitude, longitude, stationId, refTime, forecastTime, eventType'

PSQL="/awips2/psql/bin/psql"

SQL_STATEMENT="
BEGIN;
ALTER TABLE ${TABLE_NAME} RENAME COLUMN ${COLUMN_NAME} to ${BACKUP_NAME};
ALTER TABLE ${TABLE_NAME} ADD COLUMN ${COLUMN_NAME} character varying(255);
UPDATE ${TABLE_NAME}
SET ${COLUMN_NAME} =
CASE ${BACKUP_NAME}
	WHEN 0 then 'AVALANCHE'
	WHEN 1 then 'BLIZZARD'
	WHEN 2 then 'DENSE FOG'
	WHEN 3 then 'DOWNBURST'
	WHEN 4 then 'DROUGHT'
	WHEN 5 then 'DUST STORM'
	WHEN 6 then 'EXCESSIVE HEAT'
	WHEN 7 then 'EXTREME COLD'
	WHEN 8 then 'EXTR WIND CHILL'
	WHEN 9 then 'FLASH FLOOD'
	WHEN 10 then 'FLOOD'
	WHEN 11 then 'FREEZE'
	WHEN 12 then 'FREEZING RAIN'
	WHEN 13 then 'FUNNEL CLOUD'
	WHEN 14 then 'HAIL'
	WHEN 15 then 'HEAVY RAIN'
	WHEN 16 then 'HEAVY SLEET'
	WHEN 17 then 'HEAVY SNOW'
	WHEN 18 then 'HIGH ASTR TIDES'
	WHEN 19 then 'HIGH SURF'
	WHEN 20 then 'HIGH SUST WINDS'
	WHEN 21 then 'HURRICANE'
	WHEN 22 then 'ICE STORM'
	WHEN 23 then 'LIGHTNING'
	WHEN 24 then 'LOW ASTR TIDES'
	WHEN 25 then 'MARINE HAIL'
	WHEN 26 then 'MARINE TSTM WIND'
	WHEN 27 then 'NON-TSTM WND DMG'
	WHEN 28 then 'NON-TSTM WND GST'
	WHEN 29 then 'RIP CURRENTS'
	WHEN 30 then 'SEICHE'
	WHEN 31 then 'SLEET'
	WHEN 32 then 'SNOW'
	WHEN 33 then 'STORM SURGE'
	WHEN 34 then 'TORNADO'
	WHEN 35 then 'TROPICAL STORM'
	WHEN 36 then 'TSTM WND DMG'
	WHEN 37 then 'TSTM WND GST'
	WHEN 38 then 'WATER SPOUT'
	WHEN 39 then 'WILDFIRE'
	WHEN 40 then 'FREEZING DRIZZLE'
	WHEN 41 then 'COASTAL FLOOD'
	WHEN 42 then 'DEBRIS FLOW'
	WHEN 43 then 'BLOWING SNOW'
	WHEN 44 then 'RAIN'
    ELSE ''
END;
UPDATE ${TABLE_NAME} set ${STATION_COLUMN} = concat(${LON_COLUMN}, ':', ${LAT_COLUMN});
ALTER TABLE ${TABLE_NAME} DROP COLUMN ${BACKUP_NAME};
ALTER TABLE ${TABLE_NAME} DROP COLUMN ${DATAURI_COLUMN};
ALTER TABLE ${TABLE_NAME} ADD CONSTRAINT ${CONSTRAINT_NAME} UNIQUE (${CONSTRAINT_COLUMNS});
COMMIT;"

COLUMN_TYPE=$(${PSQL} -U ${DBUSER} -d ${DBNAME} -tc "select data_type from INFORMATION_SCHEMA.COLUMNS where table_name = '${TABLE_NAME}' and column_name = '${COLUMN_NAME}'")

if [[ $COLUMN_TYPE =~ integer ]]
then
    if ${PSQL} -U ${DBUSER} -d ${DBNAME} -c "${SQL_STATEMENT}"
    then
        echo "${TABLE_NAME} updated successfully, vacuuming table"
        ${PSQL} -U ${DBUSER} -d ${DBNAME} -c "VACUUM FULL ANALYZE ${TABLE_NAME}"
    else
        echo "Update failed on ${TABLE_NAME}"
    fi
else
    echo "$TABLE_NAME already updated, no changes made"
fi
