#!/bin/bash
# DR #3722 - this update script will drop the dataURI column from taf

PSQL="/awips2/psql/bin/psql"
SQL_FILE="/tmp/DR3722.sql"

# Creates new columns to replace corindicator and amdindicator
function addNewTafColumns {
   if [[ "$ftype" != "boolean" ]]; then
      echo "INFO: Converting taf corindicator and amdindicator to temporary boolean fields"
      echo "ALTER TABLE taf ADD COLUMN corindicator_temp boolean NOT NULL DEFAULT false;" >> $SQL_FILE
      echo "ALTER TABLE taf ADD COLUMN amdindicator_temp boolean NOT NULL DEFAULT false;" >> $SQL_FILE
      echo "UPDATE taf set corindicator_temp = true where corindicator = 'COR';" >> $SQL_FILE
      echo "UPDATE taf set corindicator_temp = true where corindicator = 'true';" >> $SQL_FILE
      echo "UPDATE taf set amdindicator_temp = true where amdindicator = 'AMD';" >> $SQL_FILE
      echo "UPDATE taf set amdindicator_temp = true where amdindicator = 'true';" >> $SQL_FILE

      ${PSQL} -U awips -d metadata -f $SQL_FILE
      if [ $? -ne 0 ]; then
         echo "ERROR: Failed to generate new corindicator and amdindicator columns for table $1.  Commands that failed at $SQL_FILE"
         echo "FATAL: The update has failed."
         exit 1
      fi

      rm $SQL_FILE
   fi
}

# Drops duplicates utilizing new indicator columns and also deletes any rows that null datauri fields that are no longer allowed to be null
function deleteTafData {
   echo "INFO: Dropping any taf duplicates keeping the earliest insert, also dropping any invalid rows containing invalid NULL fields"

   temp=""
   if [[ "$ftype" != "boolean" ]]; then
      temp="_temp"
   fi

   query="SELECT distinct b.id FROM taf a, taf b WHERE (a.reftime = b.reftime AND a.stationid = b.stationid AND a.corindicator$temp = b.corindicator$temp AND a.amdindicator$temp = b.amdindicator$temp AND a.issue_timestring = b.issue_timestring AND ((a.inserttime < b.inserttime) or (a.inserttime = b.inserttime and a.id < b.id))) or (b.reftime isnull) or (b.stationid isnull) or (b.issue_timestring isnull)"

   echo "   INFO: Finding taf entries to delete"
   result=(`${PSQL} -U awips -d metadata -t -A -c "$query"`)
   numEntries="${#result[@]}"

   if [[ "${numEntries}" > 0 ]]; then
      echo "   INFO: Found $numEntries to delete"
      taf_ids="${result[0]}"

      if [[ "${numEntries}" > 1 ]]; then
         for id in "${result[@]:1}"
         do
            taf_ids+=", $id"
         done
      fi

      # handle cascade tables
      echo "SELECT distinct id from taf_change_groups where parentid in ($taf_ids)" > $SQL_FILE

      echo "   INFO: Finding cascaded taf_change_group entries"
      result=(`${PSQL} -U awips -d metadata -t -A -f $SQL_FILE`)

      numEntries="${#result[@]}"

      echo "" > $SQL_FILE

      if [[ "${numEntries}" > 0 ]]; then
         echo "   INFO: Found $numEntries to delete"
         taf_change_ids="${result[0]}"

         if [[ "${numEntries}" > 1 ]]; then
            for id in "${result[@]:1}"
            do
               taf_change_ids+=", $id"
            done
         fi

         echo "DELETE FROM taf_icing_layers where parentid in ($taf_change_ids);" >> $SQL_FILE
         echo "DELETE FROM taf_sky_cover where parentid in ($taf_change_ids);" >> $SQL_FILE
         echo "DELETE FROM taf_temperature_forecasts where parentid in ($taf_change_ids);" >> $SQL_FILE
         echo "DELETE FROM taf_turbulence_layers where parentid in ($taf_change_ids);" >> $SQL_FILE
         echo "DELETE FROM taf_weather_conditions where parentid in ($taf_change_ids);" >> $SQL_FILE
         echo "DELETE FROM taf_change_groups where id in ($taf_change_ids);" >> $SQL_FILE
      fi

      echo "DELETE FROM taf where id in ($taf_ids)" >> $SQL_FILE

      echo "   INFO: Deleting data"
      ${PSQL} -U awips -d metadata -f $SQL_FILE
      if [ $? -ne 0 ]; then
         echo "ERROR: Failed to delete duplicate and invalid data for taf tables.  Commands that failed at $SQL_FILE"
         echo "FATAL: The update has failed."
         exit 1
      fi

      rm $SQL_FILE
   else
      echo "   INFO: Found no entries to delete"
   fi
}

# takes two args: table, old constraint name
# Drops the prior scripts unique constraint, the current unique constraint,
# old amdindicator and corindicator columns, and renames temp columns
function dropConstraintsAndRenameColumns {
   echo "INFO: Dropping $1 unique constraints if exists.  Replacing original corindicator and amdindicator with boolean fields."
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP CONSTRAINT IF EXISTS taf_reftime_stationid_corindicator_amdindicator_issuetimestring_key;"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP CONSTRAINT IF EXISTS uk_fs43xfrjmc8lk31lxp3516eh3;"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP CONSTRAINT IF EXISTS $2;"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 ALTER COLUMN stationid SET NOT NULL;"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 ALTER COLUMN issue_timestring SET NOT NULL;"

   if [[ "$ftype" != "boolean" ]]; then
      ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP COLUMN corindicator;"
      ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP COLUMN amdindicator;"
      ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 RENAME COLUMN corindicator_temp to corindicator;"
      ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 ALTER COLUMN corindicator DROP DEFAULT;"
      ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 RENAME COLUMN amdindicator_temp to amdindicator;"
      ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 ALTER COLUMN amdindicator DROP DEFAULT;"
   fi
}

# takes one arg: a table name
# drops the datauri constraint and column if they exist
function dropDatauri {
   echo "INFO: Dropping DataURI column from $1"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP CONSTRAINT IF EXISTS ${1}_datauri_key;"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP COLUMN IF EXISTS datauri;"

   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to drop dataURI column for $table"
      echo "FATAL: The update has failed."
      exit 1
   fi
}

# takes three args: table, constraint name, unique columns
# will first drop the constraint if it exists and then adds it back, this is
# fairly inefficient if it does exist but operationally it won't exist and for
# testing this allows the script to be run easily as a noop.
function dropDatauriAndAddConstraint {
   dropDatauri $1
   echo "INFO: Adding unique constraint"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP CONSTRAINT IF EXISTS $2;"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 ADD CONSTRAINT $2 UNIQUE $3;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to add new unique constraint for $table"
      echo "FATAL: The update has failed."
      exit 1
   fi
}

ftype=`${PSQL} -d metadata -U awips -t -A -c "select data_type from information_schema.columns where table_name='taf' and table_schema='awips' and column_name='corindicator';"`

# delete previous file
if [[ -f $SQL_FILE ]]; then
   rm $SQL_FILE

   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to delete prior sql file $SQL_FILE"
      echo "FATAL: The update has failed."
      exit 1
   fi
fi

# make sure can write to correct file
echo "" > $SQL_FILE
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to write to sql file $SQL_FILE"
   echo "FATAL: The update has failed."
   exit 1
fi

addNewTafColumns
deleteTafData
dropConstraintsAndRenameColumns taf uk_taf_datauri_fields
dropDatauriAndAddConstraint taf uk_taf_datauri_fields "(reftime, stationid, corindicator, amdindicator, issue_timestring)"
${PSQL} -U awips -d metadata -c "DROP INDEX IF EXISTS taf_reftimeindex;"
${PSQL} -U awips -d metadata -c "CREATE INDEX taf_reftimeindex ON taf USING btree (reftime);"
${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE taf"

echo "INFO: taf dataURI columns dropped successfully"
