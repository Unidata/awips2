#!/bin/bash

# DCS 19644 - adds a new column "ts" (type-source) as part of primary key
#             onto the locdatalimits table. The new "TS" column is placed after
#             column "dur".
#
# Author: jwu
# April 3rd, 2018

# DR 21004 - Move column ts to the end of table, not after column "dur"

psql=/awips2/psql/bin/psql

echo INFO: Adding column ts into table locdatalimits.

# Get hydro database name(s)
database=$(${psql} -d metadata -U awipsadmin -Aqtc "
    SELECT datname FROM pg_database
        WHERE datistemplate = false
        AND datname LIKE 'hd_ob%'
    ")

# Default TS
default_ts='NA'

# Update locdatalimits table in each hydro DB (though normally only one like 'hd_ob92oax')
for hydrodb in ${database}
    do
        # Check if column ts exists.
        has_column=$(${psql} -U awipsadmin -d $hydrodb -Atc "
            SELECT EXISTS(
                SELECT 1 FROM information_schema.columns c
                WHERE c.table_name = 'locdatalimits'
                AND c.column_name = 'ts'
                );")

        has_na_ts=$(${psql} -U awipsadmin -d  $hydrodb -Atc "
            SELECT EXISTS(
                SELECT 1 FROM shefts
                WHERE ts = '$default_ts');")

        # A new row 'NA" is added into shefts table to allow locdatalimits to
        # define a default location limit for all 'ts'. This type of ts
        # will be ignored in other places.
        if [[ "${has_na_ts}" == "f" ]]; then
            ${psql} -U awipsadmin -d $hydrodb -Atc "
                BEGIN TRANSACTION;
                INSERT INTO shefts (ts, name) VALUES ('$default_ts', 'Not Applicable');
                COMMIT TRANSACTION;"
        fi

        # If column ts not exist, create a new table similar to locdatalimits
        # but add ts column after column dur and initial it as NA. Then rename
        # original table to locdatalimitsold and rename the new table as
        # locdatalimits.
        #
        # DR 21004 - Move column ts to the end of table, not after column "dur"
        if [[ "${has_column}" == "f" ]]; then
            ${psql} -U awipsadmin -d $hydrodb -Atc "
               BEGIN TRANSACTION;
               CREATE TABLE locdatalimitsnew
               (
                   lid character varying(8) NOT NULL,
                   pe character varying(2) NOT NULL,
                   dur smallint NOT NULL,
                   monthdaystart character varying(5) NOT NULL,
                   monthdayend character varying(5) NOT NULL,
                   gross_range_min double precision,
                   gross_range_max double precision,
                   reason_range_min double precision,
                   reason_range_max double precision,
                   roc_max double precision,
                   alert_upper_limit double precision,
                   alert_roc_limit double precision,
                   alarm_upper_limit double precision,
                   alarm_roc_limit double precision,
                   alert_lower_limit double precision,
                   alarm_lower_limit double precision,
                   alert_diff_limit double precision,
                   alarm_diff_limit double precision,
                   ts character varying(2) NOT NULL,
                   CONSTRAINT locdatalimit_pk PRIMARY KEY (lid, pe, dur, ts, monthdaystart),
                   CONSTRAINT locdatalimit_dur_fk FOREIGN KEY (dur)
                       REFERENCES public.shefdur (dur) MATCH FULL
                       ON UPDATE NO ACTION ON DELETE NO ACTION,
                   CONSTRAINT locdatalimit_pe_fk FOREIGN KEY (pe)
                       REFERENCES public.shefpe (pe) MATCH FULL
                       ON UPDATE NO ACTION ON DELETE NO ACTION,
                   CONSTRAINT locdatalimit_ts_fk FOREIGN KEY (ts)
                       REFERENCES public.shefts (ts) MATCH FULL
                       ON UPDATE NO ACTION ON DELETE NO ACTION
                 )
                 WITH (
                   OIDS=FALSE
                 );
                 ALTER TABLE locdatalimitsnew OWNER TO awipsadmin;
                 GRANT ALL ON TABLE locdatalimitsnew TO awipsadmin;
                 GRANT SELECT, UPDATE, INSERT, DELETE ON TABLE locdatalimitsnew TO public;
                 INSERT INTO locdatalimitsnew SELECT lid, pe, dur, monthdaystart, monthdayend, gross_range_min,
                         gross_range_max, reason_range_min, reason_range_max, roc_max,
                         alert_upper_limit, alert_roc_limit, alarm_upper_limit, alarm_roc_limit, alert_lower_limit,
                        alarm_lower_limit, alert_diff_limit, alarm_diff_limit, '$default_ts' FROM locdatalimits;
                 ALTER TABLE locdatalimits RENAME TO locdatalimitsold;
                 ALTER TABLE locdatalimitsnew RENAME TO locdatalimits;
                 COMMIT TRANSACTION;
                 "
             echo INFO: Added column ts into table locdatalimits within ${hydrodb}.
         elif [[ "${has_column}" == "t" ]]; then
             echo WARN: Column ts already exists within ${hydrodb} locdatalimits. Do nothing.
         else
             echo ERROR: Failed to query the ${hydrodb} database.
         fi
    done
echo INFO: Done.