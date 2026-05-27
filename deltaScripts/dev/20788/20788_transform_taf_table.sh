#!/bin/bash

# DR 20788: Modified Tafs to use the SurfaceObsLocation embedded type. This script adjusts the corresponding taf table accordingly.
#
# Run as root on Postgres server. Postgres must be running for this to work.
#
# Author: smoorthy

transform_taf_table(){

    echo INFO: transforming taf table
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 <<EOF
    \set ON_ERROR_STOP on

    /* add the new columns with defaults*/ 
    ALTER TABLE taf
    ADD COLUMN elevation INTEGER,
    ADD COLUMN latitude REAL NOT NULL DEFAULT 'NaN',
    ADD COLUMN location geometry,
    ADD COLUMN locationdefined BOOLEAN DEFAULT FALSE,
    ADD COLUMN longitude REAL NOT NULL DEFAULT 'NaN';

    /* update taf fields from entries in the common_obs_spatial table*/
    UPDATE taf SET location = c.the_geom, longitude = ST_X(c.the_geom), latitude=ST_Y(c.the_geom), locationdefined=TRUE, elevation=c.elevation
    FROM common_obs_spatial as c
    WHERE c.gid = location_gid AND c.the_geom is not NULL;


   /* add "temp" old columns for the purposes of getting the right column order */
    ALTER TABLE taf
    ADD COLUMN stationid_temp VARCHAR(48) NOT NULL DEFAULT '',
    ADD COLUMN remarks_temp VARCHAR(255),
    ADD COLUMN taftext_temp VARCHAR(1024),
    ADD COLUMN wmoheader_temp VARCHAR(255);

    /* Copy data from old columns into the temps */
    UPDATE taf 
    set stationid_temp=stationid, remarks_temp=remarks, taftext_temp=taftext, wmoheader_temp=wmoheader;

    /* drop the old columns */
    ALTER TABLE taf
    DROP COLUMN stationid,
    DROP COLUMN remarks,
    DROP COLUMN taftext,
    DROP COLUMN wmoheader;

    /* restore the old columns from the temps, so they're in the correct places */
    ALTER TABLE taf
    RENAME COLUMN stationid_temp to stationid;
    ALTER TABLE taf
    RENAME COLUMN remarks_temp to remarks;
    ALTER TABLE taf
    RENAME COLUMN taftext_temp to taftext;
    ALTER TABLE taf
    RENAME COLUMN wmoheader_temp to wmoheader;


    /* drop un-needed column */
    ALTER TABLE taf DROP COLUMN location_gid;

    /* re-create index from dropped column */
    CREATE INDEX taf_stationindex on taf (stationid);


    /* add the constaint back */
    ALTER TABLE taf
    ADD CONSTRAINT uk_taf_datauri_fields
    UNIQUE (reftime, stationid, corindicator, amdindicator, issue_timestring);

    /* drop default settings for necessary columns */
    ALTER TABLE taf ALTER COLUMN latitude DROP DEFAULT;
    ALTER TABLE taf ALTER COLUMN longitude DROP DEFAULT;
    ALTER TABLE taf ALTER COLUMN stationid DROP DEFAULT;
    ALTER TABLE taf ALTER COLUMN locationdefined DROP DEFAULT;

EOF
}


transform_taf_table

