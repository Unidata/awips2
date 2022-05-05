#!/bin/bash

# DR 22487: jobname column size is too small for names of some jobs. Increase size to 256.
#
# Run as root on Postgres server. Postgres must be running for this to work.
#
# Author: smoorthy

update_jobname_size(){
    echo INFO: updating size of jobname column of backup_job
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 <<EOF
    \set ON_ERROR_STOP on
    ALTER TABLE backup_job ALTER COLUMN jobname TYPE character varying(256);
EOF
}

update_jobname_size
