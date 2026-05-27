#!/bin/bash

# DR 22245: projectname column size is too small for names of some projects. Increase size to 120.
#
# Run as root on Postgres server. Postgres must be running for this to work.
#
# Author: jrohwein

update_projectname_size(){
    echo INFO: updating size of jobname column of backup_job
    /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 <<EOF
    \set ON_ERROR_STOP on
    ALTER TABLE stq ALTER COLUMN projectname TYPE character varying(120);
EOF
}

update_projectname_size
