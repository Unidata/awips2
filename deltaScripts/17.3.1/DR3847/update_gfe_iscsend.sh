#!/bin/bash
#
# Update gfe_iscsend table to use a foreign key link to the gfe_parmid table
#
# Data is transient and dropped whenever GFE site is deactivated so does not
# need to be preserved.
#
scriptDir=`dirname $0`
psql -U awipsadmin -d metadata -f ${scriptDir}/update_gfe_iscsend.sql
