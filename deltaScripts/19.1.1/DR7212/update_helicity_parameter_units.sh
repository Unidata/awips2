#!/bin/bash

# Delta script for DR #7212
# This script will update the units for the helicity parameter in the parameter
# to be "m^2/s^2".
#
# Author: dgilling

psql=/awips2/psql/bin/psql

echo "INFO: Running delta script for DR #7212: Updating helicity parameter units."

${psql} --db metadata -U awipsadmin -c "UPDATE parameter SET unit='m^2/s^2' WHERE abbreviation='Heli'" 

echo "INFO: Delta script complete"
