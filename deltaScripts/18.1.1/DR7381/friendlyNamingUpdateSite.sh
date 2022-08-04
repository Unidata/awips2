#!/bin/bash

# Update awips.parameter table in DB
/awips2/psql/bin/psql -U awips -d metadata -h dx1 -f ./parameters.sql >& /dev/null
