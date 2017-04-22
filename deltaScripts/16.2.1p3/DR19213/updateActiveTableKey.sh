#!/bin/bash

#psql -h dx1 -U awips -d metadata -f updateActiveTableKey.sql
/awips2/psql/bin/psql -U awips -d metadata -f updateActiveTableKey.sql
