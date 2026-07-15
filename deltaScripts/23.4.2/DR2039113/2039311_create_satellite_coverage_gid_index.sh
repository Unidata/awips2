#!/bin/bash

# Script to add index to the satellite.coverage_gid field in postgres
#
# 2025-07-11 	ada.lockleigh 	2039113 	Initial version


/awips2/fxa/bin/a2dbauth /awips2/psql/bin/psql -U awipsadmin -h dv1 metadata <<EOF

CREATE INDEX satellite_coveragegididx on satellite (coverage_gid);

VACUUM(ANALYZE) satellite;
VACUUM(ANALYZE) satellite_spatial;

REINDEX TABLE satellite;
REINDEX TABLE satellite_spatial;


EOF
