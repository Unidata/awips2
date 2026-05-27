#! /bin/bash
#
# Improve accuracy of the gridcoverage named 255.
/awips2/psql/bin/psql -U awips -d metadata -c "update gridcoverage set the_geom = 'POLYGON((-124.733314989453 24.9410313179688,-66.933372789453 24.9410313179688,-66.933372789453 52.8743367179688,-124.733314989453 52.8743367179688,-124.733314989453 24.9410313179688))', dx=0.0666666, dy=0.0666666 where name = '255';"