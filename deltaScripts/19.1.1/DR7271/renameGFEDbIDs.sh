#! /bin/bash
#
# rename the GFE DbIds that were changed in serverConfig.py 
#
# NOTE: this script should be run BEFORE starting the EDEX JVMs.
#
echo "RODO DR 7271: renaming the GFE DbIds that were changed in serverConfig.py "
/awips2/psql/bin/psql -U awips -d metadata << EOF
UPDATE gfe_dbid SET modelName='ECMWF' WHERE modelName='ECMWFHiRes'
    AND NOT EXISTS (SELECT 1 FROM gfe_dbid WHERE modelName='ECMWF');
UPDATE gfe_dbid SET modelName='GLAMP' WHERE modelName='GFSLAMPGrid'
    AND NOT EXISTS (SELECT 1 FROM gfe_dbid WHERE modelName='GLAMP');
UPDATE gfe_dbid SET modelName='GFS' WHERE modelName='GFS20'
    AND NOT EXISTS (SELECT 1 FROM gfe_dbid WHERE modelName='GFS');
UPDATE gfe_dbid SET modelName='NAM' WHERE modelName='NAM12'
    AND NOT EXISTS (SELECT 1 FROM gfe_dbid WHERE modelName='NAM');
UPDATE gfe_dbid SET modelName='WW3-2km' WHERE modelName='GLWN'
    AND NOT EXISTS (SELECT 1 FROM gfe_dbid WHERE modelName='WW3-2km');
UPDATE gfe_dbid SET modelName='RAP' WHERE modelName='RUC13' 
    AND NOT EXISTS (SELECT 1 FROM gfe_dbid WHERE modelName='RAP');
UPDATE gfe_dbid SET modelName='URMA' WHERE modelName='URMA25' 
    AND NOT EXISTS (SELECT 1 FROM gfe_dbid WHERE modelName='URMA');
EOF
echo "RODO DR 7271: Done" 
