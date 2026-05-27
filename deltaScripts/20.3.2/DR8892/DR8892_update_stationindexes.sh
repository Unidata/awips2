#! /bin/bash
# This script should be ran once on dx/dv1 to update the stationindexes for 
# all tables with AircraftObsLocation or SurfaceObsLocation embedded. 
# The update should change the indexes to reflect what they should be generated with after DR8892.

# For every table in the list, check if it exists. 
# If it does, recreate the %TABLE%_stationindex for it using only stationid as the index
tableList=( 'acars' 'acarssounding' 'acarssoundinglayer' 'airep' 'bufrascat' 'bufrhdw' 'bufrmthdw' 'bufrncwf' 'bufrssmi' 'bufrua' 'dmw' 'fssobs' 'goessounding' 'ldadhydro' 'ldadmesonet' 'lsr' 'madis' 'modelsounding' 'ncpafm' 'nctaf' 'ncuair' 'obs' 'pirep' 'poessounding' 'qc' 'sfcobs' 'stq' 'svrwx' 'tcg' 'tcs' 'vaa')
for myTable in ${tableList[*]}; do
   echo "Updating table: ${myTable}..."

/awips2/psql/bin/psql -U awipsadmin -d metadata -c "
DO \$\$
BEGIN
IF EXISTS
(select 1 from information_schema.tables where table_schema = 'awips' and table_name = '${myTable}')
THEN
drop index if exists ${myTable}_stationindex; 
create index ${myTable}_stationindex on ${myTable} (stationid);
END IF;
END
\$\$;
"
done
