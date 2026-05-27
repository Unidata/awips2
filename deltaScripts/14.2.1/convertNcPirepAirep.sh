#!/bin/bash
# DR #2316,2317 replace airep and pirep with ncairep and ncpirep

PSQL="/awips2/psql/bin/psql"

SQL_COMMAND="
delete from plugin_info where name in ('ncpirep','ncairep');
drop table pirep, pirep_anc_data, ncpirep_anc_data, airep;
alter table ncpirep rename to pirep;
alter table ncairep rename to airep;
update pirep set datauri = replace(datauri, 'ncpirep', 'pirep');
update airep set datauri = replace(datauri, 'ncairep', 'airep');
"
${PSQL} -U awips -d metadata -c "${SQL_COMMAND}"

if [ -d "/awips2/edex/data/hdf5/ncpirep" ]
then
    mv /awips2/edex/data/hdf5/ncpirep /awips2/edex/data/hdf5/pirep 
    
    files=`ls /awips2/edex/data/hdf5/pirep/ncpirep*.h5`
    for file in $files; do
        newfile=${file/ncpirep/pirep}
        mv $file $newfile
    done
fi

if [ -d "/awips2/edex/data/hdf5/ncairep" ]
then
    mv /awips2/edex/data/hdf5/ncairep /awips2/edex/data/hdf5/airep
    
    files=`ls /awips2/edex/data/hdf5/airep/ncairep*.h5`
    for file in $files; do
        newfile=${file/ncairep/airep}
        mv $file $newfile
    done
fi