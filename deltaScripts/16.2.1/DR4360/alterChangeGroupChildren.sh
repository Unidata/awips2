#!/bin/bash 
# DR #4360 - Delete duplicate entries of children of taf_change_groups.

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

# first argument is the table other arguments are columns to check for equal.
function delDup {
        table=${1}
        shift
        col1=${1}
        shift
        cmd="select distinct t1.id"
        cmd="${cmd} from ${table} t1, ${table} t2 where t1.${col1}=t2.${col1}"
        for col in ${@} ; do
                cmd="${cmd} and t1.${col}=t2.${col}"
        done
        cmd="${cmd} and t2.id<t1.id"
        echo "INFO: Delete duplicate entries in table ${table}."
        ${PSQL} -U awips -d metadata -c "DELETE from ${table} where id in (${cmd}) ;"
}

delDup taf_icing_layers parentid icing_intensity icing_min_alt_ft_agl icing_max_alt_ft_agl
delDup taf_temperature_forecasts parentid valid_time sfc_temp_c
delDup taf_turbulence_layers parentid turbulence_intensity turbulence_min_alt_ft_agl turbulence_max_alt_ft_agl
delDup taf_weather_conditions parentid precipitation obscuration intensityProximity descriptor other
delDup taf_sky_cover parentid height type
