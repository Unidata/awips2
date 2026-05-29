#!/bin/bash
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
# -----------------------------------------------------------------
# ! script to create the Climate database
# !
# ! $1 = ignored (was psql install directory)
# ! $2 = ignored (was postgresql install directory)
# ! $3 = DB port number
# ! $4 = username
# ! $5 = script directory
# ! $6 = log file path
# !
# -----------------------------------------------------------------


# Set PGHOST appropriately:
# - If we're already on "dv1", set it to dv1.
# - If we're on any other host with postgres running, set it 
#   to "localhost".
# - If we're on any other host without postgres running, set it
#   to "dv1".

export PGHOST='localhost'

host=$(hostname)

#extract first part of hostname of the form dv1-xxx.xxx
IFS="-"
hostTemp=''
for part in $host
do
    hostTemp=$part
    break
done
unset IFS

# if we're on dv1, just set "PGHOST" to dv1
if [ $hostTemp == 'dv1' ] ; then
    export PGHOST=$hostTemp
fi

#check if postgres is running on this host. Should be many 
#processes with "postgres" in the name.
numPostgres=$(ps -ef | grep postgres | wc -l)

#choosing 3 for safety, since the "grep" is atleast 1
if [ $numPostgres -lt 3 ] ; then
   export PGHOST='dv1'
fi


echo ""
echo "--------------------------------------------------------------------------------"
echo "\| Creating Climate Database and Tables..."
echo "--------------------------------------------------------------------------------"
psql -d postgres -U ${4} -q -p ${3} -f ${5}/createClimateDb.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/createTables.sql >> ${6} 2>&1
echo ""
echo "--------------------------------------------------------------------------------"
echo "\| Populating Climate Database Tables"
echo "--------------------------------------------------------------------------------"
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_boolean_values.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_bufr_identifier.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_cat_values.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_categorical_ele.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_cli_asos_daily.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_cli_asos_monthly.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_cli_freezedates.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_cli_mon_season_yr.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_cli_sta_setup.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_climate_day_config.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_climate_period.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_climo_dates.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_contin_int_ele.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_contin_real_ele.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_coordinates_2d.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_daily_climate.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_data_source.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_data_src_version.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_day_climate_extreme.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_day_climate_norm.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_defined_values.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_discrete_ele.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_discrete_values.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_dqd.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_ele_src_version.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_element_relat.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_forecast_backup.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_fss_categ_multi.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_fss_categ_single.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_fss_cloud_layer.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_fss_contin_real.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_fss_report.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_fss_wx_period.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_hydromet_element.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_issuance_type.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_issuing_office.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_map_proj_coords.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_mon_climate_norm.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_mtr_status.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_phys_ele_relat.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_physical_element.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_physical_units.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_prod_list.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_product.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_product_version.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_relat_type.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_rpt.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_sta_agency_codes.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_station_location.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_time_zone.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_units_class.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_units_conversion.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_units_system.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_units_translations.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_weather_category.sql >> ${6} 2>&1
psql -d climate -U ${4} -q -p ${3} -f ${5}/load_wmo_state_region.sql >> ${6} 2>&1
echo ""
echo "--------------------------------------------------------------------------------"
echo "\| Creating Climate Database Constraints..."
echo "--------------------------------------------------------------------------------"
psql -d climate -U ${4} -q -p ${3} -f ${5}/createConstraints.sql >> ${6} 2>&1
echo ""
echo "--------------------------------------------------------------------------------"
echo "\| Creating cpg_session and sent_prod_record tables..."
echo "--------------------------------------------------------------------------------"
psql -d climate -U ${4} -q -p ${3} -f ${5}/createAdditionalTables.sql >> ${6} 2>&1
echo ""
