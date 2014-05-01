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
# ! script to create the HBDB database
# !
# ! $1 = install directory
# ! $2 = DB port number
# ! $3 = username
# ! $4 = script directory
# ! $5 = log file path
# !
# -----------------------------------------------------------------
echo ""
echo "--------------------------------------------------------------------------------"
echo "\| Creating HMDB Database and Tables..."
echo "--------------------------------------------------------------------------------"
${1}/bin/psql -d postgres -U ${3} -q -p ${2} -f ${4}/createHMDB.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/createTables.sql >> ${5} 2>&1
echo "--------------------------------------------------------------------------------"
echo "\| Populating HMDB Database Tables"
echo "--------------------------------------------------------------------------------"
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_boolean_values.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_bufr_identifier.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_cat_values.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_categorical_ele.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_cli_asos_daily.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_cli_asos_monthly.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_cli_freezedates.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_cli_mon_season_yr.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_cli_sta_setup.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_climate_day_config.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_climate_period.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_climo_dates.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_contin_int_ele.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_contin_real_ele.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_coordinates_2d.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_daily_climate.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_data_source.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_data_src_version.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_day_climate_extreme.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_day_climate_norm.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_defined_values.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_discrete_ele.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_discrete_values.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_dqd.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_ele_src_version.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_element_relat.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_forecast_backup.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_fss_categ_multi.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_fss_categ_single.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_fss_cloud_layer.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_fss_contin_real.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_fss_report.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_fss_wx_period.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_hydromet_element.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_issuance_type.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_issuing_office.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_map_proj_coords.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_mon_climate_norm.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_mtr_status.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_phys_ele_relat.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_physical_element.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_physical_units.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_prod_list.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_product.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_product_version.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_relat_type.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_rpt.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_sta_agency_codes.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_station_location.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_time_zone.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_units_class.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_units_conversion.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_units_system.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_units_translations.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_weather_category.sql >> ${5} 2>&1
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/load_wmo_state_region.sql >> ${5} 2>&1
echo "--------------------------------------------------------------------------------"
echo "\| Creating HMDB Database Constraints..."
echo "--------------------------------------------------------------------------------"
${1}/bin/psql -d hmdb -U ${3} -q -p ${2} -f ${4}/createConstraints.sql >> ${5} 2>&1
