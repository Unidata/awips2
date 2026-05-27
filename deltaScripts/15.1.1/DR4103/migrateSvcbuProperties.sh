#!/bin/bash


# Determine which directory this script lives in so we can locate 
# file_header.txt ...
script_path=$(dirname $(readlink -f $0))
file_header=${script_path}/file_header.txt

echo "DR #4103: Moving svcbu.properties file to localization store..."

# source edex setup.env to get primary site id
source /awips2/edex/bin/setup.env
site_id=`echo ${AW_SITE_IDENTIFIER} | tr [a-z] [A-Z]`

base_file=/awips2/edex/data/utility/edex_static/base/config/gfe/svcbu.properties
old_site_file=/awips2/GFESuite/ServiceBackup/configuration/svcbu.properties
if [[ ! -f ${base_file} ]]
then
	echo "ERROR: Can not find BASE-level svcbu.properties file ${base_file}."
	echo "Exiting!"
	exit 1
fi
if [[ ! -f ${old_site_file} ]]
then
	echo "ERROR: Can not find previous version's svcbu.properties file ${old_site_file}."
	echo "Exiting!"
	exit 1
fi

site_override_contents=""

config_entries=( "GFESUITE_HOME" "GFESUITE_BIN" "SVCBU_HOME" "LOCALIZATION_PATH" "IFPS_LOG" "IFPS_DATA" "LOCK_DIR" "SCRIPTS_DIR" "CAVE_LAUNCH_SCRIPT" "SVCBU_HOST" "MSG_SEND_COMMAND" "CDSPORT" "SVCBU_DB" "SVCBU_TRIM_ELEMS" "SVCBU_FAILED_SITE_PORT" "SVCBU_GRIDAREA" "SVCBU_ADDRESSEE" "SVCBU_WMO_HEADER" "SVCBU_USER" "SVCBU_USER_ID" "EXPORT_GRID" "PRIMARY_SITES" )

for entry in "${config_entries[@]}"
do
	base_value=$(grep -E "^${entry}=" ${base_file})
	site_value=$(grep -E "^${entry}=" ${old_site_file})
	if [ "${base_value}" != "${site_value}" ]
	then
		site_override_contents="${site_override_contents}\n${site_value}"
	fi
done

if [[ -n "${site_override_contents}" ]]
then
	new_site_file=/awips2/edex/data/utility/edex_static/site/${site_id}/config/gfe/svcbu.properties
	
	echo "Writing new site override file ${new_site_file}."
	cat ${file_header} > ${new_site_file}
	echo "" >> ${new_site_file}
	echo -e ${site_override_contents} >> ${new_site_file}
fi

rm -f ${old_site_file}

