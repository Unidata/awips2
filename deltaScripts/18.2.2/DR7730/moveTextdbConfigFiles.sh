#! /bin/bash
#
# Installs national_category_table.template and afos_lookup_table.dat files at configured level
# removes them, if they are present, from the base level.
# NOTE: these files were originally in common_static/base/textdb and are now in 
# common_static/configured/XXX/textdb/config
#
# Run this script only on dx3 as root.
#

# get the AW_SITE_IDENTIFER
source /awips2/edex/bin/setup.env

# get the directory containing this script
SCRIPTDIR=`dirname $0`

echo "INFO: Running delta script for DR 7730"

# setup the variables for the original and new file locations 
TEXTDB_BASE_DIR=/awips2/edex/data/utility/common_static/base/textdb
TEXTDB_CONFIG_DIR=/awips2/edex/data/utility/common_static/configured/${AW_SITE_IDENTIFIER}/textdb/config

# create the new directory if necessary
mkdir -p ${TEXTDB_CONFIG_DIR}

# copy the national_category_table.template and afos_lookup_table.dat files 
# from the script directory to the new location
cp -p ${SCRIPTDIR}/national_category_table.template ${TEXTDB_CONFIG_DIR}
cp -p ${SCRIPTDIR}/afos_lookup_table.dat ${TEXTDB_CONFIG_DIR}

# remove the old files (and *.md5 files), if any, from the original directory
rm -f ${TEXTDB_BASE_DIR}/national_category_table.template*
rm -f ${TEXTDB_BASE_DIR}/afos_lookup_table.dat*

# set ownership and permissions on the new files/directories
chown -R awips:fxalpha ${TEXTDB_CONFIG_DIR}/.. 
chmod 775 ${TEXTDB_CONFIG_DIR}
chmod 664 ${TEXTDB_CONFIG_DIR}/*

echo "INFO: Done."
