#!/bin/sh

##############################################################################
#
# This is the (obsolete?) script to execute the morning climate summary.
#
# Modification History:
# ---------------------
# 12/05/2002  OB2  Bob Morris         - Changed from csh to sh
#                                     - Removed CLIMATE_BIN_DIR definition, it
#                                       is set in readenv.csh.
#                                     - General cleanup.
##############################################################################
#
# Set the paths
#
CLIMATE_DATA_DIR=${CLIMATE_DIR}/data; export CLIMATE_DATA_DIR

CLIMATE_TMP_DIR=${CLIMATE_DIR}/tmp; export CLIMATE_TMP_DIR

#
# Clean up the old files
#
rm -f ${CLIMATE_TMP_DIR}/*_$1_* >> ${CLIMATE_DIR}/tmp.txt

rm -f ${CLIMATE_TMP_DIR}/*_$1 >> ${CLIMATE_DIR}/tmp.txt

rm -f ${CLIMATE_TMP_DIR}/global_day >> ${CLIMATE_DIR}/tmp.txt

exit 0
