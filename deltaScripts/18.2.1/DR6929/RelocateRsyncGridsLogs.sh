#!/bin/sh
# This script moves the recent netcdf_rsync.log files from 
#    /awips2/GFESuite/ServiceBackup/data/rsyncGridsToCWF/log/yyyymmdd
# to /awips2/GFESuite/logs/xxx/yyyymmdd/dx3-xxx/
#
# and then removes the /awips2/GFESuite/ServiceBackup/data/rsyncGridsToCWF/log directory
#
# If this script has errors it can be re-run once any issues are corrected
#
. /awips2/edex/bin/setup.env
SITE=$(echo ${AW_SITE_IDENTIFIER} | tr '[a-z]' '[A-Z]')  # site in all upper case
site=$(echo ${AW_SITE_IDENTIFIER} | tr '[A-Z]' '[a-z]')  # site in all lower case

NEW_LOG_DIR=/awips2/GFESuite/logs/${SITE}
OLD_LOG_DIR=/awips2/GFESuite/ServiceBackup/data/rsyncGridsToCWF/log

# for any date that exists in the new log directory
for yyyymmdd in `ls -1 ${NEW_LOG_DIR}` ; do
    
    # if a log file for this date exists in the old log directory
    if [ -f ${OLD_LOG_DIR}/${yyyymmdd}/netcdf_rsync.log ] ; then
        
        # create dx3-xxx directory just in case it doesn't already exist
        # this will probably never be needed but is here just in case 
        if [ ! -d ${NEW_LOG_DIR}/${yyyymmdd}/dx3-${site} ] ; then 
            mkdir -p ${NEW_LOG_DIR}/${yyyymmdd}/dx3-${site}
            chmod 777 ${NEW_LOG_DIR}/${yyyymmdd}/dx3-${site}
            chown awips:fxalpha ${NEW_LOG_DIR}/${yyyymmdd}/dx3-${site}
        fi
        
        # copy the log file fom the old directory to the new directory
        cp ${OLD_LOG_DIR}/${yyyymmdd}/netcdf_rsync.log ${NEW_LOG_DIR}/${yyyymmdd}/dx3-${site}
    fi
done

# remove the old log directory
rm -rf ${OLD_LOG_DIR}
