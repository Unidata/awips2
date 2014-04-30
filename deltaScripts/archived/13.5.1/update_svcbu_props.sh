#!/usr/bin/env python

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

import logging
import shutil
import sys
from ufpy import ConfigFileUtil


logging.basicConfig(format="%(asctime)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    level=logging.INFO)


SVCBU_CONFIG_FILENAME = "/awips2/GFESuite/ServiceBackup/configuration/svcbu.properties"
SVCBU_CONFIG_BACKUP_FILENAME = SVCBU_CONFIG_FILENAME + ".bak"
SVCBU_CONFIG_MIGRATE = ['SVCBU_HOST', 'MSG_SEND_COMMAND', 'CDSPORT', 'SVCBU_DB', 
                        'SVCBU_TRIM_ELEMS', 'SVCBU_FAILED_SITE_PORT', 
                        'SVCBU_GRIDAREA', 'SVCBU_ADDRESSEE', 'SVCBU_WMO_HEADER', 
                        'SVCBU_USER', 'SVCBU_USER_ID', 'EXPORT_GRID']
STATIC_CONFIG_DATA = """
#Variables used by service backup:
#
# AWIPS_HOME:             The AWIPS II installation directory.
#
# GFESUITE_HOME:          The server directory containing files and programs
#                        used by GFE during Service Backup
#
# GFESUITE_BIN:            Directory containing GFE server side utility 
#                        programs including ifpnetCDF and iscMosaic
#
# SVCBU_HOME:            Directory used by service backup as a sandbox for 
#                        constructing files to be sent and for processing 
#                        received files.
#
# LOCALIZATION_PATH:      This is the path to the root of the localization
#                        directory.  This path is used for properly importing
#                        and exporting configuration data
#        
# IFPS_LOG:                Directory containing logs for the service backup 
#                        operations.
#
# IFPS_DATA:            Directory containing the svcbu_export_elements file.  
#                        This file is used to specify which weather elements are
#                        packaged and sent when exporting digital data for a 
#                        site.
#        
# LOCK_DIR:                Directory used for lock files.  Each Service Backup 
#                        operation maintains a lock file during its execution.  
#                        The lock remains for the duration of the operation and 
#                        is erased upon completion to prevent simultaneous 
#                        operations from occurring.
#
# SCRIPTS_DIR:            Directory containing the scripts used by service 
#                        backup
#
# CAVE_LAUNCH_SCRIPT:    This path points to the script which starts GFE.  This 
#                        variable is read when the user hits the 'Enable' button
#                        On the service backup GUI.  
#
# SVCBU_HOST:            Server where the service backup scripts will be 
#                        executed.
#
# MSG_SEND_COMMAND:     The command executed to send a message via the message handling
#                        system.  This value will usually be msg_send.  But, it can be
#                        changed to a different command in a test environment.
#
# CDSPORT:                This is the port on which the Thrift Client listens 
#                        for script execution events.
#
# SVCBU_DB:               Defines which database to use for exporting
#                       grids to central server for service backup.
#                       VALID VALUES: Fcst
#                                     Official (default)
#
# SVCBU_TRIM_ELEMS:     Indication of whether ifpnetCDF needs to trim
#                       off elements while exporting grids to central
#                       server.
#                       VALID VALUES: 1 - To do element trimming
#                                     0 - To disable element trimming
#                       Note: ${IFPS_DATA}/svcbu_export_elements.ccc
#                       file has to be present for this to work. This file
#                       will contain list of elements to include in the
#                       netcdf file that's being sent over to central srv.
#
# SVCBU_FAILED_SITE_PORT:    Unused
#
# SVCBU_GRIDAREA:        The name of the edit area used when exporting grids
#                       to the central server for service backup and 
#                       imported to the Restore databse after service backup.
#                       DEFUALT VALUE:  ISC_Send_Area
#
# SVCBU_ADDRESSEE:      The name of the msg_send addressee. Will be used to
#                       pass with -a flag of msg_send. (NCF use only).
#
# SVCBU_WMO_HEADER:     The WMO header that will be used to pass in calls to
#                       msg_send with -i argument. This will be empty to
#                       begin with. Should not be changed. (NCF use only)
#
# EXPORT_GRID           Indicate the ways of grid being exported
#                       VALID VALUES: 0 = do not export grids
#                                     1 = grids are exported by quartz timer
#                                            at 15 after each hour, the service
#                                          backup GUI, and from GFE via the 
#                                          'Send Grids to NDFD...' script
#                                     2 = grids are exported only by the service backup GUI and from GFE via the 'Send
#          Grids to NDFD...' script'
#
# SVCBU_USER            Indicates that the site can configure a special user to
#                       run GFE when in service backup
#                       VALID VALUES: 0 = do not use a designated user to run
#                                         GFE when in service backup
#                                     1 = use a designated user to run GFE 
#                                         when in service backup
#
# SVCBU_USER_ID         The user id of the designated user to run GFE when
#                       in service backup
#
# PRIMARY_SITES         (Optional) For dual-domain sites, a comma-separated 
#                       list of sites for the export grids cron to run for 
#                       instead of the site defined as AW_SITE_IDENTIFIER. If
#                       this setting is empty or not defined, cron will only
#                       export grids for site set as AW_SITE_IDENTIFIER.
#
#
# Directories used by Service Backup
GFESUITE_HOME=/awips2/GFESuite
GFESUITE_BIN=/awips2/GFESuite/bin
SVCBU_HOME=/awips2/GFESuite/ServiceBackup/svcbu
LOCALIZATION_PATH=/awips2/edex/data/utility
IFPS_LOG=/awips2/GFESuite/ServiceBackup/logs
IFPS_DATA=/awips2/GFESuite/ServiceBackup/data
LOCK_DIR=/awips2/GFESuite/ServiceBackup/locks
SCRIPTS_DIR=/awips2/GFESuite/ServiceBackup/scripts
CAVE_LAUNCH_SCRIPT=/awips2/cave/cave.sh

"""



def get_old_config():
    return ConfigFileUtil.parseKeyValueFile(SVCBU_CONFIG_FILENAME)

def backup_old_config():
    shutil.move(SVCBU_CONFIG_FILENAME, SVCBU_CONFIG_BACKUP_FILENAME)

def write_new_config(old_vals):
    with open(SVCBU_CONFIG_FILENAME, 'w') as configOut:
        configOut.write(STATIC_CONFIG_DATA)
        for entry in SVCBU_CONFIG_MIGRATE:
            oldValue = old_vals[entry]
            configOut.write(entry + "=" + oldValue + "\n")
        configOut.write("PRIMARY_SITES=")
        if "NATIONAL_CENTER" in old_vals and old_vals["NATIONAL_CENTER"] == '1':
            logging.warning("Since this system was previously configured as a " +
                            "national center, please configure the " +
                            "PRIMARY_SITES setting with your GFE sites after " +
                            "this script is complete.")

def main():
    logging.info("Migrating svcbu.properties for 13.5.1.")
    
    try:
        oldConfig = get_old_config()
    except:
        logging.exception("Could not read old configuration from " + SVCBU_CONFIG_FILENAME)
        
    try:
        backup_old_config()
    except:
        logging.exception("Could not backup previous svcbu.properties.")
        
    try:
        write_new_config(oldConfig)
    except:
        logging.exception("Could not write new svcbu.properties.")
    
    logging.info("Migration complete.")
    logging.info("After you have verified that svcbu.properties was properly migrated, " +
                 "please delete the file " + SVCBU_CONFIG_BACKUP_FILENAME + ".")


if __name__ == '__main__':
    main()
