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

import glob
import logging
import os
import os.path
import sys


logger = None

def main():
    __initLogger()
    
    logger.info("Staring upgrade script for DR #1117...")
    logger.info("All information will also be written to removeObeGribParamInfoFiles.log")
    
    if not os.path.isdir('/awips2/edex/data/utility/edex_static/site/'):
        logger.error("This script must be run on the EDEX server. Exiting.")
        sys.exit(-1)
        
    obsoleteFiles = glob.glob('/awips2/edex/data/utility/edex_static/site/*/grib/parameterInfo/*.xml')
    
    logger.info("Deleting obsolete grib parameter info files.")
    for file in obsoleteFiles:
        try:
            os.remove(file)
        except (OSError, IOError):
            logger.exception("Could not delete file [" + file + ". Please manually delete this file after the script has exited.")
    
    logger.info("Upgrade script complete.")
    logger.info("The following files will need to be regenerated before restarting EDEX: " + str(obsoleteFiles))
    
    
def __initLogger():
    global logger
    logger = logging.getLogger("removeObeGribParamInfoFiles")
    logger.setLevel(logging.DEBUG)
    format = "%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S"
    
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    formatter = logging.Formatter("%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S")
    ch.setFormatter(formatter)
    logger.addHandler(ch)
    
    ch = logging.FileHandler('removeObeGribParamInfoFiles.log')
    ch.setLevel(logging.INFO)
    formatter = logging.Formatter("%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S")
    ch.setFormatter(formatter)
    logger.addHandler(ch)


if __name__ == '__main__':
    main()
