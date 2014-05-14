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

#
# Update GFE HDF5 Group format to include minutes
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/18/10                      njensen       Initial Creation.
#    06/13/13        #2044         randerso      Fixed to use correct python
#    05/08/14        #3142         dgilling      Add better error-handling, logging.
#    
# 
#
import os
import logging

hdf5loc = "/awips2/edex/data/hdf5/gfe"

logging.basicConfig(format="%(asctime)s %(name)s:%(lineno)d %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    level=logging.INFO)
log = logging.getLogger("CleanupGfeHDF5Storage")


def processDir(hdf5Dir):
    # walk the directory tree removing *_GridParm.h5 files
    # any directories that become empty after removing those files will also
    # be deleted.
    for root, dirs, files in os.walk(hdf5Dir, topdown=False):
        for file in files:
            if str(file).endswith("_GridParm.h5"):
                fullPath = os.path.join(root, file)
                log.info("Removing " + str(fullPath))
                try:
                    os.remove(fullPath)
                except OSError:
                    log.exception("Could not delete file " + str(fullPath))
                    
        for dir in dirs:
            fullPath = os.path.join(root, dir)
            try:
                if not os.listdir(fullPath):
                    log.info("Removing " + str(fullPath))
                    try:
                        os.rmdir(fullPath)
                    except OSError:
                        log.exception("Could not delete path " + str(fullPath))
            except OSError:
                log.warning("Skipping directory " + str(fullPath), exc_info=True)

    
def main():
    processDir(hdf5loc)
    
if __name__ == '__main__':
    main()
