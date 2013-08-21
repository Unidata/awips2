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
#    
# 
#
import os

hdf5loc = "/awips2/edex/data/hdf5/gfe"

def processDir(dir):
    # walk the directory tree removing *_GridParm.h5 files
    for file in os.listdir(dir):
        filePath = os.path.join(dir, file)
        if os.path.isfile(filePath) and str(filePath).endswith("_GridParm.h5"):
            print "Removing ", filePath
            os.remove(filePath)
        elif os.path.isdir(filePath):
            processDir(filePath)
    
    # if directory is empty remove it
    if len(os.listdir(dir)) == 0:
        print "Removing ", dir
        os.removedirs(dir)
    
def main():
    processDir(hdf5loc)
    
if __name__ == '__main__':
    main()
