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
import h5py
import os
import re
import subprocess
import traceback

hdf5loc = "/awips2/edex/data/hdf5/gfe"
# T::SFC::2013_07_04_06--2013_07_04_07
oldGroupFormat = re.compile("(.+::.+::)(\d{4})_(\d\d)_(\d\d)_(\d\d)--(\d{4})_(\d\d)_(\d\d)_(\d\d)")

# T::SFC::20130704_0600--20130704_0700
newGroupFormat = re.compile("(.+::.+::)(\d{4})(\d\d)(\d\d)_(\d\d)(\d\d)--(\d{4})(\d\d)(\d\d)_(\d\d)(\d\d)")


def updateFile(filePath):
    print "Updating",filePath
    h5File = h5py.File(filePath)
    origGroups = h5File.keys()
    for groupName in origGroups:
        newMatch = newGroupFormat.match(groupName)
        oldMatch = oldGroupFormat.match(groupName)
        if newMatch:
            #print "Found new format:", groupName
            pass
        elif oldMatch:
            #print "Found old format:", groupName
            (nameAndLevel, startYear, startMonth, startDay, startHour, endYear, endMonth, endDay, endHour)= oldMatch.groups()
            newGroupName = nameAndLevel+startYear+startMonth+startDay+"_"+startHour+"00--"+endYear+endMonth+endDay+"_"+endHour+"00"
            #print "      New format:", newGroupName
            
            # if new group already exists (partial conversion)
            if newGroupName in origGroups:
                newGroup = h5File[newGroupName]
            # else create new group
            else:
                newGroup = h5File.create_group(newGroupName)
            
            # move datasets from old group to new group
            oldGroup = h5File[groupName]
            dataSets = oldGroup.keys()
            for dataSet in dataSets:
                #print "   Moving dataSet:",dataSet
                newGroup[dataSet] = oldGroup[dataSet]
                del oldGroup[dataSet]
            
            # remove old group
            del h5File[groupName]             
        else:
            print "Unrecognized group found:",groupName
            
    h5File.close()

def repack(dir):
    files = os.listdir(dir)
    for file in files:
        filePath = os.path.join(dir, file)
        if os.path.isfile(filePath) and \
           str(filePath).endswith(".h5") and \
           not str(filePath).endswith("_GridParm.h5"):
            repackFilePath = filePath+".repack"
            try:
                subprocess.check_call(("/awips2/tools/bin/h5repack", filePath, repackFilePath))
            except:
                print "h5repack failed:", filePath
                continue
            
            try:
                os.remove(filePath)
                os.rename(repackFilePath, filePath)
            except:
                print "error renaming repacked file:", repackFilePath
                continue
            

def processDir(dir):
    singleton = False
    for file in os.listdir(dir):
        filePath = os.path.join(dir, file)
        if os.path.isfile(filePath) and \
           str(filePath).endswith(".h5"):
            if str(filePath).endswith("_GridParm.h5"):
                if (str(filePath).endswith("_00000000_0000_GridParm.h5")):
                    singleton = True
            else:
                updateFile(filePath)

        elif os.path.isdir(filePath):
            # skip the Topo and climo directories (climo is obsolete and should be removed)
            if str(file) != 'Topo' and str(file) != 'climo':
                processDir(filePath)
    
    if singleton:
        print "repacking singleton database:", dir
        repack(dir)

def main():
    processDir(hdf5loc)
    
if __name__ == '__main__':
    main()
