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

##
# This is a base file that is not intended to be overridden.
##

##
# uengine is deprecated and will be removed from the system soon. Migrate your
# apps to using the Data Access Framework (DAF).
##

#
# NcModelCycleQuery
#
# This code has been developed by the SIB for use in the AWIPS2 system.
# Scans dataLocation directory and returns all the available cycles.
#
#     Usage:
#    import NcModelCycleQuery
#    query = NcModelCycleQuery.NcModelCycleQuery()
#    query.setDataLocation("$MODEL/gfs")
#    return query.execute()
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer             Description
#    ------------    ----------    -----------          --------------------------
#    10/22/10        ??            mgamazaychikov       Initial Creation
#
from java.util import ArrayList
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from gov.noaa.nws.ncep.edex.uengine.utility import GempakConvert
import os


class NcModelCycleQuery():
    
    def __init__(self):
        self.convert = GempakConvert()
    
#
# Set parameters of the metadata catalog query
#
    def setDataLocation(self, aDataLocation):
        dirLevels = aDataLocation.split("/")
        numberDirLevel = len(dirLevels)
        if aDataLocation.find("$") == 0:
            envName = dirLevels[0].split("$")[1]
            location = os.environ [envName]
            for ii in range (1,numberDirLevel -1):
                location = location + "/" + dirLevels[ii]
            model = dirLevels[numberDirLevel-1].lower()
        else:
            location = "/".join(dirLevels[0:numberDirLevel-1])
            model = dirLevels[numberDirLevel-1].lower()
        self.dataDir = location + "/" + model

#
# Executes the query and calls appropriate response functions
#    
    def execute(self):
        cycles=[]
        for fn in os.listdir (self.dataDir):
            cycle = fn.split('_')[1].split('f')[0][2:8] + "_" + fn.split('_')[1].split('f')[0][8:]+ '00'
            if cycle not in cycles:
                cycles.append(cycle)
        cycles.sort()
        if cycles is None:
            return self.makeNullResponse("No cycle times available")
        else:
            return self.__makeResponse(cycles)
#
# Generates a list of responses in XML format
#
    def __makeResponse(self, cycles):
        retStr = "|".join(cycles)
        return ResponseMessageGeneric(retStr)
    
#
# Returns a string with a response message
#
    def __makeNullResponse(self, aMessage="Database Query returned no results"):
        return ResponseMessageGeneric(aMessage)