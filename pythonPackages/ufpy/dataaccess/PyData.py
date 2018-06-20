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
# Implements IData for use by native Python clients to the Data Access
# Framework.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/03/13                      dgilling      Initial Creation.
#    
#

from ufpy.dataaccess import IData

class PyData(IData):
    
    def __init__(self, dataRecord):
        self.__time = dataRecord.getTime()
        self.__level = dataRecord.getLevel()
        self.__locationName = dataRecord.getLocationName()
        self.__attributes = dataRecord.getAttributes()

    def getAttribute(self, key):
        return self.__attributes[key]
    
    def getAttributes(self):
        return self.__attributes.keys()
    
    def getDataTime(self):
        return self.__time
    
    def getLevel(self):
        return self.__level
    
    def getLocationName(self):
        return self.__locationName
