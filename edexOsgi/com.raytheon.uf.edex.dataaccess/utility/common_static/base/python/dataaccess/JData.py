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
# Implements IData and wraps around a Java IData
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/10/12                      njensen       Initial Creation.
#    06/03/13          #2023       dgilling      Implement getAttributes().
# 
#

##
# This is a base file that is not intended to be overridden.
##



from awips.dataaccess import IData
import JUtil, DataTime

class JData(IData, JUtil.JavaWrapperClass):
    
    def __init__(self, wrappedObject):
        self.jobj = wrappedObject
    
    def getAttribute(self, key):
        return self.jobj.getAttribute(key)
    
    def getAttributes(self):
        attributes = []
        jattribs = self.jobj.getAttributes()
        itr = jattribs.iterator()
        while itr.hasNext():
            attributes.append(str(next(itr)))
        return attributes
    
    def getDataTime(self):
        return DataTime.DataTime(self.jobj.getDataTime())
    
    def getLevel(self):
        return str(self.jobj.getLevel())
    
    def getLocationName(self):
        return self.jobj.getLocationName()
    
    def toJavaObj(self):
        return self.jobj
    

