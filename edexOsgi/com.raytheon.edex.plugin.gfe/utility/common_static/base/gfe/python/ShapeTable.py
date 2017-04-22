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

import JUtil
import DefaultEditAreaNaming
from com.raytheon.edex.plugin.gfe.reference import DbShapeSource

#
# Python wrapper class for PostGIS table with interface like A1 ShapeFile.py
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/04/12            #9441     randerso       Initial Creation.
#    
# 
#

class ShapeTable(JUtil.JavaWrapperClass):
    def __init__(self, identifier):
        self.identifier = identifier
        self.name = None
        self.editAreaName = None
        self.groupName = None
        self.javaObj = DbShapeSource(identifier)
        pass    

    def filename(self, filen):
        raise NotImplementedError, "This method is obsolete. See comments in Maps.py"
    
    def filter(self, fn):
        if callable(fn):
            self._func = fn
            self.javaObj.setFiltered(True)
        else:
            raise TypeError(self.__class__+".filter() requires a function")
        
    def doFilter(self, atts):
        return self._func(atts)
    
    def getEAName(self, atts):
        if self.editAreaName is not None:
            return DefaultEditAreaNaming.getEditAreaName(atts, self.editAreaName)
        
        return ""

    def toJavaObj(self):
        self.javaObj.setDisplayName(self.name)
        self.javaObj.setGroupName(self.groupName)
        if self.editAreaName is not None:
            self.javaObj.setHasEditAreaName(True);
        return self.javaObj
    
    def __repr__(self):
        return self.identifier