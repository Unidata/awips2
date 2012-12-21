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
# __init__.py for ufpy.dataaccess package
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/10/12                      njensen       Initial Creation.
#    
# 
#

__all__ = [
           
           ]

import abc

class IDataRequest(object):
    __metaclass__ = abc.ABCMeta
    
    @abc.abstractmethod
    def setDatatype(self, datatype):
        return
    
    @abc.abstractmethod
    def addIdentifier(self, key, value):
        return
    
    @abc.abstractmethod
    def setParameters(self, params):
        return
    
    @abc.abstractmethod
    def setLevels(self, levels):
        return
    
    @abc.abstractmethod
    def getDatatype(self):
        return
    
    @abc.abstractmethod
    def getIdentifiers(self):
        return
    
    @abc.abstractmethod
    def getParameters(self):
        return
    
    @abc.abstractmethod
    def getLevels(self):
        return


class IGridRequest(IDataRequest):
    __metaclass__ = abc.ABCMeta
    

class IGeometryRequest(IDataRequest):
    __metaclass__ = abc.ABCMeta
    
    @abc.abstractmethod
    def getEnvelope(self):
        return
    
    @abc.abstractmethod
    def setEnvelope(self, env):
        return
    
    @abc.abstractmethod
    def getLocationNames(self):
        return
    
    @abc.abstractmethod
    def setLocationNames(self, locationNames):
        return


class IData(object):
    __metaclass__ = abc.ABCMeta
    
    @abc.abstractmethod
    def getAttribute(self, key):
        return
    
    @abc.abstractmethod
    def getDataTime(self):
        return
    
    @abc.abstractmethod
    def getLevel(self):
        return



class IGridData(IData):
    __metaclass__ = abc.ABCMeta
    
    @abc.abstractmethod
    def getParameter(self):
        return
    
    @abc.abstractmethod
    def getUnit(self):
        return
    
    @abc.abstractmethod
    def getRawData(self):
        return



class IGeometryData(IData):
    #__metaclass__ = abc.ABCMeta
    
    @abc.abstractmethod
    def getGeometry(self):
        return
    
    @abc.abstractmethod
    def getParameters(self):
        return
    
    @abc.abstractmethod
    def getString(self, param):
        return
    
    @abc.abstractmethod
    def getNumber(self, param):
        return
    
    @abc.abstractmethod
    def getUnit(self, param):
        return
    
    @abc.abstractmethod
    def getType(self, param):
        return
    
    @abc.abstractmethod
    def getLocationName(self, param):
        return

