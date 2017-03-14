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

import JUtil, WxDefinition

from com.raytheon.uf.common.dataplugin.gfe.weather import WeatherSubKey as JavaWeatherSubKey

#
# Provides a python compatible wrapper to WeatherSubKey
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/09/08                      njensen       Initial Creation.
#    04/20/2015      4027          randerso      Added __eq__ and __hash__ so WeatherSubKey works
#                                                correctly when used as key in a dict.
#                                                Changed several methods which were static methods in Java
#    12/08/2015      5129          dgilling      Remove dependency on DataManager.                                                 
#

class WeatherSubKey:
    def __init__(self, javaSubKey):
        self.__key = javaSubKey
        
    def __repr__(self):
        return str(self.__key.toString())
    
    def __str__(self):
        return str(self.__key.toString())
    
    def __eq__(self, other):
        try:
            return self.__key.equals(other.__key)
        except:
            return False
    
    def __ne__(self, other):
        return not self.__eq__(other)
   
    def __hash__(self):
        return self.__key.hashCode()
    
    def wxType(self):
        return self.__key.getType()
    
    def coverage(self):
        return self.__key.getCoverage()
    
    def intensity(self):
        return self.__key.getIntensity()
    
    def visibility(self):
        return self.__key.getVisibility()
        
    def attributes(self):
        return JUtil.javaObjToPyVal(self.__key.getAttributes())
    
    def wxDef(self):
        return WxDefinition.WxDefinition(self.__key.wxDef())
    
def availableCoverages(siteId, wxType):
    return JUtil.javaObjToPyVal(JavaWeatherSubKey.availableCoverages(siteId, wxType))

def availableAttributes(siteId, wxType):
    return JUtil.javaObjToPyVal(JavaWeatherSubKey.availableAttributes(siteId, wxType))

def availableIntensities(siteId, wxType):
    return JUtil.javaObjToPyVal(JavaWeatherSubKey.availableIntensities(siteId, wxType))

def availableVisibilities(siteId):         
    return JUtil.javaObjToPyVal(JavaWeatherSubKey.availableVisibilities(siteId))

def availableWxTypes(siteId):
    return JUtil.javaObjToPyVal(JavaWeatherSubKey.availableWxTypes(siteId))

def weatherSubKey(siteId, coverage, wxType, intensity, vis, attrList):
    javaSubKey = JavaWeatherSubKey(siteId, coverage, wxType,
            intensity, vis, JUtil.pyValToJavaObj(attrList))
    return WeatherSubKey(javaSubKey)    
    
