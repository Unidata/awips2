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
# Provides support for loading/saving python objects from/to localization files
# using JSON serialization
# 
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/10/14                      randerso       Initial Creation.
##
import json
import LocalizationSupport

import JUtil

def loadFromJson(localizationType, siteID, fileName):
    jsonString = LocalizationSupport.readFile(localizationType, siteID, fileName)
    object = json.loads(jsonString)
    return object
    
def saveToJson(localizationType, siteID, fileName, object):
    jsonString = json.dumps(object, sort_keys=True, 
                            indent=4, separators=(',', ': '))
    LocalizationSupport.writeFile(localizationType, siteID, fileName, jsonString)

def loadJsonFromJava(localizationType, siteID, fileName):
    object = loadFromJson(localizationType, siteID, fileName)
    javaObject = JUtil.pyValToJavaObj(object)
    return javaObject

def saveJsonFromJava(localizationType, siteID, fileName, javaObject):
    object = JUtil.javaObjToPyVal(javaObject)
    saveToJson(localizationType, siteID, fileName, object)
    