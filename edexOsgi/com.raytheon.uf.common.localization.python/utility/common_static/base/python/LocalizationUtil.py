# #
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
# #

#
# Methods to aid in using the path manager from Python as well as other features.  
# Contains internal classes to help with the transition from strings in Python
# to Java Localizationlevel and LocalizationType
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/12/13                      mnash        Initial Creation.
#    
# 
#

import os
import imp

from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as JavaLocalizationType, LocalizationContext_LocalizationLevel as JavaLocalizationLevel

class LocalizationLevel(object):
    '''
    @summary: Can use cmp() to compare the levels, and can use values() to get all possible levels
    '''
    @staticmethod
    def cmp(level1, level2):
        return JavaLocalizationLevel.compare(level1,level2)
    
    @staticmethod
    def values():
        jvals = JavaLocalizationLevel.values()
        vals = list()
        for val in jvals :
            vals.append(val.name())
        return vals

    @staticmethod
    def valueOf(value):
        return JavaLocalizationLevel.valueOf(value)

class LocalizationType(object):

    @staticmethod
    def values():
        jvals = JavaLocalizationType.values()
        vals = list()
        for val in jvals :
            vals.append(val.name())
        return vals
    
    @staticmethod
    def valueOf(value):
        return JavaLocalizationType.valueOf(value)

def loadModule(filename):
    '''
    @param filename: the fully qualified name of the file
    @return: the module that was loaded
    @summary: This function takes a filename and find the module,
    loads it and returns that module
    '''
    path = os.path.splitext(filename)[0]
    filename = os.path.split(path)[1]
    fp, pathname, description = imp.find_module(filename)
    return imp.load_module(filename, fp, pathname, description)