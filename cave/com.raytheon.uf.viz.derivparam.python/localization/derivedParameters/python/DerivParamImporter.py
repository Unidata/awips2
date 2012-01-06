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
# Python import hook (PEP 302) for providing inheritance to derived parameters.
# This enables an individual to override one of the methods of a derived
# parameter function while still retaining the base versions of the other
# methods.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/20/10                      njensen        Initial creation
#    
# 
#


import os, types, sys, imp, inspect

class DerivParamImporter:
    
    def __init__(self, userDir, siteDir, baseDir):
        self.baseDir = baseDir
        self.siteDir = siteDir
        self.userDir = userDir
    
    def __buildPath(self, dir, name):
        return dir + os.sep + name + '.py'
    
    def __isDerivParam(self, name):        
        return os.path.exists(self.__buildPath(self.baseDir, name)) or \
            os.path.exists(self.__buildPath(self.siteDir, name)) or \
            os.path.exists(self.__buildPath(self.userDir, name))
    
    def find_module(self, fullname, path=None):
        if path is None:
            if self.__isDerivParam(fullname):
                return self
        return None
    
    def load_module(self, fullname):
        if sys.modules.has_key(fullname):
            return sys.modules[fullname]
        combined = imp.new_module(fullname)
        self.__addToModule(combined, fullname, self.baseDir, "base")
        self.__addToModule(combined, fullname, self.siteDir, "site")
        self.__addToModule(combined, fullname, self.userDir, "user")
        sys.modules[fullname] = combined
        return combined
    
    def __addToModule(self, combined, moduleName, dirPath, level):
        fullpath = self.__buildPath(dirPath, moduleName)
        if os.path.exists(fullpath):
            mod = imp.load_source(level + moduleName, fullpath)
            methods = inspect.getmembers(mod, inspect.isfunction)
            for m in methods:
                combined.__setattr__(m[0], m[1])
        return combined
    