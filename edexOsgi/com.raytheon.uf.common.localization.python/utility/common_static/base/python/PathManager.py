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
# Python should use this interface to get to the localization files.
#   
#
#    
#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/18/13                      mnash        Initial Creation.
#    
# 
#

import os, os.path
import IPathManager
import JUtil
from jep import jarray

from LocalizationFile import LocalizationFile
from LockingFile import File

from com.raytheon.uf.common.localization import PathManagerFactory
from com.raytheon.uf.common.localization import LocalizationContext as JavaLocalizationContext
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType, LocalizationContext_LocalizationLevel as LocalizationLevel
from java.io import File as JavaFile   
from java.lang import String

class PathManager(IPathManager.IPathManager):
        
    def __init__(self):
        self.jpathManager = PathManagerFactory.getPathManager()
    
    def getLocalizationFile(self, name, loctype=None, loclevel=None, locname=None):
        '''
        @param context: the localization context for which to get the file
        @param name: the name and path of the file
        @param loctype: the LocalizationType (COMMON_STATIC,CAVE_STATIC...)
        @param loclevel: the localization level (BASE,SITE,USER...)
        @param locname: the localization name if desired
        @return: the localization file
        @summary: This method returns the localization file based on the context and the name
        '''
        context = self._getContext(loctype, loclevel, locname)
        if context is not None and len(context) == 1:
            lFile = self.jpathManager.getLocalizationFile(context[0], name)
        else :
            lFile = self.jpathManager.getStaticLocalizationFile(name)
        if lFile is not None:
            return LocalizationFile(lFile)
    
    def getTieredLocalizationFile(self, loctype, name):
        '''
        @param loctype: The localization type to look in
        @param name: The name of the file
        @return: a dictionary of string to localization file
        @summary: Returns the localization levels available for the file given
        '''
        jtype = self._convertType(loctype)
        jMap = JUtil.javaMapToPyDict(self.jpathManager.getTieredLocalizationFile(jtype, name))
        vals = dict()
        for level in jMap:
            jlevel = self._convertLevel(level)
            vals[jlevel.name()] = LocalizationFile(jMap.get(level))
        return vals
    
    def listFiles(self, name, extensions, recursive, filesOnly, loctype=None, loclevel=None, locname=None):
        '''
        @param name: the name and path of the file
        @param extensions: a list of the file extensions to filter on
        @param recursive: whether or not to search through the directory recursively
        @param filesOnly: whether or not directories should be included in the list
        @param loctype: the localization type for which to get the file
        @param loclevel: the localization level for which to get the file
        @param locname: the name for which to get
        @return: a list of the file paths
        @summary: This method returns the list of fully qualified file paths for a 
        directory or a directory and its sub-directories
        '''
        contexts = self._getContext(loctype, loclevel, locname)
        extensionSize = len(extensions)
        extArr = jarray(extensionSize, String)
        for i in range(extensionSize):
            extArr[i] = String(extensions[i])
            
        if contexts is not None :            
            jfiles = self.jpathManager.listFiles(contexts, name, extArr, recursive, filesOnly)
        else :
            jfiles = self.jpathManager.listStaticFiles(name, extArr, recursive, filesOnly)
        if jfiles is not None :
            files = list()
            for file in jfiles :
                files.append(LocalizationFile(file))
            return files
           
    def getAvailableLevels(self):
        '''
        @return: the levels available to the caller
        @summary: This method returns the list of available levels.
        '''
        jLevels = self.jpathManager.getAvailableLevels()
        levels = list()
        for level in jLevels :
            levels.append(level.name())
        return levels    

    # converts a type of a list of types to the java counterparts
    def _convertType(self, loctype):
        if loctype is not None:
            jtype = loctype
            if isinstance(loctype, basestring):
                jtype = LocalizationType.valueOf(loctype)
            elif isinstance(loctype, list):
                jtype = []
                for i in range(loctype):
                    jtype[i] = self._convertType(loctype[i])
            return jtype
    
    # converts a level or a list of levels to the java counterparts
    def _convertLevel(self, loclevel):
        if loclevel is not None :
            jlevel = loclevel
            if isinstance(loclevel, basestring):
                jlevel = LocalizationLevel.valueOf(loclevel)
            elif isinstance(loclevel, list):
                jlevel = []
                for i in range(loclevel):
                    jlevel[i] = self._convertLevel(loclevel[i])
            return jlevel
        
    def _getContext(self, loctype, loclevel, locname=None):
        jtype = self._convertType(loctype)
        jlevel = self._convertLevel(loclevel)
        context = None
        if isinstance(jtype, list) is False:
            if jtype is not None :
                jtype = [jtype]
            if jlevel is not None :
                jlevel = [jlevel]
        return self._contextForList(jtype, jlevel, locname)
                    
    def _contextForList(self, loctypes, loclevels, locname=None):
        # gets the contexts in list form, for ease of use, we always use a list of contexts
        # for methods that can take both
        contexts = jarray(len(loctypes), JavaLocalizationContext)
        for i in range(len(loctypes)):
            if locname is None and loclevels is None:
                return None
            elif loclevels is not None :
                contexts[i] = self.jpathManager.getContext(loctypes[i], loclevels[i])   
            if locname is not None :
                contexts[i].setContextName(locname)
        return contexts
