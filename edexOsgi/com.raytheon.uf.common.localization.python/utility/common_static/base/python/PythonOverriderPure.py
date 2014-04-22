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
# The pure python module override solution. Merges multiple python modules retrieved
# from localization.
#
#
#
#    SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/12/13                      bkowal         Initial Creation.
#    02/17/14        2712          bkowal         Provide a default value for localization site.
#    03/19/14        2929          bkowal         'REGION' is now recognized as a valid localization level.
#
#
#

import os, tempfile, shutil
import numpy
import PythonOverriderCore
from ufpy import ThriftClient
from dynamicserialize.dstypes.com.raytheon.uf.common.auth.resp import SuccessfulExecution
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationContext
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.msgs import ListUtilityCommand
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.msgs import UtilityRequestMessage
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationLevel
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationType
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.stream import LocalizationStreamGetRequest

BUFFER_SIZE = 512 * 1024
availableLevels = ['BASE', 'REGION', 'CONFIGURED', 'SITE', 'USER']

def importModule(name, localizationHost, localizationPort, localizedSite=None, localizationUser=None, 
                 loctype='COMMON_STATIC', level=None):
    '''
    @param name: the name of the localization file
    @param localizationHost: the EDEX server that the localization should be
        retrieved from
    @param localizationPort: the port that will be used to connect to the
        EDEX server
    @param localizedSite: the site that localization information should be
        retrieved for (if applicable)
    @param localizationUser: the user that localization information should
        be retrieved for (if applicable)
    @param loctype: the type of localization files to retrieve  
    @param level: the minimum level that localization files should be retrieved
        for
    @return: the merged module
    @summary: this is the pure python (no jep dependencies) version of the python overrider  
    '''   
    
    # determine which localization levels files need to be retrieved for
    levels = PythonOverriderCore._buildLocalizationLevelsList(availableLevels, level)
    
    # create a thrift instance
    thrift = ThriftClient.ThriftClient(localizationHost, localizationPort, '/services')    
    
    # retrieve a list of the localization files that will need to be merged                
    serverResponse = \
        _executeLocalizationFileListRetrieval(name, levels, localizedSite,
                                              localizationUser, loctype, thrift)
    # download the localization files
    lfiles =  _downloadLocalizationFiles(serverResponse, thrift, name)
    # complete the module merge
    return _executeModuleMerge(lfiles)

def _executeLocalizationFileListRetrieval(filename, localizationLevels, localizedSite, 
                                          localizationUser, loctype, thrift):
    '''
    @param filename: the name of the localization file
    @param localizationLevels: a list of localization levels that should be checked
        for the specified localization file
    @param localizedSite: the site that localization information should be
        retrieved for (if applicable)
    @param localizationUser: the user that localization information should
        be retrieved for (if applicable)
    @param loctype: the type of localization files to retrieve
    @param thrift: an instance of the thrift client used to communicate
        with EDEX
    @return: a list of the localization files associated with the specified
        file name wrapped in a server response
    @summary: this function will execute a list utility command via thrift to
        retrieve a list of localization files that match the specified file name,
        that match the specified localization type, and that can be found within the
        specified localization levels.
    '''
    
    directory = os.path.dirname(filename)
    
    cmds = []
    
    # prepare the localization type
    localizationType = LocalizationType(loctype)
    
    # build the request message
    req = UtilityRequestMessage()
    for level in localizationLevels:
        cmd = ListUtilityCommand()
        cmd.setSubDirectory(directory)
        cmd.setRecursive(False)
        cmd.setFilesOnly(True)
        cmd.setLocalizedSite(localizedSite)
    
        # prepare the localization level
        localizationLevel = LocalizationLevel(level)
    
        # create a localization context
        localizationContext = LocalizationContext()
        localizationContext.setLocalizationType(localizationType)
        localizationContext.setLocalizationLevel(localizationLevel)
        if level in ['CONFIGURED', 'SITE' ]:
            localizationContext.setContextName(localizedSite)
        elif level == 'USER':
            localizationContext.setContextName(localizationUser)
        
        # build the utility command
        cmd.setContext(localizationContext)
        cmds.append(cmd)
    
    # add the command(s) to the request
    req.setCommands(cmds)
    
    try:
        serverResponse = thrift.sendRequest(req)
    except Exception, e:
        raise RuntimeError,  'Could not retrieve localization file list: ' + str(e)    
    
    return serverResponse

def _downloadLocalizationFiles(serverResponse, thrift, name):
    '''
    @param serverResponse: a list of localization files that will need to
        be downloaded as well the localization context associated with
        each file
    @param thrift: an instance of the thrift client used to communicate
        with EDEX
    @param name: the name of the localization file to download. used
        to verify that .pyc files are not retrieved
    @return: a list of the localization files that have been downloaded
    @summary: this function will loop through the server response,
        execute a function that will retrieve the localization file
        from the server as bytes, and write the bytes to a temporary
        file in a dynamically generated location. 
    '''    
    
    lfiles = []
    
    for response in serverResponse.getResponses():
        for entry in response.getEntries():
            filename = entry.getFileName()
            if filename == name:
                # create a local copy of the file
                localizationContext = entry.getContext()
                bytes = _retrieveFileFromServer(localizationContext, filename, thrift)
                # create the temporary directory
                directoryName = tempfile.mkdtemp()
                filename = os.path.split(name)[1]
                fileToWrite = os.path.join(directoryName, filename)
                try:
                    with open(fileToWrite, 'wb') as fileHandle:
                        fileHandle.write(bytes)
                except:
                    # remove any files that have successfully
                    # been created.
                    _removeTemporaryLocalizationFiles(lfiles)
                    # fail
                    raise IOError('Failed to create a local copy of the localization file: ' + filename)
                
                lfiles.append(fileToWrite)
                break    
    
    return lfiles

def _executeModuleMerge(fileList):
    '''
    @param fileList: a list of python modules that will be merged
    @return: the merged module
    @summary: this function will run the module merge  
    '''    
    
    # perform the module merge
    try:
        themodule = PythonOverriderCore._internalOverride(fileList)
    finally:
        _removeTemporaryLocalizationFiles(fileList)
        
    return themodule

def _removeTemporaryLocalizationFiles(files):
    '''
    @param files: a list files to remove
    @summary: this function will remove the temporary localization
        files and directories
    '''
          
    for file in files:
        directory = os.path.dirname(file)
        shutil.rmtree(directory, True)

def _retrieveFileFromServer(localizationContext, filename, thrift):
    '''
    @param localizationContext: the localization context associated
        with the file that will be downloaded
    @param filename: the name of the file to download
    @param thrift: an instance of the thrift client used to communicate
        with EDEX
    @return: the bytes associated with the file that was downloaded
    @summary: 
    '''    
    
    request = LocalizationStreamGetRequest()
    request.setOffset(0)
    request.setNumBytes(BUFFER_SIZE)
    request.setContext(localizationContext)
    request.setMyContextName(localizationContext.getContextName())
    request.setFileName(filename)
    
    bytes = numpy.array([], numpy.int8)
    finished = False
    while (not finished):
        serverResponse = thrift.sendRequest(request)
        if not isinstance(serverResponse, SuccessfulExecution):
            message = ""
            if hasattr(serverResponse, 'getMessage'):
                message = serverResponse.getMessage()
            raise RuntimeError(message)
        serverResponse = serverResponse.getResponse()
        # serverResponse will be returned as a LocalizationStreamPutRequest
        # object. we'll use its methods to read back the serialized file 
        # data.
        # bytes get returned to us as an numpy.ndarray
        bytes = numpy.append(bytes, serverResponse.getBytes())
        request.setOffset(request.getOffset() + len(bytes))
        finished = serverResponse.getEnd()
        
    return bytes