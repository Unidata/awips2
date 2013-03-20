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


import sys, os, pwd, string, getopt, logging
import numpy

from dynamicserialize.dstypes.com.raytheon.uf.common.auth.resp import SuccessfulExecution
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetSingletonDbIdsRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetSiteTimeZoneInfoRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GridLocRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationContext
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationLevel
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationType
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.msgs import DeleteUtilityCommand
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.msgs import ListUtilityCommand
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.msgs import UtilityRequestMessage
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.msgs import PrivilegedUtilityRequestMessage
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.stream import LocalizationStreamGetRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.stream import LocalizationStreamPutRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from dynamicserialize.dstypes.com.raytheon.uf.common.plugin.nwsauth.user import User
from dynamicserialize.dstypes.com.raytheon.uf.common.plugin.nwsauth.user import UserId
from dynamicserialize.dstypes.com.raytheon.uf.common.site.requests import GetActiveSitesRequest
from ufpy import ThriftClient

#
# The ifpServerText program.  Stores, deletes, gets, and inventories text.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/17/10                      dgilling       Initial Creation.
#    
# 
#


class textInventoryRecord:
    
    def __init__(self, fileName, path="", localCtx=None, protected=False):
        self.fileName = fileName
        self.path = path
        if localCtx is None:
            self.localCtx = LocalizationContext()
            self.localCtx.setLocalizationType("UNKNOWN")
            self.localCtx.setLocalizationLevel("UNKNOWN")
        else:
            self.localCtx = localCtx
        self.protected = protected
    
    def __str__(self):
        return str(self.localCtx) + self.path + "/" + self.fileName
    
## Logging methods ##
logger = None
def __initLogger():
    global logger
    logger = logging.getLogger("ifpServerText")
    logger.setLevel(logging.DEBUG)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    # Uncomment line below to enable debug-level logging
    # ch.setLevel(logging.DEBUG)
    formatter = logging.Formatter("%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S")
    ch.setFormatter(formatter)
    logger.addHandler(ch)


class ifpServerText:
    
    BUFFER_SIZE = 512 * 1024
    
    EXTENSION_DICT = {"Config": ".py", 
                       "EditArea": ".xml",
                       "EditAreaGroup": ".txt", 
                       "SampleSet": ".xml",
                       "ColorTable": ".cmap", 
                       "WeatherElementGroup": ".xml",
                       "SelectTR": ".SELECTTR", 
                       "Tool": ".py",
                       "Procedure": ".py", 
                       "TextProduct": ".py",
                       "TextUtility": ".py", 
                       "Utility": ".py",
                       "Combinations": ".py",
                       "ISCUtility": ".py"
                      }
    
    LOCALIZATION_DICT = {"Config": ("CAVE_STATIC", "gfe/userPython/gfeConfig"),
                       "EditArea": ("COMMON_STATIC", "gfe/editAreas"),
                       "EditAreaGroup": ("COMMON_STATIC", "gfe/editAreaGroups"),
                       "SampleSet": ("COMMON_STATIC", "gfe/sampleSets"),
                       "ColorTable": ("CAVE_STATIC", "colormaps/GFE"),
                       "WeatherElementGroup": ("CAVE_STATIC", "gfe/weGroups"),
                       "SelectTR": ("COMMON_STATIC", "gfe/text/selecttr"),
                       "Tool": ("CAVE_STATIC", "gfe/userPython/smartTools"),
                       "Procedure": ("CAVE_STATIC", "gfe/userPython/procedures"),
                       "TextProduct": ("CAVE_STATIC", "gfe/userPython/textProducts"),
                       "TextUtility": ("CAVE_STATIC", "gfe/userPython/textUtilities/regular"),
                       "Utility": ("CAVE_STATIC", "gfe/userPython/utilities"),
                       "Combinations": ("CAVE_STATIC", "gfe/combinations"),
                       "ISCUtility": ("COMMON_STATIC", "isc/utilities")
                       }

    def __init__(self):
        self.__host = None
        self.__port = None
        self.__siteID = None
        self.__user = None
        self.__mode = None
        self.__name = None
        self.__filename = None
        self.__classType = None
        self.__textCategory = None
        self.__metaInfo = None
        self.__osUser = pwd.getpwuid(os.getuid()).pw_name

        self.__cmdLine()
        
        self.__thrift = ThriftClient.ThriftClient(self.__host, self.__port, "/services")
        
        # build inventory:
        if self.__textCategory is not None:
            self.__db = self.__buildInventory()
        

    def process(self):
        # meta information
        if self.__metaInfo is not None:
            self.__metaInformation()

        # edit area
        elif self.__classType == "EditArea":
            if self.__mode == "SAVE":
                self.__saveEA()
            elif self.__mode == "DELETE":
                self.__deleteEA()
            elif self.__mode == "GET":
                self.__getEA()
            elif self.__mode == "INVENTORY":
                self.__inventoryEA()

        # edit area group
        elif self.__classType == "EditAreaGroup":
            if self.__mode == "SAVE":
                self.__saveEAGroup()
            elif self.__mode == "DELETE":
                self.__deleteEAGroup()
            elif self.__mode == "GET":
                self.__getEAGroup()
            elif self.__mode == "INVENTORY":
                self.__inventoryEAGroup()

        # sample set
        elif self.__classType == "SampleSet":
            if self.__mode == "SAVE":
                self.__saveSamples()
            elif self.__mode == "DELETE":
                self.__deleteSamples()
            elif self.__mode == "GET":
                self.__getSamples()
            elif self.__mode == "INVENTORY":
                self.__inventorySamples()

        # color table
        elif self.__classType == "ColorTable":
            if self.__mode == "SAVE":
                self.__saveCT()
            elif self.__mode == "DELETE":
                self.__deleteCT()
            elif self.__mode == "GET":
                self.__getCT()
            elif self.__mode == "INVENTORY":
                self.__inventoryCT()

        # text stuff
        elif self.__textCategory is not None:
            if self.__mode == "SAVE":
                self.__saveText()
            elif self.__mode == "DELETE":
                self.__deleteText()
            elif self.__mode == "GET":
                self.__getText()
            elif self.__mode == "INVENTORY":
                self.__inventoryText()

        #error
        else:
            raise Exception, "Unknown class type " + self.__classType


    def __cmdLine(self):
        optlist, oargs = getopt.getopt(sys.argv[1:], "h:p:o:u:sn:f:c:digm:")
        for opt in optlist:
            if opt[0] == '-h':
                self.__host = opt[1]
            elif opt[0] == '-p':
                self.__port = int(opt[1])
            elif opt[0] == '-o':
                self.__siteID = opt[1]                
            elif opt[0] == '-u':
                self.__user = opt[1]
            elif opt[0] == '-n':
                self.__name = opt[1]
            elif opt[0] == '-f':
                self.__filename = opt[1]
            elif opt[0] == '-m':
                self.__metaInfo = opt[1]
            elif opt[0] == '-c':
                if self.__classType is not None:
                    self.__usage()
                    raise SyntaxWarning, "Too many -c switches specified"
                options = ["Tool", "Procedure", "Utility", "TextUtility",
                           "TextProduct", "Config", "EditArea", "SelectTR",
                           "EditAreaGroup", "SampleSet", "WeatherElementGroup",
                           "ColorTable", "Combinations", "SmartTool", "ISCUtility"]
                if opt[1] not in options:
                    self.__usage()
                    s = "Error: Illegal class specified  " + opt[1]
                    raise SyntaxWarning, s
                self.__classType = opt[1]

                if self.__classType == "SmartTool":
                    self.__classType = "Tool"   #maintain backwards compatible
				
                dic = {"Config": "GFECONFIG", "EditArea": "EditAreas",
                       "EditAreaGroup": "EditAreaGroup", "SampleSet": "SampleSets",
                       "ColorTable": "ColorTable",
                       "WeatherElementGroup": "BUNDLE",
                       "SelectTR": "SELECTTR", "Tool": "Tool",
                       "Procedure": "Procedure", "TextProduct": "TextProduct",
                       "TextUtility": "TextUtility", "Utility": "Utility",
                       "Combinations": "COMBINATIONS", "ISCUtility": "ISCUtility"}
                self.__textCategory = dic[self.__classType] 
            elif opt[0] == '-s':
                if self.__mode is not None:
                    self.__usage()
                    raise SyntaxWarning, "Error: More than one mode specified"
                self.__mode = 'SAVE'
            elif opt[0] == '-d':
                if self.__mode is not None:
                    self.__usage()
                    raise SyntaxWarning, "Error: More than one mode specified"
                self.__mode = 'DELETE'
            elif opt[0] == '-g':
                if self.__mode is not None:
                    self.__usage()
                    raise SyntaxWarning, "Error: More than one mode specified"
                self.__mode = 'GET'
            elif opt[0] == '-i':
                if self.__mode is not None:
                    self.__usage()
                    raise SyntaxWarning, "Error: More than one mode specified"
                self.__mode = 'INVENTORY'

        # sanity checks, make sure all required switches are specified
        if self.__user is None:
            self.__user = self.__osUser
        
        if self.__host is None or self.__port is None:
            self.__usage()
            raise SyntaxWarning, "Error: Missing host or port"
        
        if self.__siteID is None and self.__metaInfo not in ['sitetimezone', 'site']:
            self.__usage()
            raise SyntaxWarning, "Error: Missing siteID information"

        if self.__mode is None and self.__metaInfo is None:
           self.__usage()
           raise SyntaxWarning, \
             "Error: Missing Mode -m, -s, -d, -i, or -n switch"

        if self.__mode == "INVENTORY" and self.__classType is None:
            self.__usage()
            raise SyntaxWarning, \
              "Error: INVENTORY mode requires -c switch"

        if self.__mode == "SAVE" and \
          (self.__name is None or self.__filename is None or \
          self.__classType is None):
            self.__usage()
            raise SyntaxWarning, \
              "Error: SAVE mode requires -n, -f, and -c switches"
        
        if self.__mode == "DELETE" and \
           self.__user not in [self.__osUser, "SITE"]:
            self.__usage()
            raise SyntaxWarning, \
              "Error: DELETE mode can only be performed on SITE or " + self.__osUser + " owned files"
              
        if self.__mode == "SAVE" and \
           self.__user not in [self.__osUser, "SITE"]:
            self.__usage()
            raise SyntaxWarning, \
              "Error: SAVE mode can only be performed on SITE or " + self.__osUser + " owned files"

        if self.__mode == "DELETE" and (self.__name is None or \
          self.__classType is None):
            self.__usage()
            raise SyntaxWarning, \
              "Error: DELETE mode requires -n and -c switches"
        
        if self.__mode == "GET" and (self.__name is None or \
          self.__classType is None):
            self.__usage()
            raise SyntaxWarning, \
              "Error: GET mode requires -n and -c switches"

        if self.__metaInfo is not None:
            valid = ["site", "singleton", "sitetimezone", "domain"]
            if self.__metaInfo not in valid:
                self.__usage()
                raise SyntaxWarning, "Error: unknown -m keyword found"
            elif self.__mode is not None:
                self.__usage()
                raise SyntaxWarning, \
                  "Error: -m with -s, -d, -i, -n switches not compatible with each other"
        

    def __usage(self):
        print """
Usage: ifpServerText -h hostname -p rpcPortNumber -o siteID [-u user]
   [-s -n name -f filename -c class] 
   [-d -n name [-c class]]
   [-i [-c class]]
   [-g [-f filename] -n [-c class]]
   [-m infoType [-f filename]]

    -h host where the ifpServer is running
    -p rpc port number for the ifpServer.
    -o siteid to retrieve information for.    
    -u userid, defaults to login user
    -s SAVEMODE
       -n name to store text under
       -f filename of source
       -c class, one of 
           EditAreaGroup, EditArea, WeatherElementGroup, ColorTable
           SelectTR, SampleSet, Tool, Procedure, Utility,
           TextUtility, TextProduct, Config, Combinations
    -d DELETEMODE
       -n name to delete from ifpServer
       -c class, one of 
           EditAreaGroup, EditArea, WeatherElementGroup, ColorTable
           SelectTR, SampleSet, Tool, Procedure, Utility,
           TextUtility, TextProduct, Config, Combinations
    -i INVENTORYMODE
       -c class, one of 
           EditAreaGroup, EditArea, WeatherElementGroup, ColorTable
           SelectTR, SampleSet, Tool, Procedure, Utility,
           TextUtility, TextProduct, Config, Combinations
    -g GETMODE
       -n name to get from ifpServer
       -f filename to store under, or if not specified stdout
       -c class, one of 
           EditAreaGroup, EditArea, WeatherElementGroup, ColorTable
           SelectTR, SampleSet, Tool, Procedure, Utility,
           TextUtility, TextProduct, Config, Combinations
    -m infoType
       Depending upon infoType, simply returns the requested information.
       infoType can be: site (for a list of site identifiers in use),
       sitetimezone (for a list of time zones in use), 
       singleton (for a list of single databases in use)
       domain (for the database grid location, domain)
       -f filename to store under, or if not specified stdout

"""

    def __buildInventory(self):
        invTuple = self.LOCALIZATION_DICT[self.__classType]
        localLevels = ["BASE", "CONFIGURED", "SITE", "USER"]
        if self.__user == "SITE":
            localLevels = localLevels[:-1]
        if self.__user == "CONFIGURED":
            localLevels = localLevels[:-2]
        elif self.__user == "BASE":
            localLevels = localLevels[0]
        
        req = UtilityRequestMessage()
        cmds = []
        
        for level in localLevels:             
            cmd = ListUtilityCommand()
            cmd.setSubDirectory(invTuple[1])
            cmd.setRecursive(False)
            cmd.setFilesOnly(True)
            cmd.setLocalizedSite(self.__siteID)
            ctx = LocalizationContext()
            ll = LocalizationLevel(level)
            type = LocalizationType(invTuple[0])
            ctx.setLocalizationType(type)
            ctx.setLocalizationLevel(ll)
            if level in ["CONFIGURED", "SITE"]:
                ctx.setContextName(self.__siteID)
            elif (level == "USER"):
                ctx.setContextName(self.__user)
            cmd.setContext(ctx)
            cmds.append(cmd)
    
        req.setCommands(cmds)
        
        try:
            serverResponse = self.__thrift.sendRequest(req)
        except Exception, e:
            raise RuntimeError,  "Could not retrieve product inventory: " + str(e)
           
        inventory = {} 
        for response in serverResponse.getResponses():
            for entry in response.getEntries():
                filename = os.path.split(entry.getFileName())[1]
                shortName = os.path.splitext(filename)[0]
                record = textInventoryRecord(filename, response.getPathName(), entry.getContext(), entry.getProtectedFile())
                inventory[shortName] = record
        
        return inventory
        

    def __saveText(self):
        #Saves a text file

        # read the file from disk
        f = open(self.__filename, 'r')
        txt = f.read()
        f.close()

        # verify the class based on the contents of the file
        if self.__classType == "Tool":
            self.__verifyClass(txt, "class Tool")
        elif self.__classType == "Procedure":
            self.__verifyClass(txt, "class Procedure")
        elif self.__classType == "TextProduct":
            self.__verifyClass(txt, "class TextProduct")

        # Store the main file
        # need to convert python bytearray type (which is just a list of ints)
        # to numpy.int8 type to ensure this data is serialized as bytes
        bytes = numpy.asarray(bytearray(txt, 'utf8'), dtype=numpy.int8)
        totalSize = len(bytes)
        request = LocalizationStreamPutRequest()
        request.setOffset(0)
        
        localizationInfo = self.LOCALIZATION_DICT[self.__classType]
        if self.__user != "SITE":
            levelName = "USER"
        else:
            levelName = self.__user
        ctx = LocalizationContext()
        ll = LocalizationLevel(levelName)
        type = LocalizationType(localizationInfo[0])
        ctx.setLocalizationType(type)
        ctx.setLocalizationLevel(ll)
        if self.__user != "SITE":
            ctx.setContextName(self.__user)
        else:
            ctx.setContextName(self.__siteID)
        request.setContext(ctx)
        request.setMyContextName(ctx.getContextName())
        request.setFileName(localizationInfo[1] + "/" + self.__name + self.EXTENSION_DICT[self.__classType])
        
        totalSent = 0
        finished = False
        while (not finished):
            request.setOffset(totalSent)
            sendBuffer = bytes[:self.BUFFER_SIZE]
            bytes = bytes[self.BUFFER_SIZE:]
            totalSent += len(sendBuffer)
            request.setBytes(sendBuffer)
            request.setEnd(totalSent == totalSize)
            finished = request.getEnd()
            try:
                serverResponse = self.__thrift.sendRequest(request)
                if not isinstance(serverResponse, SuccessfulExecution):
                    message = ""
                    if hasattr(serverResponse, "getMessage"):
                        message = serverResponse.getMessage()
                    raise RuntimeError(message)
                serverResponse = serverResponse.getResponse()
            except Exception, e:
                raise RuntimeError("Could not send file to localization server: " + str(e))
            
        logger.info("Saved file " + self.__filename + " under " + self.__name)

    def __deleteText(self):
        #Deletes a text file
        keys = self.__db.keys()
        if self.__name not in keys:
            s = "DELETE failed since " + self.__name + " not in inventory"
            raise KeyError, s
        
        record = self.__db[self.__name]
        
        if self.__user != "SITE" and str(record.localCtx.getLocalizationLevel()) != "USER":
            raise RuntimeError, "User can only delete user-level files."
        
        if self.__user == "SITE" and str(record.localCtx.getLocalizationLevel()) != "SITE":
            raise RuntimeError, "SITE can only delete site-level files."
        
        req = PrivilegedUtilityRequestMessage()
        cmds = []
        cmd = DeleteUtilityCommand()
        cmd.setContext(record.localCtx)
        cmd.setFilename(record.path + "/" + record.fileName)
        cmd.setMyContextName(record.localCtx.getContextName())
        cmds.append(cmd)
        # make sure to delete the .pyo and .pyc files as well
        if os.path.splitext(record.fileName)[1] == ".py":
            for ext in ["c", "o"]:
                cmd = DeleteUtilityCommand()
                cmd.setContext(record.localCtx)
                cmd.setFilename(record.path + "/" + record.fileName + ext)
                cmd.setMyContextName(record.localCtx.getContextName())
                cmds.append(cmd)
        req.setCommands(cmds)
        
        try:
            serverResponse = self.__thrift.sendRequest(req)
            if not isinstance(serverResponse, SuccessfulExecution):
                message = ""
                if hasattr(serverResponse, "getMessage"):
                    message = serverResponse.getMessage()
                raise RuntimeError(message)
            serverResponse = serverResponse.getResponse()
        except Exception, e:
            raise RuntimeError("Could not delete file from localization server: " + str(e))

        logger.info("Deleted " + self.__name)

    def __getText(self):
        #Gets the text file
        keys = self.__db.keys()
        if self.__name not in keys:
            s = "GET failed since " + self.__name + " not in inventory"
            raise KeyError, s
        
        invRecord = self.__db[self.__name]
        request = LocalizationStreamGetRequest()
        request.setOffset(0)
        request.setNumBytes(self.BUFFER_SIZE)
        request.setContext(invRecord.localCtx)
        request.setMyContextName(invRecord.localCtx.getContextName())
        request.setFileName(invRecord.path + "/" + invRecord.fileName)
        
        finished = False
        txt = ""
        while (not finished):
            try:
                serverResponse = self.__thrift.sendRequest(request)
                if not isinstance(serverResponse, SuccessfulExecution):
                    message = ""
                    if hasattr(serverResponse, "getMessage"):
                        message = serverResponse.getMessage()
                    raise RuntimeError(message)
                serverResponse = serverResponse.getResponse()
            except Exception, e:
                raise RuntimeError("Could not retrieve file from localization server: " + str(e))
            # serverResponse will be returned as a LocalizationStreamPutRequest
            # object. we'll use its methods to read back the serialized file 
            # data.
            # bytes get returned to us as an numpy.ndarray
            bytes = serverResponse.getBytes()
            txt += bytes.tostring()
            request.setOffset(request.getOffset() + len(bytes))
            finished = serverResponse.getEnd()
        
        if self.__filename is None:
            print txt
        else:
            f = open(self.__filename, 'w', 0644)
            f.write(txt)
            f.close()
            logger.info("Got " + self.__name + " --- written to " + self.__filename)

    def __inventoryText(self):
        #Returns the inventory
        keys = self.__db.keys()
        
        print "%-40s" % "Name", "%-15s" % "Access", "Protect"
        print "%-40s" % "----", "%-15s" % "------", "-------"
        keys.sort()
        for k in keys:
            a = self.__db[k]
            if a.protected == True:
                pro = 'Read-Only'
            else:
                pro = 'Read-Write'
            print "%-40s" % k, "%-15s" % a.localCtx.getLocalizationLevel(), pro

    def __verifyClass(self, txt, searchString):
        lines = string.split(txt, '\n')
        for line in lines:
            index = string.find(line, searchString)
            if index != -1:
                return 1
        s = "Input file is not written in class format or contains improper type"
        raise Exception, s
        return 0

    def __saveSamples(self):
        #Saves a sample set  format: #points, lon/lat, lon/lat, .....
        self.__saveText()

    def __deleteSamples(self):
        #Deletes a sample set
        self.__deleteText()

    def __getSamples(self):
        #Gets a sample set
        self.__getText()

    def __inventorySamples(self):
        #Returns the inventory for sample sets
        self.__inventoryText()

    def __saveCT(self):
        #Saves a color table format: #points, rgb, rgb, rgb, rgb
        self.__saveText()

    def __deleteCT(self):
        #Deletes a color table
        self.__deleteText()

    def __getCT(self):
        #Gets a color table
        self.__getText()

    def __inventoryCT(self):
        #Returns the inventory for color tables
        self.__inventoryText()

    def __saveEA(self):
        #Saves an edit area
        self.__saveText()

    def __deleteEA(self):
        #Deletes a text file
        self.__deleteText()

    def __getEA(self):
        #Gets the edit area
        self.__getText()

    def __inventoryEA(self):
        #Returns the inventory for edit areas
        self.__inventoryText()

    def __saveEAGroup(self):
        #Saves a text file
        self.__saveText()

    def __deleteEAGroup(self):
        #Deletes an edit area group
        self.__deleteText()

    def __getEAGroup(self):
        #Gets the text file, decodes it
        self.__getText()

    def __inventoryEAGroup(self):
        #Returns the inventory
        self.__inventoryText()


    def __metaInformation(self):
        # gets the meta information
        txt = ""
        wsId = WsId(progName="ifpServerText")
        if self.__metaInfo == "sitetimezone":
            request = GetSiteTimeZoneInfoRequest()
            request.setWorkstationID(wsId)
            request.setSiteID("")
            try:
                serverResponse = self.__thrift.sendRequest(request)
            except Exception, e:
                raise RuntimeError,  "Could not retrieve meta information: " + str(e)
            
            if (serverResponse.isOkay()):
                tzInfo = serverResponse.getPayload()
                for k in tzInfo.keys():
                    txt = txt + k + ' ' + tzInfo[k] + "\n"
            else:
                raise Exception, serverResponse.message()
        elif self.__metaInfo == "site":
            request = GetActiveSitesRequest()
            try:
                serverResponse = self.__thrift.sendRequest(request)
            except Exception, e:
                raise RuntimeError,  "Could not retrieve meta information: " + str(e)
            
            for site in serverResponse:
                txt = txt + site + "\n"

        elif self.__metaInfo == "singleton":
            request = GetSingletonDbIdsRequest()
            request.setWorkstationID(wsId)
            request.setSiteID(self.__siteID)
            try:
                serverResponse = self.__thrift.sendRequest(request)
            except Exception, e:
                raise RuntimeError,  "Could not retrieve meta information: " + str(e)
            
            if (serverResponse.isOkay()):
                singletons = serverResponse.getPayload()
                for s in singletons:
                    txt = txt + str(s) + "\n"
            else:
                raise Exception, serverResponse.message()
        elif self.__metaInfo == "domain":
            request = GridLocRequest()
            request.setWorkstationID(wsId)
            request.setSiteID(self.__siteID)
            try:
                serverResponse = self.__thrift.sendRequest(request)
            except Exception, e:
                raise RuntimeError,  "Could not retrieve meta information: " + str(e)
            
            if (serverResponse.isOkay()):
                domain = serverResponse.getPayload()[0]
                proj = domain.getProjection()
                txt = "ProjectionID: " + proj.getProjectionID() + "\n"
                txt = txt + "Grid Size: " + `(domain.getNx(), domain.getNy())` + "\n"
                txt = txt + "Grid Domain: " + `(domain.getOrigin(), domain.getExtent())` + "\n"
                
                keys = proj.keys()
                for k in keys:
                    txt = txt + k + ': ' + str(getattr(proj, k)) + "\n"
            else:
                raise Exception, serverResponse.message()
        if self.__filename is None:
            print txt
        else:
            f = open(self.__filename, 'w', 0644)
            f.write(txt)
            f.close()
            logger.info("Got MetaInfo " + self.__metaInfo + " --- written to " + self.__filename)


def main():
    __initLogger()
    logger.info("ifpServerText Starting")

    try:
        obj = ifpServerText()
        obj.process()
    except Exception, e:
        logger.exception("Error encountered running ifpServerText:")
        sys.exit(1)

    logger.info("ifpServerText Finished")
    sys.exit(0)


if __name__ == "__main__":
    main()
    
