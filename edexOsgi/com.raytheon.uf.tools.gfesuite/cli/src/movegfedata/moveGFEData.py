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


import logging, sys, os
import numpy

from dynamicserialize.dstypes.com.raytheon.uf.common.auth.resp import SuccessfulExecution
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetActiveSitesRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.message import ServerResponse
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

from ufpy import ThriftClient
from ufpy import UsageArgumentParser


#
# The moveGFEData program.  Moves data from one user to another.
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



SourceUser = ""
DestUser = ""
SourceSite = ""
DestSite = ""
Source = None
Dest = None
CopyOnly = False
Action = " move "

BUFFER_SIZE = 512 * 1024
LOCALIZATION_DICT = {"GFECONFIG": ("CAVE_STATIC", "gfe/userPython/gfeConfig"),
                     "EditArea": ("COMMON_STATIC", "gfe/editAreas"),
                     "EditAreaGroup": ("COMMON_STATIC", "gfe/editAreaGroups"),
                     "SampleSet": ("COMMON_STATIC", "gfe/sampleSets"),
                     "ColorTable": ("CAVE_STATIC", "colormaps/GFE"),
                     "BUNDLE": ("COMMON_STATIC", "gfe/weGroups"),
                     "SELECTTR": ("COMMON_STATIC", "gfe/text/selecttr"),
                     "Tool": ("CAVE_STATIC", "gfe/userPython/smartTools"),
                     "Procedure": ("CAVE_STATIC", "gfe/userPython/procedures"),
                     "TextProduct": ("CAVE_STATIC", "gfe/userPython/textProducts"),
                     "TextUtility": ("CAVE_STATIC", "gfe/userPython/textUtilities/regular"),
                     "Utility": ("CAVE_STATIC", "gfe/userPython/utilities"),
                     "COMBODATA": ("CAVE_STATIC", "gfe/comboData"),
                     "COMBINATIONS": ("CAVE_STATIC", "gfe/combinations")}
LOCALIZATION_LEVELS = ["BASE", "CONFIGURED", "SITE", "USER"]


class textInventoryRecord:
    def __init__(self, filePath, localCtx=None, protected=False):
        self.filePath = filePath
        if localCtx is None:
            self.localCtx = LocalizationContext()
            self.localCtx.setLocalizationType("UNKNOWN")
            self.localCtx.setLocalizationLevel("UNKNOWN")
        else:
            self.localCtx = localCtx
        self.protected = protected
    
    def __str__(self):
        return str(self.localCtx) + self.filePath


## Logging methods ##
def __initLogger():
    logger = logging.getLogger("moveGFEData.py")
    logger.setLevel(logging.INFO)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    formatter = logging.Formatter("%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S")
    ch.setFormatter(formatter)
    logger.addHandler(ch)
    
def logEvent(msg):
    logging.getLogger("moveGFEData.py").info(msg)

def logProblem(msg):
    logging.getLogger("moveGFEData.py").error(msg)
    
def logException(msg):
    logging.getLogger("moveGFEData.py").exception(msg)    

def logVerbose(msg):
    logging.getLogger("moveGFEData.py").debug(msg)


def main():
    __initLogger()
    options = validateArgs()
    
    global SourceUser
    SourceUser = options.sourceUser
    global DestUser
    DestUser = options.destUser
    if options.srcSiteID is not None:
        global SourceSite
        SourceSite = options.srcSiteID
    if options.destSiteID is not None:
        global DestSite
        DestSite = options.destSiteID
    global CopyOnly
    CopyOnly = options.copyOnly
    
    category = getOperationCategory(SourceUser, DestUser)
    
    if category > 0:
        textCategories = [None, ("Color Table", "ColorTable"), 
                          ("Edit Area", "EditArea"), ("Sample Set", "SampleSet"), 
                          ("Weather Element Groups", "BUNDLE"), 
                          ("Edit Area Groups", "EditAreaGroup"),
                          ("GFE Configuration Files", "GFECONFIG"),
                          ("Selection Time Ranges", "SELECTTR"),  
                          ("Smart Tools", "Tool"), 
                          ("Procedures", "Procedure"), ("Utilities", "Utility"), 
                          ("Text Utilities", "TextUtility"), 
                          ("Text Formatters", "TextProduct"), 
                          ("Zone Combiner Saved Combos and Colors", "COMBODATA"), 
                          ("Zone Combiner Combination Files", "COMBINATIONS")]
        textShuffle(textCategories[category][0], textCategories[category][1])
    
    sys.exit(0)
    

def validateArgs():
    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="srcHost",
                      help="upon which the ifpServer is running", 
                      required=True, metavar="hostname")
    parser.add_argument("-p", action="store", type=int, dest="srcPort", 
                      help="the port that ifpServer is serving",
                      required=True, metavar="portNumber")
    parser.add_argument("-s", action="store", dest="sourceUser", 
                      help="user from which to copy the data",
                      required=True, metavar="sourceUser")                      
    parser.add_argument("-d", action="store", dest="destUser", 
                      help="user to copy the data to",
                      required=True, metavar="destUser")
    parser.add_argument("-c", action="store_true", dest="copyOnly", 
                      help="copy only, do not delete original")
    parser.add_argument("-a", action="store", dest="destHost", 
                      help="destination server host, if different from primary", 
                      metavar="hostname2")
    parser.add_argument("-b", action="store", type=int, dest="destPort", 
                      help="destination server port, if different from primary",
                      metavar="port2")
    parser.add_argument("-w", action="store", dest="srcSiteID", 
                      help="source site ID",
                      metavar="sourceSiteID")
    parser.add_argument("-x", action="store", dest="destSiteID", 
                      help="destination site ID",
                      metavar="destSiteID")
    options = parser.parse_args()
    
    logEvent("MoveGFEData from [" + options.sourceUser + "] to [" + options.destUser + "]")
    
    if ((options.sourceUser == "BASE" and not options.copyOnly) or \
        (options.destUser == "BASE")):
        parser.error("Operations not permitted with user \'BASE\'")
        
    if ((options.sourceUser == "CONFIGURED" and not options.copyOnly) or \
        (options.destUser == "CONFIGURED")):
        parser.error("Operations not permitted with user \'CONFIGURED\'")
        
    if options.sourceUser == "SITE" and not options.copyOnly:
        parser.error("Must specify the copy only flag with source user \'SITE\'")
        
    if (options.destHost is None):
        options.destHost = options.srcHost
    
    if (options.destPort is None):
        options.destPort = options.srcPort
    
    if (options.srcHost != options.destHost) or (options.srcPort != options.destPort):
        logEvent("Multiple servers: Source: " + options.srcHost + ":" +str(options.srcPort) + " Dest: " + options.destHost + ":" +str(options.destPort))
        
    if options.copyOnly:
        global Action
        Action = " copy "
    
    # test connectivity
    global Source
    Source = ThriftClient.ThriftClient(options.srcHost, options.srcPort, "/services")
    global Dest
    Dest = ThriftClient.ThriftClient(options.destHost, options.destPort, "/services")
    request = createSitesRequest()
    try:
        sr1 = Source.sendRequest(request)
        sr2 = Dest.sendRequest(request)
    except:
        parser.error("Unable to connect to ifpServer")
    if not sr1.isOkay() or not sr2.isOkay():
        parser.error("Unable to connect to ifpServer")
    
    activeSites = sr1.getPayload()
    if options.srcSiteID is None:
        if len(activeSites) == 1:
            options.srcSiteID = activeSites.pop()
        else:
            parser.error("Could not determine active site ID of ifpServer. Please provide -w flag.")
    
    if options.destSiteID is None:
        options.destSiteID = options.srcSiteID
    
    if ((options.sourceUser == "SITE" and options.srcSiteID is None) or \
         (options.destUser == "SITE" and options.destSiteID is None)):
        parser.error("A site ID must be provided with user \'SITE\'")
    
    if (options.destHost == options.srcHost and \
        options.destPort == options.srcPort and \
        options.destUser == options.sourceUser and \
        options.sourceUser != "SITE"):
        parser.error("Source user must be different from destination user with same server/host")
        
    if (options.destHost == options.srcHost and \
        options.destPort == options.srcPort and \
        options.destUser == options.sourceUser and \
        options.sourceUser == "SITE" and \
        options.srcSiteID == options.destSiteID):
        parser.error("Source siteID must be different from destination siteID with same server/host")
    
    return options
    
def createSitesRequest():    
    obj = GetActiveSitesRequest()
    wsId = WsId(progName="moveGfeData")
    obj.setWorkstationID(wsId)
    obj.setSiteID("")
    return obj

def getOperationCategory(SourceUser, DestUser):
    banner = "******************* MoveGFEData ****************** \n" + \
             "SourceUser: " + SourceUser + "\n" + \
             "DestinationUser: " + DestUser + "\n" + \
             "\n" + \
             "Enter type of data.  Choose one of the options: \n" + \
             "0 - exit \n" + \
             "1 - Color Tables \n" + \
             "2 - Edit Areas (Reference Areas) \n" + \
             "3 - Sample Sets \n" + \
             "4 - Weather Element Groups \n" + \
             "5 - Edit Area Groups \n" + \
             "6 - GFE/ifpIMAGE Configurations \n" + \
             "7 - Selection Time Ranges \n" + \
             "8 - Smart Tools \n" + \
             "9 - Procedures \n" + \
             "10 - Utilities \n" + \
             "11 - Text Utilities \n" + \
             "12 - Text Formatters \n" + \
             "13 - Zone Combiner Saved Combos and Colors \n" + \
             "14 - Zone Combiner Combination Files"
    print banner
    category = int(raw_input())
    if category > 14:
        category = 0
    return category

def textShuffle(title, category):    
    print "\n" + "***** " + title + " *****"
    
    while True:
        inventory = getTextInventory(category)
        if inventory is None:
            return
        if len(inventory) < 1:
            print "No files available to" + Action
            return
        
        print "Current Inventory for User: ", SourceUser
        print "0.  Exit"
        print "-1. ALL Entries"
        sortedInv = sorted(inventory.keys())
        for i, key in enumerate(sortedInv):
            print str(i + 1) + ".  " + key
        print "Enter number to" + Action + "from " + SourceUser + " to " + DestUser
        
        answer = int(raw_input())
        if (answer == 0):
            return
        if answer > len(inventory):
            continue
        
        filesToMove = []
        if answer == -1:
            filesToMove = sortedInv
        else:
            filesToMove = [sortedInv[answer - 1]]
        for file in filesToMove:
            if not moveText(file, inventory[file], title, category):
                return

def moveText(id, invRecord, title, category):
    # get the data
    try:
        fileData = getTextData(invRecord)
    except:
        logException("Error getting data: " + id + " ")
        return False
    
    # store the data
    try:
        saveTextData(invRecord, fileData, category)
    except:
        logException("Error saving data: " + id + " ")
        return False
    logEvent("Copied " + title + " [" + id + "] to [" + DestUser + "]")
    
    # delete the original
    if not CopyOnly:
        try:
            deleteTextData(invRecord)
        except:
            logException("Error deleting data: " + id + " ")
            return False
        logEvent("Deleted " + title + " [" + id + "] from [" + SourceUser + "]")
            
    return True

def getTextInventory(category):
    if SourceUser not in ["SITE", "CONFIGURED", "BASE"]:
        locLevels = [LOCALIZATION_LEVELS[-1]]
    elif SourceUser == "SITE":
        locLevels = [LOCALIZATION_LEVELS[-2]]
    elif SourceUser == "CONFIGURED":
        locLevels = [LOCALIZATION_LEVELS[1]]
    else:
        locLevels = [LOCALIZATION_LEVELS[0]]
    
    invTuple = LOCALIZATION_DICT[category]
    inventory = {} 
    for level in locLevels:
        req = UtilityRequestMessage()
        cmds = []
        cmd = ListUtilityCommand()
        cmd.setSubDirectory(invTuple[1])
        cmd.setRecursive(False)
        cmd.setFilesOnly(True)
        cmd.setLocalizedSite(SourceSite)
        ctx = LocalizationContext()
        ll = LocalizationLevel(level)
        type = LocalizationType(invTuple[0])
        ctx.setLocalizationType(type)
        ctx.setLocalizationLevel(ll)
        if (level == "USER"):
            ctx.setContextName(SourceUser)
        if (level in ["CONFIGURED", "SITE"]):
            ctx.setContextName(SourceSite)
        cmd.setContext(ctx)
        cmds.append(cmd)
        req.setCommands(cmds)
        
        try:
            serverResponse = Source.sendRequest(req)
        except:
            logException("Error getting inventory.")
            return None
           
        for response in serverResponse.getResponses():
            for entry in response.getEntries():
                if not entry.getProtectedFile():
                    filePath = entry.getFileName()
                    filename = os.path.split(filePath)[1]
                    shortName = os.path.splitext(filename)[0]
                    record = textInventoryRecord(filePath, entry.getContext(), entry.getProtectedFile())
                    inventory[shortName] = record
    
    return inventory

def getTextData(invRecord):
    request = LocalizationStreamGetRequest()
    request.setOffset(0)
    request.setNumBytes(BUFFER_SIZE)
    request.setContext(invRecord.localCtx)
    request.setMyContextName(invRecord.localCtx.getContextName())
    request.setFileName(invRecord.filePath)
    
    bytes = numpy.array([], numpy.int8)
    finished = False
    while (not finished):
        serverResponse = Source.sendRequest(request)
        if not isinstance(serverResponse, SuccessfulExecution):
            message = ""
            if hasattr(serverResponse, "getMessage"):
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

def saveTextData(invRecord, fileData, category):
    totalSize = len(fileData)
    request = LocalizationStreamPutRequest()
    request.setOffset(0)
    ctx = LocalizationContext()
    type = invRecord.localCtx.getLocalizationType()
    if (DestUser != "SITE"):
        ll = LocalizationLevel("USER")
        ctx.setContextName(DestUser)
    else:
        ll = LocalizationLevel("SITE")
        ctx.setContextName(DestSite)
    ctx.setLocalizationType(type)
    ctx.setLocalizationLevel(ll)
    request.setContext(ctx)
    request.setMyContextName(ctx.getContextName())
    fileName = os.path.split(invRecord.filePath)[1]
    path = LOCALIZATION_DICT[category][1]
    request.setFileName(os.path.join(path, fileName))
    
    totalSent = 0
    finished = False
    while (not finished):
        request.setOffset(totalSent)
        sendBuffer = fileData[:BUFFER_SIZE]
        fileData = fileData[BUFFER_SIZE:]
        totalSent += len(sendBuffer)
        request.setBytes(sendBuffer)
        request.setEnd(totalSent == totalSize)
        finished = request.getEnd()
        serverResponse = Dest.sendRequest(request)
        if not isinstance(serverResponse, SuccessfulExecution):
            message = ""
            if hasattr(serverResponse, "getMessage"):
                message = serverResponse.getMessage()
            raise RuntimeError(message)
        serverResponse = serverResponse.getResponse()

def deleteTextData(invRecord):
    req = PrivilegedUtilityRequestMessage()
    cmds = []
    cmd = DeleteUtilityCommand()
    cmd.setContext(invRecord.localCtx)
    cmd.setFilename(invRecord.filePath)
    cmd.setMyContextName(invRecord.localCtx.getContextName())
    cmds.append(cmd)
    # be sure to delete .pyo and .pyc files
    if os.path.splitext(invRecord.filePath)[1] == ".py":
        for ext in ["c", "o"]:
            cmd = DeleteUtilityCommand()
            cmd.setContext(invRecord.localCtx)
            cmd.setMyContextName(invRecord.localCtx.getContextName())
            cmd.setFilename(invRecord.filePath + ext)
            cmds.append(cmd)
    req.setCommands(cmds)
    
    serverResponse = Source.sendRequest(req)
    if not isinstance(serverResponse, SuccessfulExecution):
        message = ""
        if hasattr(serverResponse, "getMessage"):
            message = serverResponse.getMessage()
        raise RuntimeError(message)


if __name__ == '__main__':
    main()
    