#!/awips2/python/bin/python2

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

from __future__ import print_function

import base64
import getpass
import json
import logging
import urllib2
import urlparse

from awips.localization.LocalizationFileManager import LocalizationFileVersionConflictException
from awips import ThriftClient
from awips import UsageArgumentParser

from dynamicserialize.dstypes.com.raytheon.uf.common.site.requests import GetActiveSitesRequest


#
# The moveGFEData program.  Moves data from one user to another.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/17/10                      dgilling       Initial Creation.
#    02/12/18         #7215        dgilling       Re-factor based on localization
#                                                 REST service.
#    02/19/18         #6602        dgilling       Update for new text utility 
#                                                 location.
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

LOCALIZATION_DICT = {"GFECONFIG": ("cave_static", "gfe/userPython/gfeConfig/"),
                     "EditArea": ("common_static", "gfe/editAreas/"),
                     "EditAreaGroup": ("common_static", "gfe/editAreaGroups/"),
                     "SampleSet": ("common_static", "gfe/sampleSets/"),
                     "ColorTable": ("common_static", "colormaps/GFE/"),
                     "BUNDLE": ("common_static", "gfe/weGroups/"),
                     "SELECTTR": ("common_static", "gfe/text/selecttr/"),
                     "Tool": ("cave_static", "gfe/userPython/smartTools/"),
                     "Procedure": ("cave_static", "gfe/userPython/procedures/"),
                     "TextProduct": ("cave_static", "gfe/userPython/textProducts/"),
                     "TextUtility": ("cave_static", "gfe/userPython/textUtilities/"),
                     "Utility": ("cave_static", "gfe/userPython/utilities/"),
                     "COMBODATA": ("cave_static", "gfe/comboData/"),
                     "COMBINATIONS": ("cave_static", "gfe/combinations/")}


logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    # level=logging.DEBUG)
                    level=logging.INFO)
logger = logging.getLogger("moveGFEData")



def validateArgs():
    """Collects program options from the command line. On error, returns the usage information."""

    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve", prog="moveGFEData")
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

    logger.info("MoveGFEData from [%s] to [%s]", options.sourceUser, options.destUser)

    if (options.sourceUser == "BASE" and not options.copyOnly) or \
        (options.destUser == "BASE"):
        parser.error("Operations not permitted with user \'BASE\'")

    if (options.sourceUser == "CONFIGURED" and not options.copyOnly) or \
        (options.destUser == "CONFIGURED"):
        parser.error("Operations not permitted with user \'CONFIGURED\'")
    
    if options.destHost is None:
        options.destHost = options.srcHost
    
    if options.destPort is None:
        options.destPort = options.srcPort
    
    if options.srcHost != options.destHost or options.srcPort != options.destPort:
        logging.info("Multiple servers: Source: %s:%d Dest: %s:%d", options.srcHost, options.srcPort, options.destHost, options.destPort)
    
    if options.destHost == options.srcHost and \
        options.destPort == options.srcPort and \
        options.destUser == options.sourceUser and \
        options.sourceUser != "SITE":
        parser.error("Source user must be different from destination user with same server/host")

    srcClient = ThriftClient.ThriftClient(options.srcHost, options.srcPort, "/services")
    request = GetActiveSitesRequest()
    try:
        response = srcClient.sendRequest(request)
    except:
        parser.error("Unable to connect to ifpServer " + options.srcHost + ":" + str(options.srcPort))
    else:
        srcActiveSites = response
    
    destClient = ThriftClient.ThriftClient(options.destHost, options.destPort, "/services")
    try:
        response = destClient.sendRequest(request)
    except:
        parser.error("Unable to connect to ifpServer " + options.destHost + ":" + str(options.destPort))
    else:
        destActiveSites = response
    
    if not options.srcSiteID and options.sourceUser in ['CONFIGURED', 'SITE']:
        if len(srcActiveSites) == 1:
            options.srcSiteID = srcActiveSites.pop()
        else:
            parser.error("Could not determine active site ID of ifpServer. Please provide -w flag.")

    if not options.destSiteID and options.sourceUser in ['CONFIGURED', 'SITE']:
        if len(destActiveSites) == 1:
            options.destSiteID = destActiveSites.pop()
        else:
            parser.error("Could not determine active site ID of ifpServer. Please provide -x flag.")

    if (options.sourceUser == "SITE" and not options.srcSiteID) or \
         (options.destUser == "SITE" and not options.destSiteID):
        parser.error("A site ID must be provided with user \'SITE\'")

    if options.destHost == options.srcHost and \
        options.destPort == options.srcPort and \
        options.destUser == options.sourceUser and \
        options.sourceUser == "SITE" and \
        options.srcSiteID == options.destSiteID:
        parser.error("Source siteID must be different from destination siteID with same server/host")

    options.srcUrlFormat = urlparse.urlunparse(["http", 
                                                options.srcHost + ":" + str(options.srcPort), 
                                                "/services/localization/{locType}/{locLevel}/{locPath}",
                                                "", "", ""])
    options.destUrlFormat = urlparse.urlunparse(["http", 
                                                options.destHost + ":" + str(options.destPort), 
                                                "/services/localization/{locType}/{locLevel}/{locPath}",
                                                "", "", ""])

    return options

def textShuffle(title, category):
    """Moves single categories of TEXT data.

        Args:
            title: type of file being moved
            category: short name for type of file being moved
    """
    print("\n")
    print("*****", title, "*****")

    while True:
        try:
            inventory = getTextInventory(category)
        except:
            logger.exception("Error getting inventory.")
            return
        
        if not inventory:
            print("No files available to", Action, "\n")
            return
        
        print("Current Inventory for User:", SourceUser)
        print("0.  Exit")
        print("-1. ALL Entries")
        sortedInv = sorted(inventory.keys())
        for i, key in enumerate(sortedInv):
            print(str(i + 1) + ".  " + key)

        print("Enter number to" + Action + "from " + SourceUser + " to " + DestUser)
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
        for filename in filesToMove:
            invTuple = LOCALIZATION_DICT[category]
            destUrl = buildURL(Dest, invTuple[0], DestUser, DestSite, invTuple[1] + filename)
            logger.debug("Source URL: " + inventory[filename])
            logger.debug("Dest URL: " + destUrl)
            if not moveText(filename, inventory[filename], destUrl, title):
                return

def moveText(filename, sourceUrl, destUrl, title):
    """Routine moves text data from source to destination, then deletes the source.
    
        Args:
            filename: filename of the file to be moved
            sourceUrl: complete URL of the source file
            destUrl: complete URL of the destination file
            title: type of file being moved

        Returns:
            True, if the move operation was successful. False, if not.
    """
    try:
        fileData, checksum = getTextData(sourceUrl)
        logger.debug("Retrieved [%s]", sourceUrl)
        logger.debug("moveText: File: [%s], Checksum: [%s]", filename, checksum)
        logger.debug("FileData:\n" + fileData[:2048])
    except:
        logger.exception("Error getting data: %s", sourceUrl)
        return False
    
    destUserForAuth = DestUser if DestUser not in ['BASE', 'CONFIGURED', 'SITE'] else getpass.getuser()
    try:
        saveTextData(fileData, destUrl, destUserForAuth)
    except:
        logger.exception("Error saving data: %s", destUrl)
        return False

    logger.info("Copied %s [%s] to [%s]", title, filename, DestUser)

    if not CopyOnly:
        srcUserForAuth = SourceUser if SourceUser not in ['BASE', 'CONFIGURED', 'SITE'] else getpass.getuser()
        try:
            deleteTextData(sourceUrl, srcUserForAuth, checksum)
        except:
            logger.exception("Error deleting data: %s", sourceUrl)
            return False
        logger.info("Deleted %s [%s] from [%s]", title, filename, SourceUser)
    
    return True

def getTextInventory(category):
    """Retrieves the localization inventory for a given GFE file type.
    
        Args:
            category: type of file we're retrieving inventory for

        Returns:
            A dict mapping filename to URL on source host.
    """
    invTuple = LOCALIZATION_DICT[category]
    sourceUrl = buildURL(Source, invTuple[0], SourceUser, SourceSite, invTuple[1])
    logger.debug("Listing Source URL: " + sourceUrl)

    request = urllib2.Request(sourceUrl)
    request.add_header("Accept", "application/json")
    response = urllib2.urlopen(request)

    jsonInventory = json.load(response)
    logger.debug("JSON data: " + repr(jsonInventory))
    inventoryMap = {}
    for filename in jsonInventory.keys():
        filename = filename.encode('ascii')
        inventoryMap[filename] = sourceUrl + filename

    return inventoryMap

def buildURL(formatString, localizationType, user, siteID, localizationPath):
    if user not in ["SITE", "CONFIGURED", "BASE"]:
        locLevelString = 'user/' + user
    elif user == "SITE":
        locLevelString = 'site/' + siteID
    elif user == "CONFIGURED":
        locLevelString = 'configured/' + siteID
    else:
        locLevelString = 'base'
    return formatString.format(locType=localizationType, locLevel=locLevelString, locPath=localizationPath)

def getTextData(url):
    """Retrieves a localization file from the specified URL.
    
        Args:
            url: URL for file to retrieve from localization service.

        Returns:
            A tuple containing the file contents and its MD5 checksum.
    """
    request = urllib2.Request(url)
    response = urllib2.urlopen(request)
    return (response.read(), response.headers["Content-MD5"])

def saveTextData(data, url, user):
    """Saves a file to the localization data store.
    
        Args:
            data: contents of the file
            url: URL for file to save using localization service.
            user: user name of the person performing the save, used for 
                  authorization purposes
    """
    try:
        request = urllib2.Request(url)
        response = urllib2.urlopen(request)
    except urllib2.HTTPError as e:
        if e.code == 404:
            checksum = "NON_EXISTENT_CHECKSUM"
        else:
            raise
    else:
        checksum = response.headers["Content-MD5"]

    logger.debug("saveTextData: File: [%s], Checksum: [%s]", url, checksum)

    request = urllib2.Request(url)
    request.get_method = lambda: "PUT"
    request.add_data(data)
    request.add_header("If-Match", checksum)

    base64string = base64.b64encode('%s:%s' % (user, user))
    authString = "Basic %s" % base64string
    request.add_header("Authorization", authString)

    try:
        response = urllib2.urlopen(request)
    except urllib2.HTTPError as e:
        if e.code == 409:
            raise LocalizationFileVersionConflictException(e.read())
        else:
            raise

def deleteTextData(url, user, checksum):
    """Deletes a localization file from the data store.
    
        Args:
            url: URL of file to delete from localization service.
            user: user name of the person performing the delete, used for 
                  authorization purposes
            checksum: checksum of file to delete, needed for version conflict
                      resolution in localization service
    """
    deleteFile(url, user, checksum)
    if url.endswith(".py"):
        # make sure .pyc and .pyo files get deleted when necessary
        for extension in ["c", "o"]:
            try:
                deleteFile(url + extension, user)
            except urllib2.HTTPError as e:
                if e.code != 404:
                    raise

def deleteFile(url, user, checksum=None):
    logger.debug("Attempting to delete [%s]", url)

    if not checksum:
        request = urllib2.Request(url)
        response = urllib2.urlopen(request)
        checksum = response.headers["Content-MD5"]

    logger.debug("deleteFile: File: [%s], Checksum: [%s]", url, checksum)
    
    request = urllib2.Request(url)
    request.get_method = lambda: "DELETE"
    request.add_header("If-Match", checksum)

    base64string = base64.b64encode('%s:%s' % (user, user))
    authString = "Basic %s" % base64string
    request.add_header("Authorization", authString)

    try:
        response = urllib2.urlopen(request)
    except urllib2.HTTPError as e:
        if e.code == 409:
            raise LocalizationFileVersionConflictException(e.read())
        else:
            raise


def main():
    """This contains the main routine for the program."""
    logger.info("Move GFE Data")

    options = validateArgs()
    logger.debug("Options: %s", options)

    global Source
    Source = options.srcUrlFormat
    global Dest
    Dest = options.destUrlFormat
    global SourceUser
    SourceUser = options.sourceUser
    global DestUser
    DestUser = options.destUser
    if options.srcSiteID:
        global SourceSite
        SourceSite = options.srcSiteID
    if options.destSiteID:
        global DestSite
        DestSite = options.destSiteID
    global CopyOnly
    CopyOnly = options.copyOnly
    if options.copyOnly:
        global Action
        Action = " copy "

    print("******************* MoveGFEData ******************")
    print("SourceUser:", options.sourceUser)
    print("DestinationUser:", options.destUser)

    print("")
    print("Enter type of data.  Choose one of the options:")
    print("0 - exit")

    textCategories = [("Color Tables", "ColorTable"),
                          ("Edit Areas (Reference Areas)", "EditArea"),
                          ("Sample Sets", "SampleSet"),
                          ("Weather Element Groups", "BUNDLE"),
                          ("Edit Area Groups", "EditAreaGroup"),
                          ("GFE Configuration Files", "GFECONFIG"),
                          ("Selection Time Ranges", "SELECTTR"),
                          ("Smart Tools", "Tool"),
                          ("Procedures", "Procedure"),
                          ("Utilities", "Utility"),
                          ("Text Utilities", "TextUtility"), 
                          ("Text Formatters", "TextProduct"), 
                          ("Zone Combiner Saved Combos and Colors", "COMBODATA"), 
                          ("Zone Combiner Combination Files", "COMBINATIONS")
                      ]
    for i, textInfo  in enumerate(textCategories):
        print((i + 1), "-", textInfo[0])

    category = int(raw_input())

    if category == 0:
        return 1

    # range is now inclusive of last element
    if category in range(1, len(textCategories) + 1):
        category -= 1
        logger.debug("Calling textShuffle(%s, %s)", textCategories[category][0], textCategories[category][1])
        textShuffle(textCategories[category][0], textCategories[category][1])
    
    return 0



if __name__ == '__main__':
    main()
    