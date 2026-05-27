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


import base64
import getopt
import json
import logging
import os
import sys
import urllib.parse
import urllib.request

import numpy

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetSingletonDbIdsRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GetSiteTimeZoneInfoRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import GridLocRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationContext
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationLevel
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationType
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from dynamicserialize.dstypes.com.raytheon.uf.common.site.requests import GetActiveSitesRequest
from ufpy import ThriftClient
from ufpy.localization import LocalizationUtil
from ufpy.localization.LocalizationFileManager import LocalizationFileVersionConflictException

#
# The ifpServerText program.  Stores, deletes, gets, and inventories text.
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/17/10                      dgilling       Initial Creation.
#    11/17/15         #5129        dgilling       Support changes to GetSiteTimeZoneInfoRequest.
#    07/17/17         #6285        randerso       Change to use new Roles/Permissions framework.
#    02/19/18         #6602        dgilling       Update for new text utility
#                                                 location.
#    06/06/22         #8871        dgilling       Fix "-m domain" option broken by Python
#                                                 3 updates.
#
#


logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s",
                    datefmt="%H:%M:%S",
                    # level=logging.DEBUG)
                    level=logging.INFO)
logger = logging.getLogger("ifpServerText")


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


class ifpServerText:

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
                         "ColorTable": ("COMMON_STATIC", "colormaps/GFE"),
                         "WeatherElementGroup": ("CAVE_STATIC", "gfe/weGroups"),
                         "SelectTR": ("COMMON_STATIC", "gfe/text/selecttr"),
                         "Tool": ("CAVE_STATIC", "gfe/userPython/smartTools"),
                         "Procedure": ("CAVE_STATIC", "gfe/userPython/procedures"),
                         "TextProduct": ("CAVE_STATIC", "gfe/userPython/textProducts"),
                         "TextUtility": ("CAVE_STATIC", "gfe/userPython/textUtilities"),
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
        self.__osUser = LocalizationUtil.getUser()

        self.__cmdLine()

        self.__thrift = ThriftClient.ThriftClient(
            self.__host, self.__port, "/services")

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

        # error
        else:
            raise Exception("Unknown class type " + self.__classType)

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
                    raise SyntaxWarning("Too many -c switches specified")
                options = ["Tool", "Procedure", "Utility", "TextUtility",
                           "TextProduct", "Config", "EditArea", "SelectTR",
                           "EditAreaGroup", "SampleSet", "WeatherElementGroup",
                           "ColorTable", "Combinations", "SmartTool", "ISCUtility"]
                if opt[1] not in options:
                    self.__usage()
                    s = "Error: Illegal class specified  " + opt[1]
                    raise SyntaxWarning(s)
                self.__classType = opt[1]

                if self.__classType == "SmartTool":
                    self.__classType = "Tool"  # maintain backwards compatible

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
                    raise SyntaxWarning("Error: More than one mode specified")
                self.__mode = 'SAVE'
            elif opt[0] == '-d':
                if self.__mode is not None:
                    self.__usage()
                    raise SyntaxWarning("Error: More than one mode specified")
                self.__mode = 'DELETE'
            elif opt[0] == '-g':
                if self.__mode is not None:
                    self.__usage()
                    raise SyntaxWarning("Error: More than one mode specified")
                self.__mode = 'GET'
            elif opt[0] == '-i':
                if self.__mode is not None:
                    self.__usage()
                    raise SyntaxWarning("Error: More than one mode specified")
                self.__mode = 'INVENTORY'

        # sanity checks, make sure all required switches are specified
        if self.__user is None:
            self.__user = self.__osUser

        if self.__host is None or self.__port is None:
            self.__usage()
            raise SyntaxWarning("Error: Missing host or port")
        self.__url = urllib.parse.urlunparse(["http",
                                              self.__host + ":" +
                                              str(self.__port),
                                              "/services/localization/{locType}/{locLevel}/{locPath}",
                                              "", "", ""])

        if self.__siteID is None and self.__metaInfo not in [
                'sitetimezone', 'site']:
            self.__usage()
            raise SyntaxWarning("Error: Missing siteID information")

        if self.__mode is None and self.__metaInfo is None:
            self.__usage()
            raise SyntaxWarning(
                "Error: Missing Mode -m, -s, -d, -i, or -n switch")

        if self.__mode == "INVENTORY" and self.__classType is None:
            self.__usage()
            raise SyntaxWarning("Error: INVENTORY mode requires -c switch")

        if self.__mode == "SAVE" and \
            (self.__name is None or self.__filename is None or
             self.__classType is None):
            self.__usage()
            raise SyntaxWarning(
                "Error: SAVE mode requires -n, -f, and -c switches")

        if self.__mode == "DELETE" and \
           self.__user not in [self.__osUser, "SITE"]:
            self.__usage()
            raise SyntaxWarning(
                "Error: DELETE mode can only be performed on SITE or " +
                self.__osUser +
                " owned files")

        if self.__mode == "SAVE" and \
           self.__user not in [self.__osUser, "SITE"]:
            self.__usage()
            raise SyntaxWarning(
                "Error: SAVE mode can only be performed on SITE or " +
                self.__osUser +
                " owned files")

        if self.__mode == "DELETE" and (self.__name is None or
                                        self.__classType is None):
            self.__usage()
            raise SyntaxWarning(
                "Error: DELETE mode requires -n and -c switches")

        if self.__mode == "GET" and (self.__name is None or
                                     self.__classType is None):
            self.__usage()
            raise SyntaxWarning("Error: GET mode requires -n and -c switches")

        if self.__metaInfo is not None:
            valid = ["site", "singleton", "sitetimezone", "domain"]
            if self.__metaInfo not in valid:
                self.__usage()
                raise SyntaxWarning("Error: unknown -m keyword found")
            elif self.__mode is not None:
                self.__usage()
                raise SyntaxWarning(
                    "Error: -m with -s, -d, -i, -n switches not compatible with each other")

    def __usage(self):
        print("""
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

""")

    def __buildInventory(self):
        localLevels = ["BASE", "CONFIGURED", "SITE", "USER"]
        if self.__user == "SITE":
            localLevels = localLevels[:-1]
        if self.__user == "CONFIGURED":
            localLevels = localLevels[:-2]
        elif self.__user == "BASE":
            localLevels = localLevels[0]

        inventory = {}
        for level in localLevels:
            localizationInfo = self.LOCALIZATION_DICT[self.__classType]
            ctx = LocalizationContext()
            ll = LocalizationLevel(level)
            locType = LocalizationType(localizationInfo[0])
            ctx.setLocalizationType(locType)
            ctx.setLocalizationLevel(ll)
            if level == "USER":
                ctx.setContextName(self.__user)
            elif level in ["SITE", "CONFIGURED"]:
                ctx.setContextName(self.__siteID)
            fakeRecord = textInventoryRecord("", localizationInfo[1], ctx)

            url = self.__buildUrl(fakeRecord)
            logger.debug("url: %s", url)

            request = urllib.request.Request(
                url, headers={'Accept': 'application/json'})
            try:
                with urllib.request.urlopen(request) as response:
                    jsonInventory = json.load(response)
                    for filename in jsonInventory:
                        shortName = os.path.splitext(filename)[0]
                        inventory[shortName] = textInventoryRecord(
                            filename, localizationInfo[1], ctx)
            except urllib.error.HTTPError as e:
                if e.code != 404:
                    raise

        return inventory

    def __saveText(self):
        # Saves a text file

        # read the file from disk
        with open(self.__filename, 'rb') as f:
            txt = f.read()

        # verify the class based on the contents of the file
        if self.__classType == "Tool":
            self.__verifyClass(txt, "class Tool")
        elif self.__classType == "Procedure":
            self.__verifyClass(txt, "class Procedure")
        elif self.__classType == "TextProduct":
            self.__verifyClass(txt, "class TextProduct")

        localizationInfo = self.LOCALIZATION_DICT[self.__classType]
        if self.__user != "SITE":
            levelName = "USER"
        else:
            levelName = self.__user
        ctx = LocalizationContext()
        ll = LocalizationLevel(levelName)
        locType = LocalizationType(localizationInfo[0])
        ctx.setLocalizationType(locType)
        ctx.setLocalizationLevel(ll)
        if self.__user != "SITE":
            ctx.setContextName(self.__user)
        else:
            ctx.setContextName(self.__siteID)
        record = textInventoryRecord(self.__name + self.EXTENSION_DICT[self.__classType],
                                     localizationInfo[1], ctx)
        url = self.__buildUrl(record)

        checksum = "NON_EXISTENT_CHECKSUM"
        try:
            with urllib.request.urlopen(url) as response:
                checksum = response.headers["Content-MD5"]
        except urllib.error.HTTPError as e:
            if e.code != 404:
                raise

        base64string = base64.b64encode(
            '{0}:{0}'.format(self.__osUser).encode())
        authString = "Basic {}".format(base64string.decode())
        headers = {'If-Match': checksum,
                   'Authorization': authString,
                   'Content-Type': 'application/octet-stream', }
        request = urllib.request.Request(
            url, data=txt, headers=headers, method='PUT')

        try:
            urllib.request.urlopen(request)
        except urllib.error.HTTPError as e:
            if e.code == 409:
                raise LocalizationFileVersionConflictException(
                    e.read().decode())
            else:
                raise RuntimeError(
                    "Could not send file to localization server: " +
                    str(e)) from e

        logger.info("Saved file %s under %s", self.__filename, self.__name)

    def __deleteText(self):
        # Deletes a text file
        if self.__name not in self.__db:
            s = "DELETE failed since " + self.__name + " not in inventory"
            raise KeyError(s)

        record = self.__db[self.__name]

        if self.__user != "SITE" and str(
                record.localCtx.getLocalizationLevel()) != "USER":
            raise RuntimeError("User can only delete user-level files.")

        if self.__user == "SITE" and str(
                record.localCtx.getLocalizationLevel()) != "SITE":
            raise RuntimeError("SITE can only delete site-level files.")

        url = self.__buildUrl(record)

        self.__deleteFile(url)
        if url.endswith(".py"):
            idx = url.rfind("/")
            filename = url[idx+1:]
            filenameParts = os.path.splitext(filename)
            newFilename = f"{filenameParts[0]}.{sys.implementation.cache_tag}{filenameParts[1]}c"
            newUrl = f"{url[:idx]}/__pycache__/{newFilename}"
            try:
                self.__deleteFile(newUrl)
            except urllib.error.HTTPError as e:
                if e.code != 404:
                    msg = "Could not delete file from localization server: " + \
                            str(e)
                    raise ValueError(msg) from e

        logger.info("Deleted %s", self.__name)

    def __deleteFile(self, url):
        checksum = 'NON_EXISTENT_CHECKSUM'
        with urllib.request.urlopen(url) as response:
            checksum = response.headers['Content-MD5']
        base64string = base64.b64encode(
            '{0}:{0}'.format(self.__osUser).encode())
        authString = "Basic {}".format(base64string.decode())

        headers = {"If-Match": checksum,
                   "Authorization": authString,
                   }
        request = urllib.request.Request(url, headers=headers, method='DELETE')
        try:
            urllib.request.urlopen(request)
        except urllib.error.HTTPError as e:
            if e.code == 409:
                raise LocalizationFileVersionConflictException(
                    e.read().decode())
            else:
                raise

    def __getText(self):
        # Gets the text file
        if self.__name not in self.__db:
            s = "GET failed since " + self.__name + " not in inventory"
            raise KeyError(s)

        invRecord = self.__db[self.__name]
        url = self.__buildUrl(invRecord)

        request = urllib.request.Request(url)
        response = urllib.request.urlopen(request)
        txt = response.read()

        if self.__filename is None:
            print(txt.decode())
        else:
            with open(self.__filename, 'wb', 0o644) as f:
                f.write(txt)
            logger.info(
                "Got %s --- written to %s",
                self.__name,
                self.__filename)

    def __inventoryText(self):
        print("%-40s" % "Name", "%-15s" % "Access", "Protect")
        print("%-40s" % "----", "%-15s" % "------", "-------")
        for (k, a) in sorted(self.__db.items()):
            if a.protected == True:
                pro = 'Read-Only'
            else:
                pro = 'Read-Write'
            print(
                "%-40s" %
                k,
                "%-15s" %
                a.localCtx.getLocalizationLevel(),
                pro)

    def __verifyClass(self, txt, searchString):
        for line in txt.decode().splitlines():
            if searchString in line:
                return 1
        raise ValueError(
            'Input file is not written in class format or contains improper type')

    def __saveSamples(self):
        # Saves a sample set  format: #points, lon/lat, lon/lat, .....
        self.__saveText()

    def __deleteSamples(self):
        # Deletes a sample set
        self.__deleteText()

    def __getSamples(self):
        # Gets a sample set
        self.__getText()

    def __inventorySamples(self):
        # Returns the inventory for sample sets
        self.__inventoryText()

    def __saveCT(self):
        # Saves a color table format: #points, rgb, rgb, rgb, rgb
        self.__saveText()

    def __deleteCT(self):
        # Deletes a color table
        self.__deleteText()

    def __getCT(self):
        # Gets a color table
        self.__getText()

    def __inventoryCT(self):
        # Returns the inventory for color tables
        self.__inventoryText()

    def __saveEA(self):
        # Saves an edit area
        self.__saveText()

    def __deleteEA(self):
        # Deletes a text file
        self.__deleteText()

    def __getEA(self):
        # Gets the edit area
        self.__getText()

    def __inventoryEA(self):
        # Returns the inventory for edit areas
        self.__inventoryText()

    def __saveEAGroup(self):
        # Saves a text file
        self.__saveText()

    def __deleteEAGroup(self):
        # Deletes an edit area group
        self.__deleteText()

    def __getEAGroup(self):
        # Gets the text file, decodes it
        self.__getText()

    def __inventoryEAGroup(self):
        # Returns the inventory
        self.__inventoryText()

    def __buildUrl(self, record):
        localizationType = str(record.localCtx.getLocalizationType()).lower()
        level = str(record.localCtx.getLocalizationLevel())
        if level in {"SITE", "CONFIGURED", "USER"}:
            locLevelString = '/'.join([level.lower(),
                                       record.localCtx.getContextName()])
        else:
            locLevelString = 'base'
        localizationPath = '/'.join([record.path, record.fileName])
        return self.__url.format(
            locType=localizationType, locLevel=locLevelString, locPath=localizationPath)

    def __metaInformation(self):
        # gets the meta information
        txt = ""
        wsId = WsId(progName="ifpServerText")
        if self.__metaInfo == "sitetimezone":
            request = GetActiveSitesRequest()
            try:
                serverResponse = self.__thrift.sendRequest(request)
            except Exception as e:
                raise RuntimeError(
                    "Could not retrieve meta information: " + str(e))

            siteIds = serverResponse
            request = GetSiteTimeZoneInfoRequest()
            request.setWorkstationID(wsId)
            request.setSiteID("")
            request.setRequestedSiteIDs(siteIds)
            try:
                serverResponse = self.__thrift.sendRequest(request)
            except Exception as e:
                raise RuntimeError(
                    "Could not retrieve meta information: " + str(e))

            if (serverResponse.isOkay()):
                tzInfo = serverResponse.getPayload()
                for (k, v) in tzInfo.items():
                    txt += k + ' ' + v + "\n"
            else:
                raise Exception(serverResponse.message())
        elif self.__metaInfo == "site":
            request = GetActiveSitesRequest()
            try:
                serverResponse = self.__thrift.sendRequest(request)
            except Exception as e:
                raise RuntimeError(
                    "Could not retrieve meta information: " + str(e))

            for site in serverResponse:
                txt = txt + site + "\n"

        elif self.__metaInfo == "singleton":
            request = GetSingletonDbIdsRequest()
            request.setWorkstationID(wsId)
            request.setSiteID(self.__siteID)
            try:
                serverResponse = self.__thrift.sendRequest(request)
            except Exception as e:
                raise RuntimeError(
                    "Could not retrieve meta information: " + str(e))

            if (serverResponse.isOkay()):
                singletons = serverResponse.getPayload()
                for s in singletons:
                    txt = txt + str(s) + "\n"
            else:
                raise Exception(serverResponse.message())
        elif self.__metaInfo == "domain":
            request = GridLocRequest()
            request.setWorkstationID(wsId)
            request.setSiteID(self.__siteID)
            try:
                serverResponse = self.__thrift.sendRequest(request)
            except Exception as e:
                raise RuntimeError(
                    "Could not retrieve meta information: " + str(e))

            if (serverResponse.isOkay()):
                domain = serverResponse.getPayload()
                proj = domain.getProjection()
                txt = f"ProjectionID: {proj.getProjectionID()}\n"
                txt += f"Grid Size: ({domain.getNx()}, {domain.getNy()})\n"
                txt += f"Grid Domain: ({domain.getOrigin()!r}, {domain.getExtent()!r})\n"

                for k in proj.keys():
                    txt += f"{k}: {getattr(proj, k)}\n"
            else:
                raise Exception(serverResponse.message())
        if self.__filename is None:
            print(txt)
        else:
            f = open(self.__filename, 'w', 0o644)
            f.write(txt)
            f.close()
            logger.info(
                "Got MetaInfo " +
                self.__metaInfo +
                " --- written to " +
                self.__filename)


def main():
    logger.info("ifpServerText Starting")

    try:
        obj = ifpServerText()
        obj.process()
    except Exception:
        logger.exception("Error encountered running ifpServerText:")
        sys.exit(1)

    logger.info("ifpServerText Finished")
    sys.exit(0)


if __name__ == "__main__":
    main()
