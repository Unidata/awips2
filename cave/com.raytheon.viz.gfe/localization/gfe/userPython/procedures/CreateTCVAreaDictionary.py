
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CreateTCVAreaDictionary
#
# Author:
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]

try:  # See if this is the AWIPS I environment
    import AFPS
    AWIPS_ENVIRON = "AWIPS1"
except:  # Must be the AWIPS II environment
    AWIPS_ENVIRON = "AWIPS2"


import SmartScript
from LockingFile import File
from com.raytheon.uf.common.localization import PathManagerFactory
from com.raytheon.uf.common.localization import LocalizationContext
LocalizationType = LocalizationContext.LocalizationType
LocalizationLevel = LocalizationContext.LocalizationLevel
## For documentation on the available commands,
##   see the SmartScript Utility, which can be viewed from
##   the Edit Actions Dialog Utilities window

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        
    def execute(self, editArea, timeRange, varDict):
        self._siteID = self.getSiteID()
        
        if AWIPS_ENVIRON == "AWIPS1":
            import siteConfig
            self._gfeHome = siteConfig.GFESUITE_HOME
            self._gfeServer = siteConfig.GFESUITE_SERVER
            self._gfePort = siteConfig.GFESUITE_PORT
        
        self._tcvAreaDictionaryContents = \
"""
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TCV_AreaDictionary
#   TCV_AreaDictionary file
#
# Author: GFE Installation Script
# ----------------------------------------------------------------------------

# Here is an example TCVAreaDictionary for just a single zone and with comments
# to talk about the structure of the dictionary.
#
# TCV_AreaDictionary = {
#     # Zone
#     'FLZ173': {
#         # A list of location names.
#         'locationsAffected': [
#             "Miami Beach",
#             "Downtown Miami",
#         ],
#         
#         # Potential impacts statements can be overriden here; anything not
#         # overriden here will use the generic potential impacts statements
#         'potentialImpactsStatements': {
#             # Section name: "Wind", "Storm Surge", "Flooding Rain" or "Tornado"
#             "Wind": {
#                 # Threat level: "None", "Low", "Mod", "High" or "Extreme"
#                 "Extreme": [
#                     # Each string will be on its own line
#                     "Widespread power outages with some areas experiencing long-term outages",
#                     "Many bridges and access routes connecting barrier islands impassable",
#                     "Structural category to sturdy buildings with some having complete wall and roof failures",
#                     "Complete destruction of mobile homes",
#                     "Numerous roads impassable from large debris",
#                     
#                 ],
#             },
#         },
#         
#         # Additional information that will be displayed at the end of the segment
#         # The structure is a list containing strings and/or lists. Strings in the
#         # same list will be idented the same amount. Introducing a list, idents the
#         # text until it ends. For example:
#         #
#         # 'infoSection': [
#         #     "This will be at tab level 0",
#         #     [
#         #         "A new list was introduced so this is at tab level 1",
#         #         [
#         #             "Yet another list so this is tab level 2",
#         #             "Still at tab level 2 here",
#         #         ],
#         #         "We are back at tab level 1 because we ended the list",
#         #     ],
#         #     "We ended the other list and are back at tab level 0 now",
#         # ]
#         'infoSection': [
#             "LOCAL EVACUATION AND SHELTERING: MIAMI-DADE COUNTY EMERGENCY MANAGEMENT",
#             [
#                 "HTTP://WWW.MIAMIDADE.GOV/EMERGENCY/",
#             ],
#             "FAMILY EMERGENCY PLANS: FEDERAL EMERGENCY MANAGEMENT AGENCY",
#             [
#                 "HTTP://READY.GOV/",
#             ],
#             "LOCAL WEATHER CONDITIONS AND FORECASTS: NWS MIAMI FLORIDA",
#             [
#                 "HTTP://WWW.SRH.NOAA.GOV/MFL/",
#             ],
#         ],
#     },
# }

TCV_AreaDictionary = {
"""
        self._zoneSkeletonContents = {
            'locationsAffected' : [],
            'potentialImpactsStatements' : {},
            'infoSection' : [],
        }
        
        TCVAreaDictionary = {}
        try:
            if AWIPS_ENVIRON == "AWIPS1":
                import TCVAreaDictionary
                TCVAreaDictionary = TCVAreaDictionary.TCV_AreaDictionary
            else:
                filename = "gfe/userPython/textUtilities/regular/TCVAreaDictionary.py"
                fileContents = self._getFileContents(LocalizationType.CAVE_STATIC,
                                                     LocalizationLevel.SITE,
                                                     self._siteID,
                                                     filename)
                
                exec(fileContents)
                    
                TCVAreaDictionary = TCV_AreaDictionary
        except Exception:
            pass
        
        for zone in self._getZones():
            self._tcvAreaDictionaryContents += "    '" + zone + "': {\n"
            
            # Don't clobber existing dictionary entries
            if zone in TCVAreaDictionary:
                # Add new entries
                for key in self._zoneSkeletonContents:
                    if key not in TCVAreaDictionary[zone]:
                        TCVAreaDictionary[zone][key] = self._zoneSkeletonContents[key]
                
                # Remove entries that are no longer needed
                existingKeys = TCVAreaDictionary[zone].keys()
                for key in existingKeys:
                    if key not in self._zoneSkeletonContents:
                        TCVAreaDictionary[zone].pop(key)
                
                self._tcvAreaDictionaryContents += self._formatDictionary(TCVAreaDictionary[zone],
                                                                          tabLevel = 2)
            else:
                self._tcvAreaDictionaryContents += self._formatDictionary(self._zoneSkeletonContents,
                                                                          tabLevel = 2)

            self._tcvAreaDictionaryContents += "    },\n\n"

        self._tcvAreaDictionaryContents += "}\n"

        with open("/tmp/TCVAreaDictionary.TextUtility", "w") as file:
            file.write(self._tcvAreaDictionaryContents)

        self._installDictionary()

    def _installDictionary(self):
        from subprocess import call
        if AWIPS_ENVIRON == "AWIPS1":
            call([self._gfeHome + "/bin/ifpServerText",
                  "-h", self._gfeServer,
                  "-p", self._gfePort,
                  "-s",
                  "-u", "SITE",
                  "-n", "TCVAreaDictionary",
                  "-f", "/tmp/TCVAreaDictionary.TextUtility",
                  "-c", "TextUtility"])
        else:
            call(["/awips2/GFESuite/bin/ifpServerText",
                  "-o", self._siteID,
                  "-s",
                  "-u", "SITE",
                  "-n", "TCVAreaDictionary",
                  "-f", "/tmp/TCVAreaDictionary.TextUtility",
                  "-c", "TextUtility"])

    def _getZones(self):
        editAreasFilename = "gfe/combinations/EditAreas_PublicZones_" + \
                            self._siteID + ".py"
        zonesKey = "Zones_" + self._siteID
        
        editAreasFileContents = self._getFileContents(LocalizationType.CAVE_STATIC,
                                                      LocalizationLevel.CONFIGURED,
                                                      self._siteID,
                                                      editAreasFilename)
        exec(editAreasFileContents)
        
        # EASourceMap comes from the EditAreas file
        return EASourceMap[zonesKey]
    
    def _getFileContents(self, loctype, loclevel, locname, filename):
        pathManager = PathManagerFactory.getPathManager()
        context = pathManager.getContext(loctype, loclevel)
        context.setContextName(locname)
        localizationFile = pathManager.getLocalizationFile(context, filename)
        with File(localizationFile.getFile(), filename, 'r') as pythonFile:
            fileContents = pythonFile.read()
        
        return fileContents

    def _formatDictionary(self, dictionary, tabLevel, output=""):
        TAB = " " * 4
        
        for key in dictionary:
            output += TAB*tabLevel + repr(key) + ": "
            
            value = dictionary[key]
            if type(value) is dict:
                output += "{\n"
                output = self._formatDictionary(value, tabLevel+1, output)
                output += TAB*tabLevel + "},\n"
            elif type(value) is list:
                output += "[\n"
                output = self._formatList(value, tabLevel+1, output)
                output += TAB*tabLevel + "],\n"
            else:
                output += repr(value) + ",\n"
        
        return output
    
    def _formatList(self, theList, tabLevel, output=""):
        TAB = " " * 4
        
        for value in theList:
            if type(value) is dict:
                output += TAB*tabLevel + "{\n"
                output = self._formatDictionary(value, tabLevel+1, output)
                output += TAB*tabLevel + "},\n"
            elif type(value) is list:
                output += TAB*tabLevel + "[\n"
                output = self._formatList(value, tabLevel+1, output)
                output += TAB*tabLevel + "],\n"
            else:
                output += TAB*tabLevel + repr(value) + ",\n"
        
        return output
