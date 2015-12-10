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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Hazard_HLS
# Produces HLS product.
#
# Author: (Initial) Matt Davis/ARX
#         OB9.2  Tracy Hansen
#         OB9.3  Shannon White/Tracy Hansen/Matt Belk
#
#
# Version 3/7/11
# Version 8/22/11
# ----------------------------------------------------------------------------

import GenericHazards
import string, time, re, os, glob, types, copy, LogStream
import ModuleAccessor, SampleAnalysis
from math import *
import AbsTime, DatabaseID, StartupDialog
from com.raytheon.uf.viz.core import VizApp
from com.raytheon.uf.common.gfe.ifpclient import PyFPClient


DEG_TO_RAD = 0.017453292

from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData, ReferenceID
CoordinateType = ReferenceData.CoordinateType


import sys, types
sys.argv = [__name__]

class TextProduct(GenericHazards.TextProduct):
    Definition = copy.deepcopy(GenericHazards.TextProduct.Definition)

    Definition["displayName"]   = "None"
    Definition["outputFile"]    = "{prddir}/TEXT/Hazard_HLS.txt"
    Definition["database"]      =  "Official"  # Source database
    Definition["debug"]         =  1
    Definition["mapNameForCombinations"] = ["Zones_<site>"]
    #Definition["mapNameForCombinations"] = ["Zones_<site>","Marine_Zones_<site>"]
    Definition["defaultEditAreas"] = "EditAreas_PublicZones_<site>"
    Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display

    Definition["productName"]       = "Tropical Cyclone Local Statement"

    Definition["fullStationID" ]    = "<fullStationID>"
    Definition["wmoID" ]            = "<wmoID>"
    Definition["wfoCityState" ]     = "<wfoCityState>"
    Definition["pil" ]              = "<pil>"
    Definition["textdbPil" ]        = "<textdbPil>"
    Definition["awipsWANPil" ]      = "<awipsWANPil>"
    Definition["site"]              = "<site>"
    Definition["wfoCity"]           = "<wfoCity>"

    Definition["areaName"]          = ""  #optional area name for product
    Definition["areaDictionary"]    = "AreaDictionary"
    Definition["language"]          = "english"
    Definition["lineLength"]        = 66   #Maximum line length

    Definition["purgeTime"]         = 8 # Default Expiration in hours if
    Definition["includeCities"]     = 0 # Cities not included in area header
    Definition["cityDescriptor"]    = "Including the cities of"
    Definition["includeZoneNames"]  = 1 # Zone names will be included in the area header
    Definition["includeIssueTime"]  = 0 # Issue Time will be included in the area header
    Definition["easPhrase"] = \
        "URGENT - IMMEDIATE BROADCAST REQUESTED" # Optional EAS phrase to be include in product header
    Definition["callToAction"] = 1

    def __init__(self):
        GenericHazards.TextProduct.__init__(self)

    #####################################################################
    #####################################################################
    ### Organization of Formatter Code

    ###############################################################
    ###  MUST OVERRIDE ZONE DEFINITIONS !!!
    ###    _inlandAreas, _coastalAreas, _marineAreas, _cwa
    ###############################################################


    ###############################################################
    ### Optional Overrides, HLS GUI options and Configuration for
    ###    Situations and Scenarios
    #
    #   _areaDisplayType_land and _marine -- how zones are displayed in GUI's
    #   _GUI_labels -- wording for GUI titles
    #   _font_GUI_dict -- font for GUI titles
    #
    #   _overview_list -- list of Overview GUI frames (GUI 1)
    #   _overviewEndInstructions
    #   _overviewSections
    #
    #   _situation_list -- list of situations (each is a dictionary)
    #   _segmentSections -- list of segment sections (each is a dictionary)
    ###############################################################

    ###############################################################
    ###  Hazards and Additional Hazards
    ###   allowedHazards is used for segmentation e.g. HU.W, TR.W...
    ###   allowedHeadlines are additional hazards reported in overview
    ###           e.g. CF.W, FA.A, TO.A...
    ###############################################################

    ###############################################################
    #  CODE
    ###############################################################
    ###  High level flow of formatter
    ###   generateForecast, determineTimeRanges, sampleData,
    ###   preProcessProduct, makeProduct, postProcessProduct...
    ###############################################################

    ###############################################################
    ###  Helper methods -- Getting statistics from grids,
    ###       summarizing hazards found, determining inland/coastal/marine
    ###    _getSegmentInfo, _checkHazard, _orderSections,
    ###    _findInDictList, _accessDict
    ###    _analysisList_HLS
    ######################################################################
    ###  Previous Product Helper methods
    ######################################################################

    ######################################################################
    ###  OVERVIEW Sections
    ######################################################################
    ###  SEGMENT Sections
    ####################################################
    ###      Precautionary Preparedness Statement Dictionaries
    ######################################################################
    ###      Wind Situation/Scenario methods
    ######################################################################
    ###      Segment statements and thresholds e.g. Wind Statements
    #####################################################

    ###############################################################
    ### Example TCP product for automated testing
    ###############################################################

    #####################################################
    ### HLS GUI Processing
    #
    #####################################################################
    ##  TK GUI Classes
    #####################################################################
    #####################################################################

    ###############################################################
    ###  MUST OVERRIDE these methods!

    def _inlandAreas(self):
        return [
            #"FLZ052", "FLZ056", "FLZ057", "FLZ061", "FLZ043",
            ]

    def _coastalAreas(self):
        return [
            #"FLZ039", "FLZ042", "FLZ048", "FLZ049", "FLZ050", "FLZ051", "FLZ055", "FLZ060",
            #"FLZ062", "FLZ065",
            ]
    def _marineAreas(self):
        return [
            #"GMZ830", "GMZ850", "GMZ853", "GMZ856", "GMZ856", "GMZ870","GMZ873","GMZ876"
            ]

    def _cwa(self):
        return ""  #"TBW"

    def _cwa_descriptor(self):
        return "" # "central west Florida"

    def _maor_descriptor(self):
        return "" # "west central Florida waters and the Gulf of Mexico"

    def _cwa_maor_descriptor(self):
        return "" #"west Florida waters and the Gulf of Mexico"

    def _localReferencePoints(self):
        # Give the name and lat/lon for each local reference point
        return [
                #("Tampa Bay, FL", (28.01, -82.48)),
                #("Cape Coral, FL", (26.63, -82.00)),
                #("Lakeland, FL", (28.04, -81.95)),
                #("Sarasota, FL", (27.37, -82.55)),
                ]

    def _localReferencePoints_defaults(self):
        # Give a list of the local reference point names to be
        #  turned on by default
        return [] # ["Tampa Bay, FL", "Sarasota, FL"]

    #####################################################################################
    #####################################################################################
    ### Optional Overrides, HLS GUI options and Configuration for
    ###    Situations and Scenarios

    def _overviewFormat(self):
        # For overview headlines specify "listAreas" if you want specific
        #  locations listed.
        # Otherwise, specify "generic" for a general "All" or "Portions of"
        #  the CWA
        return {
            "land": "listAreas",
            "marine": "generic",
            }

    def _areaDisplayType_land(self):
        # You can set this to any key within the AreaDictionary.
        #   e.g. 'ugcName', 'altName', '
        # Also include the width of the display window
        #return ('ugcCode', 10)
        return ('ugcName', 15)

    def _referencePointLimit(self):
        # Give the number of reference points allowed to be chosen
        # Also give a label (e.g. "two") for the GUI
        return (2, "two")

    def _areaDisplayType_marine(self):
        # You can set this to any key within the AreaDictionary.
        #   e.g. 'ugcName', 'altName', '
        # Also include the width of the display window
        return ('ugcCode', 10)
        #return ('ugcName', 15)

    #################

    #  02/28/2011 (SW/MHB) - Modified the GUI behavior so that the ECs are limited to the
    #  appropriate options.
    #
    def _overview_list(self, argDict):
        allCON = argDict.get("allCON", False)
        forceAbbrev = argDict.get("forceAbbrev", False)
        allHUS = argDict.get("allHUS", False)
        watchEC = argDict.get("watchEC", False)
        allCAN = argDict.get("allCAN", False)
        step1Options = []
        step6Options = []
        step7Options = []
        if allCON:
            step1Options = [
                ("Use This GUI to Create Overview Text", "CreateFromGUI"),
                ("Use Previous Situation Overview Text", "UsePrev"),
                ]
        else:
            step1Options = [
                ("Use This GUI to Create Overview Text", "CreateFromGUI"),
                ]
        if forceAbbrev:
            step6Options = [
                ("Abbreviated Issuance (WWA First Issuance Everywhere at the same time)", "Abbreviated"),
                ]
            step7Options = [
                ("Shortly (for Abbreviated Issuances)", "Shortly"),
                ]
        elif allCAN:
            step6Options = [
                ("Post-Event (All hazards over everywhere)", "PostEvent"),
                ("Post-Tropical", "PostTropical"),
                ]
            step7Options = [
                ("Last Issuance", "LastIssuance"),
                ]
        elif allHUS:
            step6Options = [
                ("Non-Event (WWA Not Expected)", "NonEvent"),
                ("Pre-Event (WWA Possible Soon; Early Evacuations)", "PreEvent"),
                ("Post-Event (WWA Over, Statements Still Needed)", "PostEvent"),
                ]
            step7Options = [
                ("As Conditions Warrant", "Conditions"),
                ("Enter Approximate Time (below)", "Enter"),
                ]
        elif watchEC:
            step6Options = [
                ("Watches (No Warnings)", "Watch"),
                ]
            step7Options = [
                ("As Conditions Warrant", "Conditions"),
                ("Enter Approximate Time (below)", "Enter"),
                ]
        else:
            step6Options = [
                ("Warnings (With or Without Watches)", "Warning"),
                ("Conditions Occurring (With Warnings)", "Conditions"),
                ("Post-Event (WWA Ended and replaced by HU.S)", "PostEvent"),
                ]
            step7Options = [
                ("As Conditions Warrant", "Conditions"),
                ("Enter Approximate Time (below)", "Enter"),
                ]

        return [
            {
            "name": "OverviewEditMode",
            "label":"Step 1. Choose Overview Edit Mode",
            "options": step1Options,
            },
            {
            "name": "StormInfo",
            "label": "Step 2. Obtain Storm Type/Name/Info",
            "options": [
                "TCPAT1", "TCPAT2", "TCPAT3", "TCPAT4", "TCPAT5",
                "Enter PIL below (e.g. TCPEP1):",
                ],
            "entryField": "     ",
            },
            {
            "name": "Uncertainty",
            "label": "Step 3.  Declare Degree of Uncertainty",
            "options": [
                ("Smaller Degree", "Low"),
                ("Average Degree", "Average"),
                ("Larger Degree", "High"),
                ("N/A", "N/A"),
                ],
            "default": "N/A",
            },
            {
            "name":"LocalReferencePoints",
            "label": "Step 4. Locate Storm Relative to Local Reference Points (choose at most "\
            +self._referencePointLimit()[1]+")",
            "optionType": "check",
            "options": self._localReferencePoints(),
            "default": self._localReferencePoints_defaults(),
            },
            {
            "name": "MainHeadline",
            "label": "Step 5. Input Main Headline (required) ",
            "options": [
                ("Enter Unique Headline (below)", "Enter"),
                ("Use Previous HLS Headline", "UsePrev"),
                ("Use Latest TCP Headline", "UseTCP"),
                ],
            "entryField": "",
            },
            {
            "name":"EventContext",
            "label": "Step 6. Establish Event Context for CWA/MAOR (related to TC WWAs only)",
            "options": step6Options,
            },
            {
            "name": "NextUpdate",
            "label": "Step 7.  Indicate Next Update Time",
            "options": step7Options,
            "default": "Enter Approximate Time (below)",
            "entryField": "     e.g. 6 AM EDT",
            },
            ]

    def _overviewEndInstructions(self):
        return """Note: Please enter the necessary Overview (CWA/MAOR) information \n above before continuing to the Segmented (Zone Group) information. """

    def _overviewSections(self):
        # A list of dictionaries -- each dictionary represents a section.
        # The order of the list is the order the sections will appear in the GUI.
        # Fields in the dictionary can be:
        #  name -- name of section -- THIS should not be changed by the user since
        #           the code logic keys off this name
        #  label -- label for the section to appear in the GUI
        #  title -- text to appear in the product for the section
        #  endStr -- text to appear at the end of the section.
        #     NOTE: We are assuming the endStr is UNIQUE within the section and will
        #           not appear except at the end of the section!!
        return [
          {
              "name": "Overview_NewInformation",
              "label": "New Information",
              "title": ".NEW INFORMATION...\n",
          },
          {
              "name": "AreasAffected",
              "label": "Areas Affected",
              "title": ".AREAS AFFECTED...\n",
          },
          {
              "name":"WatchesWarnings",
              "label":"Watches/Warnings",
              "title":".Watches/Warnings...\n",
          },
          {
              "name":"StormInformation",
              "label":"Storm Information",
              "title": ".STORM INFORMATION...\n",
          },
          {
              "name":"SituationOverview",
              "label":"Situation Overview",
              "title": ".SITUATION OVERVIEW...\n"
          },
          {
              "name": "Overview_PrecautionaryPreparednessActions",
              "label": "Precautionary/Preparedness Actions",
              "title": ".Precautionary/Preparedness Actions...\nPrecautionary/Preparedness Actions...\n\n",
              "endStr": "\n&&"
          },
          {
              "name": "NextUpdate",
              "label": "Next Update",
              "title": ".NEXT UPDATE...\n",
          },
          ]

    def _situation_list(self):
        # List of dictionaries where each dictionary represents a situation
        # Entries in the dictionary can be:
        #   name -- name of situation -- THIS should not be changed by the user since
        #           the code logic keys off this name
        #   label -- label for the situation to appear in the GUI
        #   hazPairings -- list of action/phen/sig combo that need to exist to trigger an option
        #   ec -- list of a event contexts which will need to overlap with hazPairings
        #   scenarios -- list of possible scenarios for this
        #                situation and conditions in (label, name) form.
        #       MODIFIED 3/7 to add wrap-up scenarios for non-events to Non-Event and Post-Event
        #       MODIFIED 8/22 to add UPGTR.W to Abbreviated
        #       NOTE: You can change the scenario labels, but
        #       The scenario names should not be changed as
        #       they are tied directly to the code!
        #

        return  [
                {
                "name": "NonEvent",
                "label":"Non-Event",
##                "action": ["NEW", "CON", "EXA"],
##                "hazards": ["HU.S"],
                "hazPairings": ["NEWHU.S", "CONHU.S", "EXAHU.S", "CANHU.S"],
                "ec":        ["NonEvent", "PreEvent", "Watch", "Warning",
                              "Conditions", "PostTropical"],
                "scenarios": [
                      ("Non-Event Wind Threat", "ActiveNonEvent"),
                      ("Cancel Non-Event", "EndNonEvent"),
                    ],
                },
                {
                "name": "PreEvent",
                "label":"Pre-Event",
##                "action": ["NEW", "CON", "EXA"],
##                "hazards": ["HU.S"],
                "hazPairings": ["NEWHU.S", "CONHU.S", "EXAHU.S"],
                "ec":        ["PreEvent", "Watch", "Warning", "Conditions"],
                "scenarios": [
                      ("Advancing Wind Threat", "Advancing"),
                      ("Peripheral Wind Threat", "Peripheral"),
                      ("In Situ Developing Wind Threat", "InSitu"),
                      ],
                },
                {
                "name": "Abbreviated",
                "label": "Abbreviated",
##                "action": ["NEW", "EXA", "UPG", "CAN"],
##                "hazards": ["TR.A", "HU.A", "TR.W", "HU.W", "TY.A", "TY.W", "HU.S"],
                "hazPairings": ["NEWHU.S","EXAHU.S","NEWHU.A","EXAHU.A","NEWTY.A","EXATY.A",
                                "NEWTR.A","EXATR.A","NEWHU.W","EXAHU.W","NEWTY.W","EXATY.W",
                                "NEWTR.W","EXATR.W","CANHU.S","UPGHU.A","UPGTY.A","UPGTR.A",
                                "UPGTR.W","CANHU.W","CANTY.W","CANTR.W",
                                "CANTR.A","CANHU.A","CANTY.A"],
                "ec":        ["Abbreviated", "Watch", "Warning", "Conditions"],
                "scenarios": [
                      ("First Issuance", "FirstIssuance"),
                      ],
                },
                {
                "name": "Watch",
                "label": "Watch",
##                "action": ["NEW","CON", "UPG"],
##                "hazards": ["TR.A", "HU.A", "TY.A"],
                "hazPairings": ["CONHU.A","CONTY.A","CONTR.A"],
                "ec":        ["Watch", "Warning", "Conditions"],
                "scenarios": [
                      ("Advancing Wind Threat", "Advancing"),
                      ("Peripheral Wind Threat", "Peripheral"),
                      ("In Situ Developing Wind Threat", "InSitu"),
                     ],
                },
                {
                "name": "Warning",
                "label": "Warning",
##                "action": ["NEW","CON", "UPG", "CAN"],
##                "hazards": ["TR.W", "HU.W", "TY.W"],
                "hazPairings": ["CONHU.W", "CONTY.W", "CONTR.W", "CANHU.A", "CANTY.A"],
                "ec":        ["Warning", "Conditions"],
                "scenarios": [
                      ("Advancing Wind Threat", "Advancing"),
                      ("Peripheral Wind Threat", "Peripheral"),
                      ("In Situ Developing Wind Threat", "InSitu"),
                     ],
                },
                {
                "name": "Conditions",
                "label": "Conditions",
##                "action": ["NEW","CON", "UPG", "CAN"],
##                "hazards": ["TR.W", "HU.W", "TY.W"],
                "hazPairings": ["CONHU.W", "CONTY.W", "CONTR.W"],
                "ec":        ["Conditions"],
                "scenarios": [
                      ("Imminent Wind Threat", "Imminent"),
                      ("Ongoing Wind Threat", "Ongoing"),
                      ("Diminishing Wind Threat", "Diminishing"),
                      ],
                },
                {
                "name": "PostEvent",
                "label": "Post-Event",
##                "action": ["NEW", "EXA", "CON", "CAN"],
##                "hazards": ["TR.A", "HU.A", "TR.W", "HU.W", "TY.A", "TY.W", "HU.S"],
                "hazPairings": ["CANHU.W", "CANTY.W", "CANTR.W", "NEWHU.S", "CONHU.S",
                                "EXAHU.S","CANHU.S", "CANHU.A", "CANTY.A", "CANTR.A"],
                "ec":        ["Warning", "Conditions", "PostEvent"],
                "scenarios": [
                      ("Immediate Rescue/Recovery", "Immediate"),
                      ("Minor/No Impact", "NoImpact"),
                      ("Longer-term Rescue/Recovery","LongTerm"),
                      ],
                },
                {
                "name": "PostTropical",
                "label": "Post-Tropical",
##                "action": ["CAN"],
##                "hazards": ["TR.W", "HU.W"],
                "hazPairings": ["CANHU.W", "CANTR.W"],
                "ec":        ["PostTropical"],
                "scenarios": [
                       ("In Progress", "InProgress"),
                       ("Completed Transition", "Completed"),
                      ],
                },
                ]

    def importMethod(self, argDict, segment):
        #  This is a dummy method for importing text
        #  Enter code here to get text from a flat file or previous product if desired
        #  Then specify this in the "importMethod" entry for the desired segment
        #   in the _segmentSections set up
        return ""

    def _segmentSections(self):
        # A list of dictionaries -- each dictionary represents a section.
        # The order of the list is the order the sections will appear in the GUI.
        # Fields in the dictionary can be:
        #  name -- name of section -- THIS should not be changed by the user since
        #           the code logic keys off this name
        #  label -- label for the section to appear in the GUI
        #  inSegments -- "always", "optional" or situation-specific
        #  excludeFromSituations -- list of situations for which this section is NOT
        #           to be included
        #  includeFor -- can be list of areas for which to include this section as an option such that
        #                if ANY segment area meets the criteria, the section will be included as an
        #                option   OR
        #              a method to be called with 2 arguments:  (name, segmentAreas) which
        #                should return True/False for the section to be included as an option.
        #  defaultOn -- IF included as an option in the GUI, specify whether it should be defaulted ON
        #           Set to True/False OR specify a method to be called.
        #           The method will be called with 2 arguments:  (name, segmentAreas)
        #  usePrev -- if True, include a "Use Previous" check box on GUI
        #  importPIL -- specify a product PIL from which to get section information
        #              If present, a check box will appear on the GUI for importing
        #  importMethod -- optional method for importing information from an external source.
        #              Specify a method (see example above) for getting text from
        #              an external source.
        #              If present, a check box will appear on the GUI for importing
        #       NOTE: If both importPIL and importMethod are present, the importPIL will be used.
        #  orderBox -- if True, include a text box on GUI to enter an order number
        #  title -- This is the section title that will appear in the product.
        #           It can be a tuple consisting of:
        #                 (Title for Public zones, Title for Marine Zones)
        #           For example:
        #                 ("...Winds...\n","...Winds and Seas...\n")
        #
        # To ADD a new section, you must
        #  --Add a dictionary for the section in this list
        #  --Provide a method for producing the contents of the section.
        #    The name of the method must match the "name" field for the new
        #    section. (Look at the "Tornadoes" method for an example.)
        #    For example, if you add a section to this list:
        #
        #    {
        #      "name": "CoastalHazards",
        #      "label": "Coastal Hazards",
        #      "defaultOn": True,
        #      "includeFor": ["coastal"],
        #      "orderBox": True,
        #      "usePrev": True,
        #      "inSegments": "optional",
        #      "importMethod": None,
        #      "title": "...Coastal Hazards...\n",
        #     },

        #  Then you must have a method which returns a text string:
        #
        #    def CoastalHazards(self, title, argDict, segment, section, info):
        #
        return [
          {
              "name": "NewInformation",
              "label": "New Information",
              "defaultOn": False,
              "includeFor": self._allAreas(),
              "usePrev": False,
              "inSegments": "optional",
              "importMethod": None,
              "importPIL": None,
              "title": "...New Information...\n",
          },
          {
              "name": "PrecautionaryPreparednessActions",
              "label": "Precautionary/Preparedness Actions",
              "defaultOn": True,
              "includeFor": self._allAreas(),
              "usePrev": True,
              "inSegments": "optional",
              "importMethod": None,
              "importPIL": None,
              "title": "...Precautionary/preparedness actions...\nprecautionary/preparedness actions...\n\n",
              "endStr":"\n&&",
          },
          {
              "name": "Probability",
              "label": "Probability of Tropical Storm/Hurricane Conditions",
              "defaultOn": False,
              "includeFor": self._allAreas(),
              "inSegments": "optional",
              "excludeFromSituations": ["Conditions", "PostEvent", "PostTropical"],
              "importMethod": None,
              "importPIL": None,
              "title": "...Probability of tropical storm/hurricane conditions...\n",
          },
          {
              "name": "Wind",
              "label": "Winds and Seas",
              "defaultOn": True,
              "includeFor": self._allAreas(),
              "orderBox": True,
              "usePrev": True,
              "inSegments": "optional",
              "importMethod": None,
              "importPIL": None,
              "title": ("...Winds...\n","...Winds and Seas...\n"),
          },
          {
              "name": "StormSurgeTide",
              "label": "Storm Surge and Storm Tide",
              "defaultOn": True,
              "includeFor": self._coastalAreas(),
              "orderBox": True,
              "usePrev": True,
              "inSegments": "optional",
              "importMethod": None,
              "importPIL": None,
              "title":"...Storm surge and storm tide...\n" ,
          },
          {
              "name": "InlandFlooding",
              "label": "Inland Flooding",
              "defaultOn": True,
              "includeFor": self._inlandAreas()+self._coastalAreas(),
              "orderBox": True,
              "usePrev": True,
              "inSegments": "optional",
              "importMethod": None,
              "importPIL": None,
              "title": "...Inland flooding...\n",
          },
          {
              "name": "Tornadoes",
              "label": "Tornadoes and Waterspouts",
              "defaultOn": False,
              "includeFor": self._allAreas(),
              "orderBox": True,
              "usePrev": True,
              "inSegments": "optional",
              "importMethod": None,
              "importPIL": None,
              "title": ("...Tornadoes...\n","...Tornadoes and Waterspouts...\n")
          },
#          {
#              "name": "Marine",
#              "label": "Marine",
#              "defaultOn": True,
#              "includeFor": self._marineAreas(),
#              "orderBox": True,
#              "usePrev": True,
#              "inSegments": "optional",
#              "importMethod": None,
#              "importPIL": None,
#              "title":"...Marine...\n" ,
#          },
          ]

    def _defaultOn_StormSurgeTide(self, name, segmentAreas):
        # Default logic will set StormSurgeTide to ON if there are any coastal zones.
        # Local offices can add to the list of accepted areas (e.g. if some inland
        #    zones should have the Storm Surge Tide section defaulted on)
        #    OR change the logic as in any way desired.
        defaultOn = False
        for area in segmentAreas:
            if area in self._coastalAreas():
                defaultOn = True
        return defaultOn

    def _allAreas(self):
        return self._inlandAreas() + self._coastalAreas() + self._marineAreas()

    ########## GUI Configuration

    def _GUI_sizing_dict(self):
        # This contains values that adjust the GUI sizing.
        return {
            "GUI_height_limit": 800, # limit to GUI height in canvas pixels
            #"GUI_2_width": 820, # width for GUI 2
            "GUI_2_width": 1200, # width for GUI 2
            #"GUI_3_width": 970, # width for GUI 3
            "GUI_3_width": 1200, # width for GUI 3
            "zoneLines":     10, # number of zones to display without scrolling
            "charSize":       9,
            }

    def _GUI1_configDict(self):
        return {
            # Order and inclusion of GUI1 buttons
            # Each entry is (name of button in GUI code, desired label on GUI)
            "buttonList":[
                ("PreviousHLS","PreviousHLS"),
                ("Reset","Reset"),
                ("Next","Next"),
                ("Cancel","Cancel"),
                ],
            }

    def _GUI2_configDict(self):
        return {
            # Order and inclusion of GUI1 buttons
            # Each entry is (name of button in GUI code, desired label on GUI)
            "buttonList":[
                ("Next", "Next"),
                ("Cancel", "Cancel"),
                ],
            }

    def _GUI3_configDict(self):
        return {
            # Order and inclusion of GUI1 buttons
            # Each entry is (name of button in GUI code, desired label on GUI)
            "buttonList":[
                ("Ok", "Ok"),
                ("Cancel","Cancel"),
                 ],
            }

    def _GUI_labels(self):
        return {
            'GUI_2': "Step 8. Choose Situation Per Zone Group",
            'GUI_3a': "Step 9a. Choose Scenario Per Zone Group",
            'GUI_3b':"Step 9b. Identify & Order Sections",
            }

    def _font_GUI_dict(self):
        return {
            "headers": ("blue", ("Helvetica", 14, "bold")),
            "instructions": (None, ("Helvetica", 12, "italic")),
            }


    #####################################################################################


    ###############################################################
    ###  Hazards and Additional Hazards
    # allowedHazards is used for segmentation
    # allowedHeadlines are additional hazards reported in overview

    def allowedHazards(self):
        tropicalActions = ["NEW", "EXA", "EXB", "EXT", "UPG", "CAN", "CON", "EXP"]
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        marineActions = ["NEW", "EXA", "EXB", "EXT", "CON"]
        return [
            ('HU.A',allActions,'Hurricane'),
            ('HU.W',allActions,'Hurricane'),
            ('HU.S',allActions,'Hurricane'),
            ('TY.A',allActions,'Typhoon'),
            ('TY.W',allActions,'Typhoon'),
            ('TR.A',allActions,'Tropical'),
            ('TR.W',allActions,'Tropical'),
            ]

    def allowedHeadlines(self):
        allActions = ["NEW", "EXA", "EXB", "EXT", "CAN", "CON", "EXP"]
        return [
           ('FF.A', allActions, 'Flood'),        # FLASH FLOOD WATCH
           ('FA.A', allActions, 'Flood'),        # FLOOD WATCH
           ('CF.W', allActions, 'CoastalFlood'), # COASTAL FLOOD WARNING
           ('CF.Y', allActions, 'CoastalFlood'), # COASTAL FLOOD ADVISORY
           ('CF.A', allActions, 'CoastalFlood'), # COASTAL FLOOD WATCH
           ('SU.W', allActions, 'HighSurf'),     # HIGH SURF WARNING
           ('SU.Y', allActions, 'HighSurf'),     # HIGH SURF ADVISORY
           ('RP.S', allActions, 'Rip'),          # HIGH RIP CURRENT RISK
           ('TO.A', allActions, 'Convective'),   # TORNADO WATCH
           ('SR.W', allActions, 'Marine'),
           ('SR.A', allActions, 'Marine'),
           ('GL.W', allActions, 'Marine'),
           ('GL.A', allActions, 'Marine'),
           ('SC.Y', allActions, 'Marine'),
           ('SI.Y', allActions, 'Marine'),
           ('SW.Y', allActions, 'Marine'),
           ('RB.Y', allActions, 'Marine'),
           ('HF.W', allActions, 'Marine'),
           ('HF.A', allActions, 'Marine'),
           ]

    def _ignoreActions(self):
        # Ignore hazards with these action codes in the overview headlines
        # NOTE: the VTEC and segments will still include them correctly.
        return ['CAN', 'UPG']

    ###############################################################
    ### NOTES

    ##     HANDLING HLS SEGMENTATION
    ##
    ##     Problem:
    ##     The system is set up to sample hazards using the combinations file
    ##     edit areas i.e. do not sample zones not in the combinations
    ##     segmenting strictly according to the hazards i.e. all zones
    ##     in a combination  with the same hazard will be in a
    ##     segment.
    ##
    ##     The HLS formatter uses the combinations file differently.
    ##     Segmenting is  initially done according to the hazards as
    ##     above, but  IF a forecaster wants to further split the
    ##     segment, he/she  can set up a combination in the zone
    ##     combiner to do so.   This however, is optional and all
    ##     areas (land and marine)  in the WFO need to be sampled for
    ##     hazards regardless of  what's in the combinations file.
    ##
    ##     Solution:
    ##     The HLS code has several relatively independent pieces, and
    ##     each has to  sample the hazards correctly:
    ##
    ##     --GUI code:  Sampled by "_determineSegments" and stored in
    ##     argDict['hazards']
    ##
    ##     --Formatter Logic code:  Sampled by "_getProductInfo" and
    ##     stored in argDict['hazards'].    Note: we can't re-use the
    ##     hazards set by the GUI code  because between the time the
    ##     GUI is called and the formatter  code is invoked, the
    ##     TextFormatter infrastructure has  re-sampled the hazards
    ##     using the combinations file as  above and reset the
    ##     argDict['hazards']  entry.
    ##
    ##     --Override DiscretePhrases "getHazardList" to use the hazards
    ##     stored in argDict  rather than using the one automatically
    ##     generated by TextFormatter  infrastructure which uses the
    ##     combinations file as above.

    ###############################################################
    ###  High level flow of formatter

    def generateForecast(self, argDict):
        # Generate Text Phrases for a list of edit areas

        # Get variables
        error = self._getVariables(argDict)
        if error is not None:
            return error

        # Use previous for entire product
        try:
            if self._UsePrev:
                return self.getPreviousProduct(self._textdbPil)
        except:
            pass

        #print "\n\nvarDict", argDict["varDict"]
        segmentList = [areas for segNum, areas, situation, scenario,
                       sections, extraInfo in self._segments]
        #print "\n\nSegment Information", self._segments, "\n\n"
        if len(segmentList) == 0:
            return "No hazards to report"

        # Determine time ranges
        error = self._determineTimeRanges(argDict)
        if error is not None:
            return error

        # Sample the data
        self._sampleData(argDict)

        # Initialize the output string
        fcst = ""
        fcst = self._preProcessProduct(fcst, argDict)

        # Generate the product for each segment in the segmentList
        fraction = 0
        fractionOne = 1.0/float(len(segmentList))
        percent = 50.0
        self.setProgressPercentage(50)
        for segment in self._segments:
            self.progressMessage(fraction, percent, "Making Product for Segment")
            segmentNum, segmentAreas, situation, scenario, sections, extraInfo = segment
            fcst = self._preProcessArea(fcst, segmentAreas, self._expireTime, argDict)
            fcst  = self._makeProduct(fcst, segment, argDict)
            fcst = self._postProcessArea(fcst, segmentAreas, argDict)
            fraction = fractionOne
        fcst = self._postProcessProduct(fcst, argDict)
        return fcst

    ######### Time ranges

    def _resolution(self):
        return 3

    def _determineTimeRanges(self, argDict):
        # Set up the time range for 0-120 hours
        self._issueTime = AbsTime.AbsTime(argDict['creationTime'])

        # Create a time range from the issuanceHour out 120 hours
        #  First get the current local time
        localTime = time.localtime(argDict['creationTime'])
        year = localTime[0]
        month = localTime[1]
        day = localTime[2]
        hour = localTime[3]
        #  Now "truncate" to a 6-hourly boundary and compute startTime in local Time.
        hour =  int (int(hour/6) * 6)
        startTime = AbsTime.absTimeYMD(year, month, day, hour)
        # Finally, convert back to GMT
        localTime, shift = self.determineTimeShift()
        startTime = startTime - shift
        self._timeRange = self.makeTimeRange(startTime, startTime+120*3600)

        # Determine the time range list, making sure they are on hour boundaries
        #   w.r.t. midnight today according to the resolution
        subRanges = self.divideRange(self._timeRange, self._resolution())
        trList = []
        for tr in subRanges:
            # print tr
            trList.append((tr, "Label"))
        self._timeRangeList = trList

        self._ddhhmmTime = self.getCurrentTime(
            argDict, "%d%H%M", shiftToLocal=0, stripLeading=0)
        self._currentTime = argDict['creationTime']
        self._expireTime = self._issueTime + self._purgeTime*3600
        self._timeLabel = self.getCurrentTime(
            argDict, "%l%M %p %Z %a %b %e %Y", stripLeading=1)
        return None

    ######### Sample Data
    ##  Since the segments are determined by user input,
    ##  we need to determine combinations now (usually done automatically
    ##  by TextFormatter infrastructure.)

    def _sampleData(self, argDict):
        # Sample the data
        editAreas = self._makeSegmentEditAreas(argDict)
        areas = self._marineAreas()
        areas.append(self._cwa())
        cwa_maor = self._makeCombination(argDict, areas)
        editAreas.append(cwa_maor)
        self._cwaMaorArea, self._cwaMaorLabel = cwa_maor
        self._sampler = self.getSampler(argDict,
          (self._analysisList_HLS(), self._timeRangeList, editAreas))

    def _makeSegmentEditAreas(self, argDict):
        areasList = [segmentAreas
                    for segmentNum, segmentAreas, situation, scenario,
                     sections, extraInfo in self._segments]
        #print "areaList", areasList
        editAreas = []
        self._editAreaDict = {}
        for areas in areasList:
            if len(areas)>1:
                # Make a combination on the fly
                editArea, label = self._makeCombination(argDict, areas)
                # e.g. editArea, Combo1
                self._editAreaDict[tuple(areas)] = editArea
                editAreas.append((editArea, label))
            else:
                area = areas[0]
                self._editAreaDict[tuple(areas)] = area
                editAreas.append((area, area))
        return editAreas

    def _getComboNumber(self):
        try:
            self.__comboNumber = self.__comboNumber + 1
        except:
            self.__comboNumber = 1
        return self.__comboNumber

    def _makeCombination(self, argDict, areaNames):
        # Given a list of area names, return a combination edit area
        gridLoc = argDict["ifpClient"].getDBGridLocation()
        comboList = []
        for areaName in areaNames:
            newArea = self.getEditArea(areaName, argDict)
            if areaNames.index(areaName) == 0:
                comboNumber = self._getComboNumber()
                label = "Combo"+`comboNumber`
                refId = ReferenceID(label)
                #area = AFPS.ReferenceData(
                #    gridLoc, refId, newArea.polygons(),
                #    AFPS.ReferenceData.LATLON)
                #area.convertToAWIPS()
                area = ReferenceData(gridLoc, refId, newArea.getPolygons(CoordinateType.LATLON), CoordinateType.LATLON)
            comboList.append(newArea.getId().getName())
            area = self.unionAreas(label, area, newArea)
        return area, label

    ###### Generate headers and Overview sections

    def _preProcessProduct(self, fcst, argDict):

        self._prevHLS = self.getPreviousProduct(self._textdbPil)

        info = self._getProductInfo(argDict)
        self._getStormInfo(argDict, info)
        if self._stormTypeName.find("|*")>=0: sn = "Tropical Cyclone"
        else:                              sn = self._stormTypeName
        actualProductName = sn + " Local Statement"
        actualProductName = self.checkTestMode(argDict, actualProductName)

        # Product header
        if self._areaName != "":
            self._areaName = " for " + self._areaName
        issuedByString = self.getIssuedByString()
        productName = self.checkTestMode(argDict, actualProductName + self._areaName)

        if len(self._easPhrase) != 0:
            eas = self._easPhrase.upper() + '\n'
        else:
            eas = ''

        s = self._wmoID + " " + self._fullStationID + " " + \
               self._ddhhmmTime + "\n" + self._pil + "\n\n"
        fcst =  fcst + s.upper()
               
        s = eas + productName + "\n" +\
               "National Weather Service " + self._wfoCityState + \
               "\n" + issuedByString + self._timeLabel + "\n\n"
        fcst =  fcst + s

        # Main Headline
        mh = self._MainHeadline
        if mh == "Enter":
            hl = self._MainHeadline_entry
        elif mh == "UsePrev":
            hl = self._grabHeadline(self._prevHLS)
        elif mh == "UseTCP":
            try:  # If unnamed or downgraded, we won't have a TCP product
                hl = self._grabHeadline(self._TCP)
            except:
                hl = ""

        if hl == "":
            hl = self._frame("Enter headline here")
        hl = self._addEllipses(hl)
        fcst = fcst + hl + "\n\n" + self._overview(argDict, info)
        return fcst

    #  Modified 4/21/09 (MHB) - Fixed a problem with the construction of the
    #  overview when using previous text.  This will fix the problem with
    #  getting multiple copies of the first zone segment header.  The
    #  _grabSection method is not capable of recognizing the end of the
    #  "Next Update" overview section on its own.  Implemented the
    #  _grabOverview method (already defined in the baseline) to parse out the
    #  entire overview, with which _grabSection will work correctly.
    #
    #  Modified 12/24/10 (MHB) - Added capability to specify which sections
    #  of the overview can use previous text.  All other sections will be
    #  forced to update.  This involves a new call to the _grabSection method.

    def _overview(self, argDict, info):

        #  Establish previous HLS text for testing - if needed
        #if len(self._prevHLS.strip()) == 0:
        #    self._prevHLS = self._testPrevHLS()

        overview = ""
        if self._OverviewEditMode == "FormatFree":
            return self._frame("Enter Overview Information") + "\n\n"

        if self._OverviewEditMode == "UsePrev":
            usePrev = True

            #  Set aside to overview text, so we don't have to search the
            #  entire HLS product
            prevOverview = self._grabOverview(self._prevHLS)
        else:
            usePrev = False

##        print "prev = '%s'" % (prevOverview)

        #  Get the list of sections which must be present, in order
        sections = self._overviewSections()
        for sectionDict in sections:
            title = sectionDict.get("title", '')

             #  Start out with a blank text for this section
            sectionText = ""

            #  If we are requested to use previous text, and this is a section
            #  we can use it for
            if usePrev and \
               title.strip() not in self._noPrevTextOverviewSections():
#                print "Looking for previous '%s'" % (title)

                #  Get the previous text for this section
                sectionText = self._grabSection(prevOverview, title, True)

#                print usePrev, len(sectionText.strip())
#                print "'%s'" % (sectionText.strip())

            #  If we are not using the previous text, or we could not find
            #  the previous section text
            if not usePrev or len(sectionText.strip()) == 0:
                exec "sectionText = self." + sectionDict["name"] + "(title, sectionDict, info)"

            #  Ensure the grabbed text is wrapped to the correct product length
            sectionText = self.endline(sectionText, self._lineLength)

            #  Add this section text to the current overview
            overview = overview + sectionText + "\n\n"

        #  Return completed overview
        return overview

    ########## Produce Segment Sections

##    #  Modified 12/24/10 (MHB) - Added capability to specify which sections
##    #  of the segment can use previous text.  All other sections will be
##    #  forced to update.  This involves a new call to the _grabSection method.
##
    def _makeProduct(self, fcst, segment, argDict):
        argDict["language"] = self._language
        self._stormPrevHLS = False
        segmentNum, segmentAreas, situation, scenario, sections, extraInfo = segment
        info = self._getSegmentInfo([segment])
        self._setStats(info, argDict, segmentAreas, self._editAreaDict,
                      self._sampler, self._analysisList_HLS(), self._timeRangeList)

        #
        # This section generates the segment sections
        #
        hazardsC = argDict['hazards']
        listOfHazards = hazardsC.getHazardList(segmentAreas)
        if listOfHazards == []:
            return fcst

#        LogStream.logProblem("=== VARS ===",
#          "\nhazardsC=",hazardsC,
#          "\nlistOfHazards=",listOfHazards,
#          "\nsegmentAreas=",segmentAreas,
#          "\nphen.sig=",listOfHazards[0]['phensig'],
#        )
        LogStream.logEvent("=== VARS ===",
          "\nhazardsC=",hazardsC,
          "\nlistOfHazards=",listOfHazards,
          "\nsegmentAreas=",segmentAreas,
          "\nphen.sig=",listOfHazards[0]['phensig'],
        )

        prevHLS = self._getPrevText(listOfHazards)

        # This section generates the headline on the segment
        #
        # stuff argDict with the segmentAreas for DiscretePhrases
        argDict['segmentAreas'] = segmentAreas
        editArea = segmentAreas[0]
        # Stuff the headline for HU.S in extraInfo
        self._setUp_HU_S_Headline(extraInfo, prevHLS)
        argDict["extraInfo"] = extraInfo
        areaLabel = editArea

        fcst = fcst + self.generateProduct("Hazards", argDict, area=editArea,
                                           areaLabel=areaLabel, timeRange=self._timeRange)

        # self._segments =
        #   (segNum, areas, chosen situationName, chosen scenarioName,
        #         sections, extraInfo)
        #  For example:
##        (1, ['FLZ052'],'Warning',  'Advancing',
##        #  list of sections: name, order, usePrev, useImport
##        [
##        ('PrecautionaryPreparednessActions', None, 0, None),
##        ('Probability', '', 0, None),
##        ('Winds', '', 0, None),
##        ('StormSurgeTide', '', 0, None),
##        ('InlandFlooding', '', 0, None),
##        ('Tornadoes', '', 0, None),
##        ('Marine', '', 0, None)
##        ]
##        # Extra information for HU.S headlines
##        {'userHeadline_HU_S': 'Headline for HU.S',
##         'usePrev_HU_S_Headline':0},
##        ),

        sections = self._orderSections(sections)
        # iterate over the sections for this segment
        for section in sections:

            #  Initialize text for this section
            sectionText = ''

            #  Get info about this section
            sectionName, order, usePrev, useImport = section
            #print "section", sectionName, order, usePrev, useImport
            title = self._findInDictList(
                self._segmentSections(), "name", sectionName, "title")
            title = self._extractTitle(info, title)

            #  If we should use previous section text, and this is a section
            #  permitted to use previous text
            if usePrev and \
               title.strip() not in self._noPrevTextSegmentSections():

                #  If we will also be importing text for this section
                if useImport:
                    #  Frame the previous text to force forecaster to review it
                    sectionText = self._grabSection(prevHLS, title, True)
                else:

                    #  Just get the previous section text without framing codes
                    sectionText = self._grabSection(prevHLS, title)

            #  If we are not using previous text, or could not find it
            if not usePrev or len(sectionText.strip()) == 0:
                #  Make a blank shell section as a place-holder
                exec "sectionText = self." + sectionName + "(title, argDict, segment, section, info)"

            #  If we should also import text
            if useImport:
                importText = self._getImportText(argDict, segment, sectionName,
                                                 title)
                if importText.strip() != "":

                    print "\n\n" + "*"*80
                    print "sectionText = '%s'" % (sectionText)

                    #  Look to see if there are any section headers and framing
                    #  codes with dummy text in the existing section text
                    sectionMatch = re.search("(?is)^(\.{3}.+\.{3}.+?)\|\*" +
                                             " *(additional free|enter|add)",
                                             sectionText)


                    #  If we are not using the previous text, and the text
                    #  contains both a section header and dummy text in framing
                    #  codes
                    if sectionMatch is not None:

                        #  Keep the section header then add the imported text
                        sectionText = sectionMatch.group(1).strip() + "\n" + \
                                      importText

                    #  Otherwise, append imported text to the end of section
                    else:
                        sectionText = sectionText + "\n\n" + importText

            # Add endStr
            endStr = self._findInDictList(
                self._segmentSections(), "name", sectionName, "endStr")
            #print "\n***********endStr", endStr, sectionName
            if endStr is not None:
                # Remove first in case an endStr was added from previous or imported text
                # Note that we're assuming endStr's are unique within the section!!
                sectionText = sectionText.replace(endStr, "")
                # Now put the endStr at the end
                sectionText = sectionText + endStr

            #  Add this section to the segment
            sectionText = sectionText + "\n\n"
            fcst = fcst + sectionText

        #  Word wrap this segment
        fcst = self.endline(fcst, linelength=self._lineLength,
                            breakStr=[" ", "-", "..."])
        return fcst

    #  Added 12/24/10 (MHB) - Define a method to specify which overview
    #  sections are not permitted to use text from a previous HLS.  This means
    #  these sections will always have new text created/imported for it.  The
    #  section titles must match those as defined in the _overviewSections
    #  method (although any final "\n" can be ignored.
    def _noPrevTextOverviewSections(self):
        return [".NEW INFORMATION...", ".AREAS AFFECTED...",
                ".WATCHES/WARNINGS...", ".STORM INFORMATION...",
                ".NEXT UPDATE...",
                ".PRECAUTIONARY/PREPAREDNESS ACTIONS...\n" +
                "PRECAUTIONARY/PREPAREDNESS ACTIONS..."
               ]

    #  Added 12/24/10 (MHB) - Define a method to specify which segment
    #  sections are not permitted to use text from a previous HLS.  This means
    #  these sections will always have new text created/imported for it.  The
    #  section titles must match those as defined in the _segmentSections
    #  method (although any final "\n" can be ignored.
    def _noPrevTextSegmentSections(self):
        return ["...New Information...",
                "...Probability of tropical storm/hurricane conditions...",
               ]

    def _getImportText(self, argDict, segment, sectionName, title):
        importText = ""
        # Look for importPIL
        importPil = self._findInDictList(
            self._segmentSections(), "name", sectionName, "importPIL")
        if importPil is not None:
            importProduct = self.getPreviousProduct(importPil)
            importText = self._grabSection(importProduct, title)
            # Remove the title
            importText = importText.replace(title, "")
        else:  # Try importMethod
            importMethod = self._findInDictList(
                self._segmentSections(), "name", sectionName, "importMethod")
            if importMethod is not None:
                importText = importMethod(argDict, segment)
        if len(importText.strip()) > 0:
            #  Clean up and word-wrap imported text
            importText = self._cleanText(importText.strip())
            #  Add the imported text to this section
            importText = self._frame(importText) + "\n\n"
        return importText

    #  Modified 4/22/09 (MHB) - fixed logging options of search as it could
    #  lead to debugging confusion.  Only want log info after the loop has
    #  completed.
    def _getPrevText(self, listOfHazards):
        #=======================================================================
        #  Set aside the previous text for this segment - if we can

        prevHLS = ''

        #  Look through all hazards for this segment
        for hazIndex in range(len(listOfHazards)):

            #  See if this hazard has a previous text text key
            if listOfHazards[hazIndex].has_key('prevText'):

                #  Try to get the previous text from this hazard
                prevHLS = listOfHazards[hazIndex]['prevText']

                #  If there is actually something there
                if len(prevHLS) > 0:

                    #  No point in continuing - we found the previous text
                    break

        #  If there is still not something there
        if prevHLS == '':
#            LogStream.logProblem("No 'prevText' found for this segment")
            LogStream.logEvent("No 'prevText' found for this segment")
        else:
#            LogStream.logProblem("\nprevText=", prevHLS)
            LogStream.logEvent("\nprevText=", prevHLS)

        return prevHLS

    ############ Clean up

    def _postProcessProduct(self, fcst, argDict):
        fcst = self.endline(fcst, linelength=self._lineLength,
          breakStr=[" ", "-", "..."])
        fcst = fcst.replace("\n ","\n")
        fcst = fcst.replace("&&", "\n&&\n")

        # Prevent empty Call to Action Tags
        fcst = re.sub(r'\nPrecautionary/preparedness actions\.\.\.\s*&&\n', \
                      "", fcst)
        #
        # Clean up multiple line feeds
        #
        fixMultiLF = re.compile(r'(\n\n)\n*', re.DOTALL)
        fcst = fixMultiLF.sub(r'\1', fcst)
        # finish progress meter
        self.setProgressPercentage(100)
        self.progressMessage(0, 100, self._displayName + " Complete")
        return fcst

    ###############################################################
    ###  Helper  methods for getting information about the segments


    ## From the tropical formatters -- want to use the same configuration
    def moderated_dict(self, parmHisto, timeRange, componentName):
        """
           Modifed to lower the high end filter threshold from 20 MPH to
           15 MPH for Tropical.
        """
        # COMMENT: This dictionary defines the low and high limit at which
        # outliers will be removed when calculating moderated stats.
        # By convention the first value listed is the percentage
        # allowed for low values and second the percentage allowed
        # for high values. The thresholds chosen below gave best results
        # during testing with 2004 and 2005 tropical cyclones. This dict
        # is used with the moderatedMinMax analysis method specified in the
        # TropicalPeriod definitions specified further down for use with
        # tropical cyclones with wind parameters.

        # Get Baseline thresholds
        dict = SampleAnalysis.SampleAnalysis.moderated_dict(
            self, parmHisto, timeRange, componentName)

        #  Change thresholds for Wind, WindGust, WaveHeight and Swell
        # For entire area, we want the actual maximum
        if parmHisto.area().getId().getName() == self._cwaMaorLabel:
            dict["Wind"] = (0,0)
        else:
            dict["Wind"] = (0, 15)
        dict["WindGust"] = (0, 15)
        dict["WaveHeight"] = (0, 15)
        dict["Swell"] = (0, 15)
        dict["SurgeHtPlusTide"] = (0,2)
        dict["SurgeHtPlusTideWTopo"] = (0,2)
        return dict

    # This is a very simple way to round values -- if we need
    # something more sophisticated, we'll add it later.
    def _increment(self, element):
        dict = {
            "Wind": 5,
            "WindGust": 5,
            "SurgeHtPlusTide": 1,
            "SurgeHtPlusTideWTopo": 1,
            }
        return dict.get(element, 0)

    def _ktToMph(self, value, element):
        newVal = self.ktToMph(value)
        newVal = self.round(newVal, "Nearest", self._increment(element))
        return newVal

    class SegInfo:
        def __init__(self):
            pass

    def _getSegmentInfo(self, segments):
        # Used to handle all the information required for
        # both overview and segment sections
        # Determines
        #    kinds of areas included (land, marine, coastal, inland)
        #    hazards included
        #    and for segments, the wind and probability data from the grids
        #
        # All of this can then be passed in the "info" object
        # to the section methods for reporting
        #
        if type(segments) is not types.ListType:
            segments = [segments]
        allAreas = []
        for segment in segments:
            segmentNum, segmentAreas, situation, scenario, sections, extraInfo = segment
            allAreas = allAreas + segmentAreas

        info = self.SegInfo()

        #  Determine the types of areas included in segments
        # anyXX means that there is at least one area in the segment that is XX
        # allXX means that all the XX areas are in the segment
        info.anyInland, info.allInland, info.inlandAreas = self._checkAreas(
            allAreas, self._inlandAreas())
        info.anyCoastal, info.allCoastal, info.coastalAreas = self._checkAreas(
            allAreas, self._coastalAreas())
        info.anyMarine, info.allMarine, info.marineAreas = self._checkAreas(
            allAreas, self._marineAreas())
        info.anyLand = info.anyInland or info.anyCoastal
        info.allLand = info.allInland and info.allCoastal
        info.landAreas = info.inlandAreas + info.coastalAreas
        info.allAreas = info.inlandAreas + info.coastalAreas + info.marineAreas
        info.cwa= self._generalAreas(info.landAreas + info.marineAreas)
        info.cwaShort = info.cwa.rstrip(".\n")
        info.all_cwa_maor_areas = self._inlandAreas() + self._coastalAreas() + self._marineAreas()
        self._determineHazards(info, segments)
        return info

    def _determineHazards(self, info, segments):
        # Return a list of hazards from the given segments in the form:
        #    (key, landList, marineList, coastalList, inlandList)
        #  where key is (hdln, act, phen, sig) and the lists show which areas
        #    contain the hazard separated by category
        info.hazardHdlns = []
        hazAreaList = []
        for segment in segments:
            segmentNum, segmentAreas, situation, scenario, sections, extraInfo = segment
            hazardTable = self._argDict["hazards"]
            hazards = hazardTable.getHazardList(segmentAreas)
            for hazard in hazards:
                action = hazard['act']
                hazAreaList.append((hazard, segmentAreas))
        # Consolidate hazards (there could be multiple segments with the same phen/sig/act)
        hazardDict = {}
        hazardList = []
        for hazard, segmentAreas in hazAreaList:
            key = (hazard['hdln'], hazard['act'], hazard['phen'], hazard['sig'])
            if key not in hazardDict.keys():
                hazardDict[key] = segmentAreas
                hazardList.append(key)
            else:
                hazardDict[key] = hazardDict[key]+segmentAreas
        # Now we have areas that have the same headline and must split them into land/marine
        for key in hazardList:
            landAreas = []
            marineAreas = []
            coastalAreas = []
            inlandAreas = []
            hdln, act, phen, sig = key
            hazAreas = hazardDict[key]
            for area in hazAreas:
                if area in info.landAreas:  landAreas.append(area)
                if area in info.marineAreas:  marineAreas.append(area)
                if area in info.coastalAreas:  coastalAreas.append(area)
                if area in info.inlandAreas:  inlandAreas.append(area)
            info.hazardHdlns.append((
                key, landAreas, marineAreas, coastalAreas, inlandAreas))
        #print "\nhazardList", info.hazardHdlns

    def _getWindStats(self, argDict, sampler, analysisList, timeRangeList, editArea):
        statList = self.getStatList(sampler, analysisList, timeRangeList, editArea)
        maxWind = 0
        print "\nGetting CWA MAOR wind stats"
        for i in range(len(statList)):
            tr, label = timeRangeList[i]
            statDict = statList[i]
            wind = self._getStatValue(statDict, "Wind", "Max", self.VECTOR())
            print "  wind value", wind, tr
            if wind > maxWind:
                maxWind = wind
        print "  returning maxWind", maxWind
        return maxWind

    def _setStats(self, info, argDict, segmentAreas, editAreaDict,
                  sampler, analysisList, timeRangeList):
        # Get statistics for this segment and attach them to the info object
        editArea = editAreaDict[tuple(segmentAreas)]
        statList = self.getStatList(sampler, analysisList, timeRangeList, editArea)
        #print "statList", statList

        # Determine the MaxWind and MaxWindGust values and duration of maxWind
        info.maxWind = None
        info.minWind = None
        info.avgWind = None
        info.maxGust = None

        # Determine first grid that has Wind >= 34
        info.wind34Time = None

        # Determine min and max probabilities for 34, 64
        info.minProb34 = None
        info.maxProb34 = None
        info.minProb64 = None
        info.maxProb64 = None

        # Time of maximum pws34int and pws64int
        info.maxINTprob34 = None
        info.maxINTprob64 = None

        pws34Max = None
        pws64Max = None
        info.pwstrend = None

        # These need to be initialized to None so we'll know if NO grids are present
        info.surgeHtPlusTide = None
        info.surgeHtPlusTideWTopo = None

        # Pass 1:  Determine maximum values
        for i in range(len(statList)):
            tr, label = timeRangeList[i]
            statDict = statList[i]
            #print "\ntr", tr

            stats = self.getStats(statDict, "prob34")
            if stats is not None:
                min34, max34 = stats
                if info.maxProb34 is None: info.maxProb34 = max34
                if info.minProb34 is None: info.minProb34 = min34
                if min34 < info.minProb34:
                    info.minProb34 = min34
                if max34 > info.maxProb34:
                    info.maxProb34 = max34

            stats = self.getStats(statDict, "prob64")
            if stats is not None:
                min64, max64 = stats
                if info.minProb64 is None: info.minProb64 = min64
                if info.maxProb64 is None: info.maxProb64 = max64
                if min64 < info.minProb64:
                    info.minProb64 = min64
                if max64 > info.maxProb64:
                    info.maxProb64 = max64

            pws34int = self._getStatValue(statDict, "pws34int", "Max")
            if pws34int is not None:
                if pws34int > pws34Max:
                    info.maxINTprob34 = tr
                    pws34Max = pws34int
            pws64int = self._getStatValue(statDict, "pws64int", "Max")
            if pws64int is not None:
                if pws64int > pws64Max:
                    info.maxINTprob64 = tr
                    pws64Max = pws64int

            # Get wind and gust values --
            wind = self._getStatValue(statDict, "Wind", "MinMax", self.VECTOR())
            if wind is not None:
                minWind, maxWind = wind
                #print "minWind, maxWind", minWind, maxWind
                if info.maxWind is None:
                    info.minWind = minWind
                    info.maxWind = maxWind
                else:  # Check for maxWind increasing or decreasing
                    if maxWind > info.maxWind:
                        info.maxWind = maxWind
                    if minWind < info.minWind:
                        info.minWind = minWind
                if info.wind34Time is None and info.maxWind >= 34:
                    info.wind34Time = tr
            windGust = self._getStatValue(statDict, "WindGust", "Max")
            if windGust is not None:
                if info.maxGust is None:
                    info.maxGust = windGust
                if windGust > info.maxGust:
                    info.maxGust = windGust

            info.surgeHtPlusTide = self._pickupMaxStats(
                     statDict, info.surgeHtPlusTide, "SurgeHtPlusTide")
            info.surgeHtPlusTideWTopo = self._pickupMaxStats(
                     statDict, info.surgeHtPlusTideWTopo, "SurgeHtPlusTideWTopo")

        # Round to increment
        if info.maxWind is not None and info.minWind is not None:
            info.avgWind = (info.maxWind + info.minWind)/2.0
            info.maxWind = int(self.round(info.maxWind, "Nearest", self._increment("Wind")))
            info.minWind = int(self.round(info.minWind, "Nearest", self._increment("Wind")))
            info.avgWind = self.round(info.avgWind, "Nearest", self._increment("Wind"))
        if info.maxGust is not None:
            info.maxGust = int(self.round(info.maxGust, "Nearest", self._increment("WindGust")))
        if info.surgeHtPlusTide is not None:
            info.surgeHtPlusTide = int(self.round(
                info.surgeHtPlusTide,"Nearest", self._increment("SurgeHtPlusTide")))
            if info.surgeHtPlusTide > 10:
                info.deltaSurge = info.surgeHtPlusTide - 4
            elif info.surgeHtPlusTide > 6:
                info.deltaSurge = info.surgeHtPlusTide - 3
            elif info.surgeHtPlusTide > 2:
                info.deltaSurge = info.surgeHtPlusTide - 2
        if info.surgeHtPlusTideWTopo is not None:
            info.surgeHtPlusTideWTopo = int(self.round(
                info.surgeHtPlusTideWTopo,"Nearest", self._increment("SurgeHtPlusTideWTopo")))

        print "\n\nStats for segment", segmentAreas
        print "   maxWind, maxWindGust", info.maxWind, info.maxGust
        print "   minWind, avgWind", info.minWind, info.avgWind
        print
        print "  34 Info"
        print "   min, max prob34", info.minProb34, info.maxProb34
        print "   maxINTprob34", info.maxINTprob34
        print "  64 Info"
        print "   min, max, prob64", info.minProb64, info.maxProb64
        print "   maxINTprob64", info.maxINTprob64
        print "  SurgeHtPlusTide", info.surgeHtPlusTide
        print "  SurgeHtPlusTideWTopo", info.surgeHtPlusTideWTopo

        # Make additional passes to determine durations
        #  These are values for which we need to calculate durations
        #  In addition, we'll calculate for the maxWind value
        windDurValues = [34, 50, 64]
        info.windDur = {}
        print "Durations"
        for durVal in windDurValues + [info.maxWind]:
            info.windDur[durVal] = self._determineDuration(durVal, statList, timeRangeList)
            print "   ", durVal, info.windDur[durVal]
        print

    def _pickupMaxStats(self, statDict, curValue, element):
        # Given an element and curValue, pick up the stats from the statDict
        # If stats are greater than the curValue, replace and return curValue
        # Assumes that curValue is initialized to None
        stats = self.getStats(statDict, element)
        if stats is not None:
            if curValue is None:
                curValue = 0
            if stats > curValue:
                curValue = stats
        return curValue

    def _determineDuration(self, durValue, statList, timeRangeList):
        # Determine maxWind, wind34, and wind64 durations, and end time of 34, 64
        #  This will be the first time range that a value goes from above 34, 64 to below
        #print "\n Determine Duration for", durValue
        hit = False
        beg = None
        end = None
        for i in range(len(statList)):
            tr, label = timeRangeList[i]
            statDict = statList[i]
            # Get wind stats
            wind = self._getStatValue(statDict, "Wind", "MinMax", self.VECTOR())
            if wind is not None:
                minWind, maxWind = wind
                #print "    minWind, maxWind", minWind, maxWind
                maxWind = int(self.round(maxWind, "Nearest", self._increment("Wind")))
                #print "    new maxWind", maxWind
                if end is None:
                    if hit and maxWind < durValue:
                        end = tr
                    elif  not  hit and maxWind >= durValue:
                        hit = True
                        beg = tr
        #print "beg, end", beg, end, "\n"
        if beg is not None:
            if end is None:
                end = tr
            newTR = self.makeTimeRange(beg.startTime(), end.startTime())
            return newTR
        return None

    def _getStatValue(self, statDict, element,method=None, dataType=None):
        stats = statDict.get(element, None)
        if stats is None: return None
        if type(stats) is types.ListType:
            stats = stats[0]
            stats, tr = stats
        if dataType==self.VECTOR():
            stats, dir = stats
        return self.getValue(stats, method)

    def _determineDescriptor(self, info, areas):
        # Return the descriptor for the type of areas given plus
        #   the comparison list to determine if the areas cover all or portions
        #   of the descriptor
        # If all marine, return maor_descriptor and marineAreas
        # If all land, return cwa_descriptor and landAreas
        # else return cwa_maor_descriptor and all land and marine areas
        any, all, areas =  self._checkAreas(areas, self._marineAreas())
        if all:
            return self._maor_descriptor(), self._marineAreas()
        landAreas = self._inlandAreas()+self._coastalAreas()
        any, all, areas =  self._checkAreas(areas, landAreas)
        if all:
            return self._cwa_descriptor(), landAreas
        return self._cwa_maor_descriptor(), info.all_cwa_maor_areas

    def _checkAreas(self, segmentAreas, checkAreas):
        # all is True if ALL checkAreas are in the segmentAreas
        #   e.g. all land areas are in the segment areas
        all = True
        any = False
        areas = []
        for area in segmentAreas:
            if area in checkAreas:
                any = True
                areas.append(area)
            else:
                all = False
        return any, all, areas

    def _areaType(self, areas):
        inland = False
        coastal = False
        marine = False
        for area in areas:
            if area in self._inlandAreas():
                inland = True
                break
        for area in areas:
            if area in self._coastalAreas():
                coastal=True
                break
        for area in areas:
            if area in self._marineAreas():
                marine=True
                break
        return inland, coastal, marine

    def _checkHazard(self, hazardHdlns, phenSigList, checkAreaTypes=None,
                    checkAreas=None, returnList=False, mode="any", includeCAN=False):
        # Given a list of hazards in the form
        #    (key, landList, marineList, coastalList, inlandList)
        #  where key is (hdln, act, phen, sig) and the lists show which areas
        #    contain the hazard
        # If mode == "any":
        #     Check to see if any of the given phenSigList = [(phen, sig), (phen, sig)]
        #          are found
        # If mode == "all":
        #     Check to see if all of the given phenSigList are found
        # IF checkAreaTypes is given, then check against that particular area type(s) i.e.
        #   "land", "marine", etc.
        # IF checkAreas is given, only return areas that are in that list
        # IF returnList=True, returns a list of (key, areas) that meet the criteria
        # IF includeCAN is True then CAN hazards will be included as well.
        #     Otherwise, they are ignored.
        #
        # E.g. hdlnList = self._checkHazard(hazardHdlns, [("FA","W")], returnList=True)
#        print "phenSigList is ", phenSigList
        chosen = []
        for key, landList, marineList, coastalList, inlandList in hazardHdlns:
#            print "what is mode?", mode
            hazAreas = landList+marineList
            hazValue = (key, hazAreas)
#            print "hazValue is ", hazValue
            hdln, act, phen, sig = key
            if not includeCAN and act == "CAN":
                continue
            for checkPhen, checkSig in phenSigList:
#                print "checkPhen is ", checkPhen
#                print "checkSig is ", checkSig
                if phen == checkPhen and sig == checkSig:
                    if checkAreaTypes is not None:
                        # Check for land, marine, etc.
                        for checkAreaType in checkAreaTypes:
                            exec "testList = " + checkAreaType + "List"
#                            print "testList is", testList
                            if testList != []:
                                chosen.append(hazValue)
#                                print "chosen is ", chosen
                    elif checkAreas is not None:
                        acceptedAreas=[]
                        for hazArea in hazAreas:
                            if hazArea in checkAreas:
                                acceptedAreas.append(hazArea)
                        if acceptedAreas!=[]:
                            chosen.append((key, acceptedAreas))
                    else:
                        chosen.append(hazValue)
                    if not returnList and chosen!=[]: break
        if not returnList:
            return chosen!=[]
        return chosen

    #### Handling of HU.S headlines per segment
    def _setUp_HU_S_Headline(self, extraInfo, prevHLS):
        # Stuff the headline for HU.S in extraInfo
        usePrev = extraInfo.get("usePrev_HU_S_Headline")
        if usePrev:
            headline = self._grabHeadline(prevHLS)
        else:
            headline = extraInfo.get("userHeadline_HU_S")
        if headline == "":
            headline = self._frame("Enter headline here")
        extraInfo["headline_HU_S"] = headline

    # OVERRIDE from DiscretePhrases -- Must use argDict hazards set up
    # by the HLS rather than the one generated by the Text Formatter
    # infrastructure which uses the combinations file differently than
    # we want for the HLS.  See "Handling HLS segmentation" note
    # above.

    # Returns a formatted string announcing the hazards that are valid with
    # timing phrases
    def getHazardString(self, tree, node, fcstArea):
        if len(fcstArea) <= 0:
            return ""
        #hazardTable = self._hazards.getHazardList(fcstArea)
        argDict = tree.get("argDict")
        hazardList = argDict["hazards"].getHazardList(fcstArea)
        returnStr = ""
        issuanceTime = self._issueTime.unixTime()
        returnStr = self.makeHeadlinePhrases(tree, node, hazardList,
                                             issuanceTime)
        #Test mode?
        returnStr = self.headlinePhraseTESTcheck(tree.get("argDict"),
          returnStr)
        return returnStr

    # OVERRIDE from DiscretePhrases
    # USES the HU_S headline stuffed into argDict["extraInfo"]
    #   Makes multiple headlines based on the hazards list and returns
    #   the lot.
    def makeHeadlinePhrases(self, tree, node, hazardList, issuanceTime,
      testMode=0):
        returnStr = ""
        # make a deepcopy since we plan to mess with it.
        hList = copy.deepcopy(hazardList)

        # sort headlines in appropriate order
        if len(hList):
            if hList[0]['pil'] in ['CWF','NSH','OFF','GLF']:
                hList.sort(self.marineSortHazardAlg)
            else:
                hList.sort(self.regularSortHazardAlg)
        numHdlns = len(hList)

        while len(hList) > 0:
            hazard = hList[0]

            #  Check for HU.S headline
            #  Only report cancelled HU.S if it is a singleton hazard
            hazStr = None
            if hazard['hdln'] == "" and hazard['phen']=="HU" and hazard['sig']=="S":
                if hazard['act'] != "CAN" or numHdlns == 1:
                    argDict= tree.get("argDict")
                    extraInfo = argDict.get("extraInfo", None)
                    if extraInfo is not None:
                        hdln = extraInfo.get("headline_HU_S", None)
                        if hdln is not None:
                            hazard['hdln'] = hdln
                    hazStr = hazard['hdln']
                    # Strip ellipses since they will be added later
                    hazStr = hazStr.rstrip("...").lstrip("...")

            # Can't make phrases with hazards with no 'hdln' entry
            if hazard['hdln'] == "":
                hList.remove(hazard)
                continue

            phenSig = hazard['phen'] + "." + hazard['sig']
            actionCodeList = self.getAllowedActionCodes(phenSig)

            # if the action is not in the actionCodeList, skip it
            if hazard['sig'] != "":   # it's not locally defined
                if not hazard['act'] in actionCodeList:
                    print "...Ignoring action code:", hazard['act'], \
                      hazard['hdln']
                    hList.remove(hazard)
                    continue

            # get the headline phrase
            if hazStr is None:
                hazStr = self.makeStandardPhrase(hazard, issuanceTime)
            if len(hazStr):
                # Call user hook
                localStr = self.addSpace(self.hazard_hook(
                  tree, node, hazard['phen'], hazard['sig'], hazard['act'],
                  hazard['startTime'], hazard['endTime']), "leading")
                returnStr = returnStr + "..." + hazStr + localStr + "...\n"

            # always remove the main hazard from the list
            hList.remove(hazard)

        return returnStr
    #### END Handling of HU.S headlines per segment

    def _frame(self, text):
        return "|* " + text + " *| "

    def _orderSections(self, sections):
        # We are assuming that IF someone orders one section, then
        #     ALL must be ordered, order numbers are correct, and proper integers 1-(xxx)
        #     AND the ordered sections follow the sections that cannot be ordered
        # Otherwise, we revert to default ordering.

        # If order is None, it means that the section should be in the order it appears in the list
        # If order is a number, then order it

        #print "\nOrdering sections"
        # Gather the sections that need to be ordered
        ordered = []
        unordered = []
        orderError = False
        for section in sections:
            sectionName, order, usePrev, useImport = section
            #print sectionName
            if order is not None:
                try:
                    index = int(order)-1
                except:
                    orderError=True
                    # We will punt ordering
                    break
                ordered.append((section, index))
            else:
                unordered.append(section)

        #print "ordered", ordered
        #print "unordered", unordered

        # Order the sections correctly
        #print "orderError", orderError
        if not orderError:
            orderedSections = []
            for i in range(len(ordered)): orderedSections.append(None)
            error = False
            for section, index in ordered:
                try:
                    if orderedSections[index] != None:
                        error = True
                except: error = True
                if error: break
                orderedSections[index] = section
            #  Add them back to the end of the unordered sections
            if error: newSections = sections
            else: newSections = unordered + orderedSections
        else:
            newSections = sections

        #print "\nreordered", newSections

        # Now add in required sections that did not show up in the GUI
        # Assume that the required sections will appear before the optional ones.
        requiredSections = []
        for sectionEntry in self._segmentSections():
            if sectionEntry["inSegments"] == "always":
                sectionName = sectionEntry.get('name')
                requiredSections.append((sectionName, None, None, None))
        finalSections = requiredSections + newSections
        #print "\nfinalSections", finalSections, "\n"
        return finalSections

#    def headlineRegExpr(self):
#        # modify this to change how "previous HLS" catches the 2nd headline
#        # the first headline will be defined by the required headline
#        headlineRegEx = r'^((?:.\.\.[^\n]+?\.\.\.)|(?:\$\$))$'
#        return headlineRegEx

    def _findInDictList(self, dictList, identifier, value, field):
        for dictionary in dictList:
            if dictionary[identifier] == value:
                return dictionary.get(field, None)
        return None

    def _accessDict(self, dictionary, keys):
        value = dictionary
        for key in keys: value = value[key]
        return value

    def _addEllipses(self, string):
        # Add beginning and ending ellipses to non-null string
        #  (if not there already)
        if string != "":
            string = string.rstrip("...")
            string = string.lstrip("...")
            string = "..." + string + "..."
        return string

    def _analysisList_HLS(self):
        # 120 hours = time period of prob34, 64 grids
        # prob34 and prob64 are 120 hour grids, so just sample one value (maximum) for the
        #    whole time period
        # MaxINTProb34 = time period when the 6 hourly pws34int is maximum
        # MaxINTProb64 = time period when the 6 hourly pws64int is maximum
        # XXX : if <=32: 5, 32-42: 10, 50: 20? (same as Tropical formatters)
        #

        # Sample over 120 hours beginning at current time (OR time of prob34/prob64)
        return [
            ("Wind", self.vectorModeratedMinMax, [6]),
            ("WindGust", self.moderatedMinMax, [6]),
            ("prob34", self.minMax), # 120 hour value
            ("prob64", self.minMax), # 120 hour value
            ("pws34int", self.maximum, [6]),
            ("pws64int", self.maximum, [6]),
            ("SurgeHtPlusTide", self.moderatedMax),
            ("SurgeHtPlusTideWTopo", self.moderatedMax),
            ]

    #####################################################################################
    #####################################################################################
    ###  Previous Product Helper methods

    def _grabStormInfo(self, tcp):
        #  Get the storm information from the selected TCP
        #  return a dictionary
        #  Initialize a dictionary to hold the information we want
        dict = {"StormType" : "|* fill in storm type here *|",
                "StormName" : "|* fill in storm name here *|",
                "StormTime" : "|* Enter storm time *| ",
                "StormLat": "",
                "StormLon": "",
                "StormReference": "",
                "StormIntensity": "",
                "StormMotion": "",
                "StormInfo": "",
                "StormCenter": "",
               }
        #=======================================================================
        #  If we got the latest public advisory

        if tcp is not None and len(tcp) > 0:

            #===================================================================
            #  Try to determine the storm type and name automatically

            #  Updated version to handle WFO GUM advisories.  This pattern will
            #  handle multiple word names (including certain special characters)
            #  This is for the NHC format.
            mndSearch = re.search("(?im)^.*?(HURRICANE|(SUB|POST.?)?TROPICAL " +
                                  "(STORM|DEPRESSION)|(SUPER )?TYPHOON|" +
                                  "REMNANTS OF) ([A-Z0-9\-\(\) ]+?)" +
                                  "(SPECIAL |INTERMEDIATE )?ADVISORY", tcp)

            #  Display some debug info - if flag is set
            self.debug_print("mndSearch = '%s'" % (mndSearch))

            #  If we found the storm type and name in the MND header
            if mndSearch is not None:

                #  Pick off the storm type and name
                dict["StormType"] = mndSearch.group(1).strip()
                dict["StormName"] = mndSearch.group(5).strip()

            ####################################################################
            ####################################################################
            #  12/15/2010 (MHB) - we should not need this anymore, but will
            #  leave it for the 2011 season as a fail-safe.

            #  Look for the HPC format instead
            else:

                mndSearch = re.search("(?im)^PUBLIC ADVISORY.+?FOR REMNANTS " +
                                      "OF ([A-Z0-9\-\(\) ]+)", tcp)

                #  If we found the storm type and name in the MND header
                if mndSearch is not None:

                    #  Pick off the storm type and name
                    dict["StormType"] = "Remnants of"
                    dict["StormName"] = mndSearch.group(1).strip()

            #  end possible removal - 12/15/2010 (MHB)
            ####################################################################
            ####################################################################

            #===================================================================
            #  Clean up the product for easier parsing

            tcp = self._cleanText(tcp)

            #===================================================================
            #  Now try to grab the latest storm information

            #  Look for the new NHC format first
            summarySearch = re.search("(?is)SUMMARY OF (.+?)\.{3}.+?" +
                                      "LOCATION\.{3}(.+?[NS]) +(.+?[WE]).+?" +
                                      "(ABOUT .+?)MAXIMUM SUSTAINED WIND.+?" +
                                      "(\d+ MPH).+?PRESENT MOVEMENT\.{3}" +
                                      "(.+?)\.{3}", tcp)

            #--------------------------------------------------------------------
            #  If we found the NHC summary section

            if summarySearch is not None:

                #  Set aside some information we'll need later on
                dict["StormTime"] = summarySearch.group(1).strip()
                dict["StormLat"] = summarySearch.group(2).strip()
                dict["StormLon"] = summarySearch.group(3).strip()
                dict["StormReference"] = summarySearch.group(4).strip()
                dict["StormIntensity"] = summarySearch.group(5).strip()
                dict["StormMotion"] = summarySearch.group(6).strip()

                #================================================================
                #  Use the remaining summary groups to contruct a paragraph
                #  similar to the "old" TCP format, and save that for later use

                #  Start the paragraph with the advisory time
                dict["StormCenter"] = "AT %s...THE CENTER OF " % \
                                      (dict["StormTime"])

                #  Now add some phrasing to maintain proper grammar, if needed
                if dict["StormType"] == "Remnants of":
                     dict["StormCenter"] = "%s THE" % (dict["StormCenter"])

                #  Now add the storm type and storm name
                dict["StormCenter"] = "%s %s %s " % (dict["StormCenter"],
                                                     dict["StormType"],
                                                     dict["StormName"])

                #  Now add the storm position
                dict["StormCenter"] = \
                    "%s WAS LOCATED AT LATITUDE %s...LONGITUDE %s." % \
                    (dict["StormCenter"], dict["StormLat"], dict["StormLon"])

                #----------------------------------------------------------------
                #  Now add the primary NHC geographic reference

                #  Get all the NHC references - starting with the word 'about'
                #  after the first one
                referenceIndex = dict["StormReference"][4:].find('about')

                #  Assume we only have one NHC reference point by default
                nhcReference = dict["StormReference"]

##                print "referenceIndex = ", referenceIndex

                #  If we have more than one NHC reference point
                if referenceIndex != -1:

                    #  Adjust this index to account for the first 'about'
                    referenceIndex += 4

                    #  Only keep the first NHC reference location
                    nhcReference = dict["StormReference"][:referenceIndex]

                #  Convert any abbreviated bearings to full words
                nhcReference = nhcReference.replace(' N ', ' north ')
                nhcReference = nhcReference.replace(' NNE ', ' north-northeast ')
                nhcReference = nhcReference.replace(' NE ', ' northeast ')
                nhcReference = nhcReference.replace(' ENE ', ' east-northeast ')
                nhcReference = nhcReference.replace(' E ', ' east ')
                nhcReference = nhcReference.replace(' ESE ', ' east-southeast ')
                nhcReference = nhcReference.replace(' SE ', ' southeast ')
                nhcReference = nhcReference.replace(' SSE ', ' south-southeast ')
                nhcReference = nhcReference.replace(' S ', ' south ')
                nhcReference = nhcReference.replace(' SSW ', ' south-southwest ')
                nhcReference = nhcReference.replace(' SW ', ' southwest ')
                nhcReference = nhcReference.replace(' WSW ', ' west-southwest ')
                nhcReference = nhcReference.replace(' W ', ' west ')
                nhcReference = nhcReference.replace(' WNW ', ' west-northwest ')
                nhcReference = nhcReference.replace(' NW ', ' northwest ')
                nhcReference = nhcReference.replace(' NNW ', ' north-northwest ')

                #  Add only first one to the summary paragraph for brevity
                dict["StormCenter"] = "%s this was %s. " % \
                                      (dict["StormCenter"],
                                       self._removeKM(nhcReference.strip()))

                #----------------------------------------------------------------
                #  Add the maximum sustained wind speed phrase

                dict["StormCenter"] = "%s maximum sustained winds were %s." % \
                                       (dict["StormCenter"],
                                        self._removeKM(dict["StormIntensity"]))

                #----------------------------------------------------------------
                #  Now add the storm motion

                dict["StormCenter"] = "%s the storm motion was %s." % \
                                       (dict["StormCenter"],
                                        self._removeKM(dict["StormMotion"]))

            ####################################################################
            ####################################################################
            #  12/15/2010 (MHB) - we should not need this anymore, but will
            #  leave it for the 2011 season as a fail-safe.
            #--------------------------------------------------------------------
            #  Search the product for the legacy storm info section - in case
            #  the new NHC style was not found

            stormInfoSearch = \
                re.search('(?is)(AT +(\d+ +[AP]M [AECMPH][DS]T)' +
                          '\.{3}\d+ *(Z|UTC)\.{3}THE (CENTER|REMNANTS|EYE) .+)',
                          tcp)

            #  Display some debug info - if flag is set
            self.debug_print("storminfoSearch = '%s'" % (stormInfoSearch))
##            print stormInfoSearch.groups()

            #  If we found the storm info section of the product
            if stormInfoSearch is not None:
#                for group in stormInfoSearch.groups():
#                    print '\t' + '-'*50
#                    print "%s\n" % (group)

                #  Clean this section up a bit.  Keep each paragraph separate
                #  by a single <CR>, but remove all others as well as extra
                #  spaces.  Then store this text in the TCP dictionary
                dict["StormInfo"] = stormInfoSearch.group(1).strip()

                #  Set aside the first paragraph of the storm info since it
                #  contains the TPC-provided reference point - if we haven't
                #  already found this information
                if len(dict["StormCenter"].strip()) == 0:
                    dict["StormCenter"] = dict["StormInfo"].split('\n')[0]

                #  If we have not already found the advisory time - get it from
                #  the legacy format
                if dict["StormTime"] == "|* Enter storm time *| ":
                    dict["StormTime"] = stormInfoSearch.group(2).strip()

                #  Set aside the first paragraph of the storm info since it
                #  contains the TPC-provided reference point - if we haven't
                #  already found this information
                if len(dict["StormCenter"].strip()) == 0:
                    dict["StormCenter"] = dict["StormInfo"].split('\n')[0]

            #===================================================================
            #  Now try to grab the repeated storm information summary

            repeatInfo = re.search("(?is)(\.{3}SUMMARY.+?\.)\n *\n",
                                   tcp)
            #  If we cannot find the summary, try to find a "repeating" section
            if repeatInfo is None:
                repeatInfo = re.search("(?is)(REPEATING.+?\.)\n *\n", tcp)
##            print repeatInfo

            #  If we found the repeated storm information summary
            if repeatInfo is not None:

                #  Clean up this paragraph
                summary = repeatInfo.group(1).strip()

                #===============================================================
                #  Now try to grab the latest storm location - if we need it

                if dict["StormLat"] == "" or dict["StormLon"] == "":

                    #  Search the product for the storm location section
                    locationSearch = \
                        re.search('(?is).+LOCATION.*?(\d+\.\d+ *N).+?' +
                                  '(\d+\.\d+ *[EW])', summary)

                    #  Display some debug info - if flag is set
                    self.debug_print("locationSearch = '%s'" % (locationSearch))
##                    print locationSearch.groups()

                    #  If we found the storm location section of the product
                    if locationSearch is not None:

                        #  Pick off the storm latitude and longitude
                        dict["StormLat"] = locationSearch.group(1).strip()
                        dict["StormLon"] = locationSearch.group(2).strip()

                #===============================================================
                #  Now try to grab the latest storm intensity - if we need it

                if dict["StormIntensity"] == "":

                    #  Search the product for the storm intensity section
                    intensitySearch = \
                        re.search('(?i).+MAXIMUM SUST.+?(\d+ *MPH)', summary)

                    #  Display some debug info - if flag is set
                    self.debug_print("intensitySearch = '%s'" %
                                     (intensitySearch))

                    #  If we found the storm intensity section of the product
                    if intensitySearch is not None:

                        #  Pick off the storm intensity
                        dict["StormIntensity"] = intensitySearch.group(1).strip()

                #===============================================================
                #  Now try to grab the latest storm motion - if we need it

                if dict["StormMotion"] == "":

                    #  Search the product for the storm motion section
                    motionSearch = re.search('(?i).+MOVEMENT\.{3}(.+?\d+ MPH)',
                                             summary)
                    if motionSearch is None:
                        motionSearch = re.search('(?i).+MOVEMENT(.+?\d+.+?)\.',
                                                 summary)

                    #  Display some debug info - if flag is set
                    self.debug_print("motionSearch = '%s'" % (motionSearch))

                    #  If we found the storm motion section of the product
                    if motionSearch is not None:

                        #  Pick off the storm motion
                        motion = motionSearch.group(1).strip()

                        #  Fix the motion (i.e no '...')
                        dict["StormMotion"] = re.sub('(?i)\.{3}', ' the ',
                                                     motion)

            #  end possible removal - 12/15/2010 (MHB)
            ####################################################################
            ####################################################################

        #========================================================================
        #  Display final decoded information from TCP

##        print "\n\n" + "*" *80
##        print "Final TCP Info...\n"
##        print 'dict["StormType"] = ', dict["StormType"]
##        print 'dict["StormName"] = ', dict["StormName"]
##        print 'dict["StormTime"] = ', dict["StormTime"]
##        print 'dict["StormLat"] = ', dict["StormLat"]
##        print 'dict["StormLon"] = ', dict["StormLon"]
##        print 'dict["StormReference"] = ', dict["StormReference"]
##        print 'dict["StormIntensity"] = ', dict["StormIntensity"]
##        print 'dict["StormMotion"] = ', dict["StormMotion"]
##        print 'dict["StormInfo"] = ', dict["StormInfo"]
##        print 'dict["StormCenter"] = ', dict["StormCenter"]

        #  Return the dictionary will all the information we found in the TCP
        return dict

    def _cleanText(self, text=''):
        #  Cleans up text for easier string searches, but retains paragraphs

        #  Replace all single <CR> characters with a space
        text = re.sub("\n(?! *\n)", " ", text)

        #  Ensure all text is only single-spaced
        text = re.sub(" +", " ", text)

        #  Remove all spaces at the start of a new paragraph
        text = re.sub("(?m)^ +", "", text)

        #  Do not allow any spaces after an ellipsis
        text = re.sub("\.{3} +", "...", text)

        #  Finally, ensure the paragraphs are put back
        text = re.sub("\n", "\n\n", text)

        #  Return the cleaned-up text
        return text

    def _grabHeadline(self, text=''):
        #  Get first headline found in text and return it as a string

        #  Fixed pattern to grab headline (MHB 04/08/2009)
        #  See if there is a headline in this text
        headlineSearch = re.findall("(?ism)^(\.{3}.+?\.{3}) *\n", text)

        self.debug_print("headlineSearch = %s" % (headlineSearch))

        #  If we found a headline
        if len(headlineSearch) > 0:

            #  Return the first cleaned-up headline string we found
            return self._cleanText(headlineSearch[0])

        #  Otherwise, return an indicator there is no headline in this text
        else:
            return ''       #  Changed to an null string instead of None
                            #  (MHB  04/08/2009)

    #  Modified 4/22/09 (MHB) - fixed pattern to grab entire synopsis.
    #  Previous version only seemed to grab the last section of the overview.
    def _grabOverview(self, text=''):
        #  Grab the overview section of a previous HLS from the overall
        #  overall headline to the start of the first zone segment

        #  See if there is an overwiew in this text
        overviewSearch = re.search("(?is)(\.+.+)?[A-Z]{2}Z\d{3}", text)

        #  If we found a headline
        if overviewSearch is not None:

            #  Remove any zone blocks we may have grabbed by accident
            overview = re.sub("(?is)[A-Z]{2}Z\d{3}.+", "",
                              overviewSearch.group(1).strip())

            #  Return the cleaned-up overview string
            return overview

        #  Otherwise, return an indicator there is no overview in this text
        else:
            return ''

    #  Modified 12/15/2010 (MHB) - added a new flag which will cause the
    #  grabbed section text to be wrapped in framing codes if set to True.
    def _grabSection(self, text='', section='', useFrameCodes=False):
        #  Grab the specified subsection of text from the overall text

        #print "\n\nGRABBING SECTION", section
        #print "'%s'" % (text)

        #  If a subsection header was defined
        if section != '':

            # Add ending text so that last section can be found.
            text = text + "\n..."

            #  See if we can find it
            sectionSearch = re.search("(?ism).*^%s(.+?)^\." % (section), text)

            #  If we found the specified subsection
            if sectionSearch is not None:

#                print sectionSearch.groups()
#                print "'%s'" % (self._cleanText(sectionSearch.group(1).strip()))
                #  Clean it up
                sectionText = self._cleanText(sectionSearch.group(1).strip())

                # If we should wrap framing codes around this text
                if useFrameCodes:
                    sectionText = self._frame(sectionText)      #  do it

                #  Return the cleaned-up subsection
                return section + sectionText

        #  If we made it this far, return a null string
        return ''

    def _getProductInfo(self, argDict):
        #  The current argDict['hazards'] is set automatically by TextFormatter.py
        #  to use the zone combinations.   We need to re-do it to have all the
        #  hazards from the entire area
        #print "\n***************getProductInfo calling getHazardsTable"
        allAreas = self._inlandAreas()+self._marineAreas()+self._coastalAreas()
        argDict["combinations"]= [(allAreas,"Region1")]
        argDict['definition'] = self._definition
        hazards = self._getHazardsTable(argDict, self.filterMethod)
        argDict['hazards'] = hazards

        # Set up the areaDictionary for all to use
        accessor = ModuleAccessor.ModuleAccessor()
        self._areaDict = accessor.variable(self._areaDictionary, "AreaDictionary")
        # Get the statistics and general information for the segments
        # to be used for the Overview sections
        info = self._getSegmentInfo(self._segments)
        info.maxWind_CWA_MAOR = self._getWindStats(
            argDict, self._sampler, self._analysisList_HLS(),
            self._timeRangeList, self._cwaMaorLabel)
        return info

    #  Modified 12/15/2010 (MHB) - fixed a potential problem with the
    #  _stormTypeName variable.  If used as a failsafe it would have come out
    #  as "Cyclone Tropical" instead of "Tropical Cyclone".  Also disabled
    #  the "Unnamed" option.
    def _getStormInfo(self, argDict, info):
        #  Get the Storm information
        st = self._StormInfo
        self._stormType = "Tropical"
        self._stormName = "Cyclone"
        self._stormTypeName = self._stormType + " " +self._stormName

        # Get the name
#        if st.find("N/A") >=0:
#            self._stormTypeName = self._StormInfo_entry
#            if len(self._stormTypeName.strip()) == 0:
#                self._stormTypeName = self._frame("Enter Storm Name")
#            return

        # Get the product
        if st == "Enter PIL below (e.g. TCPEP1):":
            productID = self._StormInfo_entry
        else:   productID = self._StormInfo
        if self._useTestTCP():
            self._TCP = self._TCP_Product()
        else:
            self._TCP = self.getPreviousProduct(productID)
        stormDict = self._grabStormInfo(self._TCP)
        self._stormName = stormDict.get("StormName", "")
        self._stormType = stormDict.get("StormType", "")
        self._stormTypeName = self._stormType + " " + self._stormName
        self._decodeStormInfo(stormDict, info)
        # Storm movement in mph and the stated movement trend
        self._stormMovementTrend = "Storm Motion was " + stormDict.get("StormMotion","")
        # Storm intensity in mph and the stated intensity trend.
        self._stormIntensityTrend = "Storm Intensity was " + stormDict.get("StormIntensity","")

    ## New version from MHB 1/13/10
    def _decodeStormInfo(self, stormDict, info):
        self._stormTime = "|* Enter Storm Time *| "
        self._stormLat = "|* Enter Storm Lat *| "
        self._stormLon = "|* Enter Storm Lon *| "
        self._stormLocation = "|* Enter Storm Location *| "
        self._stormReference = ""
        self._stormLocalReferences = ""
        para = stormDict.get("StormCenter", "")
        # print "\npara", len(para), para
        if len(para)<= 0:
            return

        # Create the time string
        self._stormTime = self._formatLocalTime(para, info.allAreas)

        # Find stormLat, stormLon and stormLocation
        #     e.g. LATITUDE 15.7 NORTH...LONGITUDE 80.0 WEST
        stormLocation =""
        stormLat = None
        stormLon = None

        #  Make a pattern to find the latest storm location
        coordPtn = re.compile("(?i)(LATITUDE ([\d\.]+) ?((N|S)(O[RU]TH)?))..." +
                              "(AND )?(LONGITUDE ([\d\.]+) ?((W|E)([AE]ST)?)).+?")
##                              + "OR ((ABOUT )?.+)")

        #  Make a pattern to find the NHC reference location
        refPtn = re.compile("(?i)(WAS|OR) ((ABOUT )?\d+ MILES.+?" +
                            "(NORTH|SOUTH|EAST|WEST).+?)\.")

        #  Try to find these patterns in the text
        coordPtnMatch = coordPtn.search(para)
##        print "+" * 90
##        print "\ncoordinate search..."
##        print coordPtnMatch.groups()

        refPtnMatch = refPtn.search(para)
##        print "\nreference search..."
##        print refPtnMatch.groups()

        #  If we found the coordinates we were after
        if coordPtnMatch is not None:

            #  If we have the correct paragraph, set aside the latitude and
            #  longitude info as numbers
            self._stormLat = float(coordPtnMatch.group(2))
            self._stormLon = float(coordPtnMatch.group(8))      #  was 7

            #  Adjust latitude and longitude as need for "other" hemispheres
            if coordPtnMatch.group(4) in ["S", "s"]:
                self._stormLat *= -1.0

            if coordPtnMatch.group(10) in ["W", "w"]:
                self._stormLon *= -1.0

            #  Construct the storm location subphrase
            self._stormLocation = "%s...%s" % (coordPtnMatch.group(1),
                                               coordPtnMatch.group(7))  # was 6

        #  If we found the primary NHC reference we were after
        if refPtnMatch is not None:

            #  Set aside all the geographic reference text
##            stormReference = coordPtnMatch.group(11)
            stormReference = refPtnMatch.group(2)

            #  Watch out for some grammar gotchas with this reference
            stormReference = re.sub("(?i)^(WAS|OR) ", "", stormReference)

            #  See if there are multiple geographic references
            if re.search('(?i) and ', stormReference) is not None:

                #  Yes there are multiple references, so only keep the
                #  first one
                stormReference = re.sub("(?i) AND .+", "", stormReference)

            #  Also remove any metric distances
            self._stormReference = self._removeKM(stormReference)

        # Miles/km from chosen local reference
        self._stormLocalReferences = self._calcLocalReferences(
                self._stormLat, self._stormLon)

##        print "stormLocalRefs = ", self._stormLocalReferences

        #  Compare the NHC reference to the local references
        for localRef in self._stormLocalReferences:

##            print self._stormReference, localRef

            #  Get the locations from these statements
            nhcRef = re.search('(?i)(north|south|east|west) of (.+)',
                               self._stormReference)
            testRef = re.search('(?i)(north|south|east|west) of (.+)',
                               localRef)

##            print "nhcRef = '%s'\ttestRef = '%s'" % (nhcRef.group(2), testRef.group(2))

            #  If we have a local reference that matches the national
            #  center reference
            if testRef is not None and nhcRef is not None and \
               re.search("(?i)%s" % (testRef.group(2).strip()),
                         nhcRef.group(2)) is not None:

                #  Do not include the national reference
                self._stormReference = ""

    #  Modified 12/15/2010 (MHB) - modified to recognize the new way NHC will
    #  present metric speeds.  Will continue to recognize the "old" way for
    #  testing purposes as well.
    def _removeKM(self, words):
        # Remove references to KM e.g.
        #    420 KM... 100 KM/HR...

#        print "words = '%s'" % (words)

        kmSearch = re.compile("\.\.\. *[0-9]+ +(KM|KM/HR?) *\.?\.?\.?")

        #  Replace metric reference with a space to keep words from mashing
        #  together.
        words = kmSearch.sub(" ", words)

        #  Make sure we don't have any double space issues with this text
        doubleSpaces = re.findall('  +', words)
        for doubleSpace in doubleSpaces:
            words = re.sub(doubleSpace, ' ', words)

#        print "\tfinal words = '%s'" % (words)
        return words


    def _formatLocalTime(self, para, areas):
        # Create a time string in local time
        #  e.g.  2 AM EDT
        # Get the Z time hour
        timeSearch = re.compile("...([0-9]+) *(Z|UTC)...")
        timeStr = timeSearch.search(para)

##        gmtStr = para[timeStr.start():timeStr.end()]
##        gmt = gmtStr.strip("...").replace("Z","")
##        gmtHour = int(gmt)/100

        #  This code could bomb in the unlikely event we don't find a UTC
        #  time.  We should probably add some kind of default hour here,
        #  keyed off the current hour, to prevent this.  (MHB)
        try:
            #  Convert the hour portion of the time string to an integer
            gmtHour = int(timeStr.group(1)[:2])
        except:
            gmtHour = time.gmtime().tm_hour

        gmtTR = self.createTimeRange(gmtHour, gmtHour+1, "Zulu")
        gmtTime = gmtTR.startTime().unixTime()

        # Now make a string for each time zone
        zoneList = self._getTimeZoneList(areas)
        timeStrs = []
        timeDesc = ""
        for timeZone in zoneList:
            timeStr = self.formatTimeString(gmtTime, "%I %p %Z ", timeZone)
            timeStr = string.replace(timeStr, "  ", " ")
            timeStr = string.strip(timeStr)
            timeStr = timeStr.lstrip("0")
            if timeStr not in timeStrs:
                if len(timeStrs) > 0:
                    timeDesc += "...OR "
                timeStrs.append(timeStr)
                timeDesc += timeStr
        return timeDesc

    def _getTimeZoneList(self, areaList):
        # NOTE -- this code was taken from the middle of getAreaHeader
        #  in Header.py -- it really should be put back in and used
        #  in Header.py, but to avoid confusion, I'm repeating it here
        # get this time zone
        thisTimeZone = os.environ["TZ"]
        zoneList = []
        # check to see if we have any areas outside our time zone
        for areaName in areaList:
            if areaName in self._areaDict.keys():
                entry = self._areaDict[areaName]
                if not entry.has_key("ugcTimeZone"):   #add your site tz
                    if thisTimeZone not in zoneList:
                        zoneList.append(thisTimeZone)
                    continue  # skip this entry
                timeZoneList = entry["ugcTimeZone"]
                if type(timeZoneList) is types.StringType:  # a single value
                    timeZoneList = [timeZoneList]   # make it into a list
                for timeZone in timeZoneList:
                    if timeZone not in zoneList:
                        zoneList.append(timeZone)
        # if the resulting zoneList is empty, put in our time zone
        if len(zoneList) == 0:
            zoneList.append(thisTimeZone)
        # if the resulting zoneList has our time zone in it, be sure it
        # is the first one in the list
        try:
            index = zoneList.index(thisTimeZone)
            if index != 0:
                del zoneList[index]
                zoneList.insert(0, thisTimeZone)
        except:
            pass
        return zoneList

    def _calcLocalReferences(self, lat0, lon0):
        localRefs = []
        refList = self._LocalReferencePoints
        #refList.append(("Grand Cayman", (19.2, -81.4)))
        # Limit reference points
        refLimit = self._referencePointLimit()
        if len(refList) > refLimit:
            refList = refList[0:refLimit]
        for label, latLon in refList:
            lat, lon = latLon
            localRef = self._calcReference(lat0, lon0, lat, lon)
            localRef = localRef + " OF " + label
            localRef = localRef.replace(",","")
            localRefs.append(localRef)
        return localRefs

    def _oldCalcReference(self, lat0, lon0, lat1, lon1):
        RAD_TO_DEG = 57.296083
        #print "\ncalcReference", lat0, lon0, lat1, lon1
        #lat1 = lat0 + 1.0
        #lon1 = lon0 + 1.0
        latDist = (lat0-lat1) * 111.0
        avgLat = abs(lat0+lat1) / 2.0
        lonDist = (lon0-lon1) * 111.0 * cos(avgLat/RAD_TO_DEG)
        #lonDist = 111.0
        #latDist = 111.0
        distKm = sqrt((latDist*latDist)+(lonDist*lonDist))
        distMph = distKm * 0.62
        # Round to nearest 10
        distMph = self.round(distMph, "Nearest", 10)
        distMph_str = `int((distMph/10)*10)`
        distKm_str = `int((distKm/10)*10)`
        direct = atan2(lon0-lon1, lat0-lat1) * RAD_TO_DEG
        direction = self._dirInEnglish(direct)
        localRef ="About "+distMph_str+" miles "+direction
        print "localRef", localRef
        return localRef

    def _calcReference(self, lat0, lon0, lat1, lon1):
        #return self._oldCalcReference(lat0, lon0, lat1, lon1)
        distKm = self._distanceFromLatLon(lat0, lon0, lat1, lon1)
        distMph = distKm * 0.62
        # Round to nearest 10
        distMph = self.round(distMph, "Nearest", 10)
        distMph_str = `int((distMph/10)*10)`
        #distKm_str = `int((distKm/10)*10)`
        direction = self._bearing(lat1, lon1, lat0, lon0)
        direction = self._dirInEnglish(direction)
        localRef ="About "+distMph_str+" miles "+direction
        #print "localRef", localRef
        return localRef

    # Returns the distance from lat0, lon0 to lat1, lon1 in kilometers
    def _distanceFromLatLon(self, lat0, lon0, lat1, lon1):
        R = 6371.0
        lat0 = lat0 * DEG_TO_RAD
        lon0 = lon0 * DEG_TO_RAD
        lat1 = lat1 * DEG_TO_RAD
        lon1 = lon1 * DEG_TO_RAD
        dist = acos(sin(lat0) * sin(lat1) + cos(lat0) * cos(lat1) * cos(lon1 - lon0)) * R
        return dist

    def _bearing(self, lat0, lon0, lat1, lon1):

        dlat = (lat0 - lat1) * DEG_TO_RAD
        dlon = (lon0 - lon1) * DEG_TO_RAD

        y = sin(dlon) * cos(lat1 * DEG_TO_RAD)
        x = cos(lat0 * DEG_TO_RAD) * sin(lat1 * DEG_TO_RAD) - \
            (sin(lat0 * DEG_TO_RAD) * cos(lat1 * DEG_TO_RAD) * cos(dlon))

        direction = (atan2(x, y) / DEG_TO_RAD) - 90.0
        if direction < 0.0:
            direction = direction + 360.0
        direction = direction % 360

        return direction

##         lat0 = 30.0
##         lat1 = 20.0
##         lon0 = -80.0
##         lon1 = -90.0

##         print "complex dist:", distComplex(lat0, lon0, lat1, lon1)
##         print "bearing:", bearing(lat0, lon0, lat1, lon1)


    def _dirInEnglish(self, direction):
        dirList = ["north", "north-northeast", "northeast", "east-northeast",
                   "east", "east-southeast", "southeast", "south-southeast",
                   "south", "south-southwest", "southwest", "west-southwest",
                   "west", "west-northwest", "northwest", "north-northwest"]
        dirIndex = int((direction + 11.25) / 22.5)
        if dirIndex > 15:
            dirIndex = dirIndex - 16
        return dirList[dirIndex]

    #####################################################################################
    #####################################################################################
    #######  OVERVIEW Sections

##    def Overview_NewInformation(self, title, sectionDict, info):
##        t=""
##        ec = self._EventContext
##        if ec =="Abbreviated":
##            t+="New watches and or warnings have been issued. \n"
##        else:
##            t+= self._frame("Please enter new information here. Keep it concise.") + "\n"
##        return title + t

    def Overview_NewInformation(self, title, sectionDict, info):
        t=""
        ec = self._EventContext
        print "info.hazardHdlns = ", info.hazardHdlns

        if ec =="Abbreviated":
            hdlns = info.hazardHdlns
            #print "\n Headlines"
            reported = 0
            for hazardHdln in hdlns:
                key, landList, marineList, coastalList, inlandList = hazardHdln
                #print "hazard", hazardHdln
                hdln, act, phen, sig = key
                if phen == "HU" and sig == "S":
                    continue
                if act in self._ignoreActions():
                    continue
                if hdlns.index(hazardHdln) > 0:
                    t+= " and "
                t+= "A " + hdln
                reported += 1
            if reported > 0:
                if reported > 1: t+= " have "
                else:            t+= " has "
                t+="now been issued. "
        elif ec == "PostEvent":
            t+="Warnings have been discontinued.\n"

        else:
            t+= self._frame("Please enter new information here. Keep it concise.") + "\n"
        return title + t

############################################################################################

    def AreasAffected(self, title, sectionDict, info):
        t =  title

        if info.anyLand and info.anyMarine:
            t+= "This local statement provides important information and recommended actions for people and marine interests in "
            t+=self._all_select(info.allLand and info.allMarine)
            t+= " locations and coastal water of "+self._cwa_maor_descriptor()+ ". "

        else:
            if info.anyLand:
                t+= "This local statement provides important information and recommended actions for people in "
                t+=self._all_select(info.allLand)
                t+= " locations within " + self._cwa_descriptor() + ". "

            elif info.anyMarine:
                t+= "This local statement offers guidance and recommendations for mariners...as well as other marine interests...along "
                t+= self._all_select(info.allMarine)
                t+= " coastal water of " + self._maor_descriptor()  + ". "
        return t  + "\n"

    def _all_select(self, value):
        if value: return "All"
        else: return "Select"

    def _generalAreas(self, segmentAreas):
        """This method formats the general area description given the list of segmentAreas.
        """
        #  This method could grab information from a file formatted elsewhere.
        #  To use this capability, call this method with the appropriate
        #  argument:
        #
        #   text = self._ingestExternalFile("<path-to-external-file>")

        text = ''

        #  Make the general area Phrase - similar to HWO
        generalAreas = self.getGeneralAreaList(segmentAreas, areaDictName=self._areaDictionary)

        #  Make a list of all general areas we found
        areaLen = len(generalAreas)
        areaCount = 0
        areaPhrase = ""
        for generalArea in generalAreas:
            areaCount = areaCount + 1
            if areaCount == 1:
               conn = ""
            elif areaCount == areaLen:
               conn = " and "
            else:
               conn = "..."
            if generalArea[1] != "":
                partOfState = generalArea[1] + " "
            else:
                partOfState = ""

            #  Add this general area to the text
            areaPhrase = areaPhrase + conn + partOfState + generalArea[0]

        #  If we found any text - finish it up
        if len(areaPhrase.strip()) > 0:
            text = "%s.\n\n" % (areaPhrase)

        #  Return the completed text
        return text

    #####################################################################################
    def WatchesWarnings(self, title, sectionDict, info):
        t= title
        ec = self._EventContext
        fmtDict = self._overviewFormat()

        # Any WW will be False if there are no Watches or Warnings in the CWA or MAOR
        anyWW = self._checkHazard(
            info.hazardHdlns, [("HU","W"),("TY", "W"),("TR","W"), ("HU","A"),("TY", "A"),("TR","A")])

        # Find HU_S headlines and separate into "land" and "marine"
        # There will only be ONE HU S entry in hazardHdlns since they are
        #  consolidated across segments
        HU_S_Hdlns = []
        HU_S_landList = []
        HU_S_marineList = []
        for key, landList, marineList, coastalList, inlandList in info.hazardHdlns:
            hdln, act, phen, sig = key
            if act in self._ignoreActions():
                continue
            if phen == "HU" and sig == "S":
                HU_S_Hdlns.append((key, coastalList + inlandList, "land"))
                HU_S_Hdlns.append((key, marineList, "marine"))
                if len(coastalList + inlandList) > 0:
                    HU_S_landList = HU_S_landList + coastalList + inlandList
                if len(marineList) > 0:
                    HU_S_marineList = HU_S_marineList + marineList

        if ec == "NonEvent" and not anyWW and len(HU_S_Hdlns)>0:
            t+="Tropical cyclone watches and warnings are not in effect anywhere across "
            t+=self._cwa_maor_descriptor() + ".\n"

        elif ec == "PreEvent" and not anyWW:
            if len(HU_S_landList) > 0:
                t+="Although tropical cyclone watches or warnings are not in effect anywhere across "
                t+=self._cwa_descriptor()
                t+="...possible impacts from related hazards are becoming a concern for "
                if fmtDict["land"] == "listAreas":
                    t+= self._describeLocations(info, HU_S_landList, end="...")+ ".\n"
                else:
                    t+="portions of the area.\n"

            if len(HU_S_landList) > 0 and len(HU_S_marineList)>0: t+="\n"

            if len(HU_S_marineList)>0:
                t+="For marine interests...although tropical cyclone watches or warnings are not in effect anywhere across "
                t+=self._maor_descriptor()
                t+="...possible impacts from related hazards are becoming a concern for "
                if fmtDict["marine"] == "listAreas":
                    t+= self._describeLocations(info, HU_S_marineList, end="...")+ ".\n"
                else:
                    t+="portions of the "+ self._maor_descriptor() + ".\n"

        elif ec == "PostEvent" and not anyWW: # and (len(HU_S_landList)>0 or len(HU_S_marineList)>0):
            t+="Tropical cyclone watches and warnings are no longer in effect anywhere across "
            t+=self._cwa_maor_descriptor() + ".\n"

        elif ec == "PostTropical" and not anyWW: # and (len(HU_S_landList)>0 or len(HU_S_marineList)>0):
            t+="Tropical cyclone watches and warnings are no longer in effect anywhere across "
            t+=self._cwa_maor_descriptor()
            t+=". The issuance of tropical cyclone watches and warnings is being transitioned over to watches and warnings traditionally issued for non-tropical cyclone events.\n"
        else:
            t+=self._overview_headlines(info)
            if ec == "Abbreviated":
                t+=self._definition_stmt(info)
        if anyWW: t+=self._overview_HU_S_headlines(info, HU_S_Hdlns)
        t+=self._additional_headlines(info)
        return t

    def _definition_stmt(self, info):
        t = ""
        foundwatch = False
        foundwarning = False
        desc = " means that "
        descwatch = " conditions are possible within the next 48 hours somewhere within the specified " + \
               "areas.\n\n"
        descwarning = " conditions are expected within the next 36 hours somewhere within the specified " + \
               "areas.\n\n"
        ppwatch = " All persons in the watch areas should review their preparedness plan and be ready to " + \
               "implement it should a warning be issued for their area.\n\n"
        ppwarning = " All persons in the warning areas should already have preparations underway to protect " + \
               "life and property.\n\n"

        #  Initialize a new dictionary to pair phenSig codes with their action
        hazardDict = {}

        #  Iterate over all of the hazards
        for hazardTuple in info.hazardHdlns:
            print "\n\n" + "*"*80
            print "hazardTuple is:", hazardTuple

            #  Grab the phenomena code
            hazard = hazardTuple[0]
            print "hazard is:", hazard

            #  Split up the phenomena code
            (title, action, phen, sig) = hazard

            #  Store the action for this phenomena
            hazardDict["%s.%s" % (phen, sig)] = action

        #-----------------------------------------------------------------------
        #  Look at each of the hazards
        if self._checkHazard(info.hazardHdlns, [("HU","W")]) and \
           hazardDict["HU.W"] not in ["CAN", "UPG"]:
            hazardPhen = "Hurricane"
            hazardSig = "Warning"
            hazardPhenSig = hazardPhen+" "+hazardSig
            foundwarning = True
            t+= "A "+hazardPhenSig + desc + hazardPhen + descwarning

        if self._checkHazard(info.hazardHdlns, [("TY", "W")]) and \
           hazardDict["TY.W"] not in ["CAN", "UPG"]:
            hazardPhen = "Typhoon"
            hazardSig = "Warning"
            hazardPhenSig = hazardPhen+" "+hazardSig
            foundwarning = True
            t+= "A "+hazardPhenSig + desc + hazardPhen + descwarning

        if self._checkHazard(info.hazardHdlns, [("TR","W")]) and \
           hazardDict["TR.W"] not in ["CAN", "UPG"]:
            hazardPhen = "Tropical Storm"
            hazardSig = "Warning"
            hazardPhenSig = hazardPhen+" "+hazardSig
            foundwarning = True
            t+= "A "+hazardPhenSig + desc + hazardPhen + descwarning

        if foundwarning:
            t+= ppwarning

        if self._checkHazard(info.hazardHdlns, [("HU","A")]) and \
           hazardDict["HU.A"] not in ["CAN", "UPG"]:
            hazardPhen = "Hurricane"
            hazardSig = "Watch"
            hazardPhenSig = hazardPhen+" "+hazardSig
            foundwatch = True
            t+= "A "+hazardPhenSig + desc + hazardPhen + descwatch

        if self._checkHazard(info.hazardHdlns, [("TY", "A")]) and \
           hazardDict["TY.A"] not in ["CAN", "UPG"]:
            hazardPhen = "Typhoon"
            hazardSig = "Watch"
            hazardPhenSig = hazardPhen+" "+hazardSig
            foundwatch = True
            t+= "A "+hazardPhenSig + desc + hazardPhen + descwatch

        if self._checkHazard(info.hazardHdlns, [("TR","A")]) and \
           hazardDict["TR.A"] not in ["CAN", "UPG"]:
            hazardPhen = "Tropical Storm"
            hazardSig = "Watch"
            hazardPhenSig = hazardPhen+" "+hazardSig
            foundwatch = True
            t+= "A "+hazardPhenSig + desc + hazardPhen + descwatch

        if foundwatch:
            t+= ppwatch

        t+= "In order to make the best decisions...be sure that you understand the terminology and " + \
            "definitions associated with tropical cyclone events.\n\n"


        return t

    # In order to have the HazardsTable use the allowedHeadlines list,
    # we need to supply a filterMethod that uses allowedHeadlines instead of allowedHazards
    def _getAllowedHazardList(self, allowedHazardList=None):
        if allowedHazardList is None:
            allowedHazardList = self.allowedHazards()
        hazardList = []
        for h in allowedHazardList:
            if type(h) is types.TupleType:
                hazardList.append(h[0])
            else:
                hazardList.append(h)
        return hazardList

    def _altFilterMethod(self, hazardTable, allowedHazardsOnly=False):
        # Remove hazards not in allowedHeadlines list
        allowedHazardList = self._getAllowedHazardList(self.allowedHeadlines())
        return self._filterHazards(hazardTable, allowedHazardList,
                                   allowedHazardsOnly)

    def _filterHazards(self, hazardTable, allowedHazardList,
                       allowedHazardsOnly=False):
        newTable = []
        hazStr = ""
        for i in range(len(hazardTable)):
            if hazardTable[i]['sig'] != "":   # VTEC
                hazStr = hazardTable[i]['phen'] + "." + hazardTable[i]['sig']
            else:   #non-VTEC
                hazStr = hazardTable[i]['phen']

            if hazStr in allowedHazardList:
                newTable.append(hazardTable[i])
        if allowedHazardsOnly:
            return newTable
        # get a raw list of unique edit areas
        zoneList = []
        for t in newTable:
            if t['id'] not in zoneList:
                zoneList.append(t['id'])
        for zone in zoneList:
            # Remove lower priority hazards of the same type
            self.filterZoneHazards(zone, newTable)
        return newTable

    def _overview_headline_groups(self):
        landAreas = self._inlandAreas()+ self._coastalAreas()
        return [
          (["HU.W"], landAreas, "Hurricane Warning"),
          (["TY.W"], landAreas, "Typhoon Warning"),
          (["HU.W"], self._marineAreas(), "For marine interests...a Hurricane Warning"),
          (["TY.W"], self._marineAreas(), "For marine interests...a Typhoon Warning"),

          (["TR.W", "HU.A"], landAreas, "Tropical storm warning and a Hurricane Watch"),
          (["TR.W", "TY.A"], landAreas, "Tropical storm warning and a Typhoon Watch"),
          (["TR.W", "HU.A"], self._marineAreas(),
          "For marine interests...a tropical storm warning and a Hurricane Watch"),
          (["TR.W", "TY.A"], self._marineAreas(),
           "For marine interests...a tropical storm warning and a Typhoon Watch"),

          (["TR.W"], landAreas, "Tropical storm warning"),
          (["TR.W"], self._marineAreas(), "For marine interests...a Tropical Storm Warning"),

          (["HU.A"], landAreas, "Hurricane watch"),
          (["TY.A"], landAreas, "Typhoon watch"),
          (["HU.A"], self._marineAreas(), "For marine interests...a Hurricane Watch"),
          (["TY.A"], self._marineAreas(), "For marine interests...a Typhoon Watch"),

          (["TR.A"], landAreas, "Tropical storm watch"),
          (["TR.A"], self._marineAreas(), "For marine interests...a Tropical Storm Watch"),

          ]

    def _overview_headlines(self, info):
        # Put together Watches and Warnings
        # Need to group hazards
        hazardGroups = self._overview_headline_groups()
        hdlns = []
        for hazards, areas, hdln in hazardGroups:
            hdlns = hdlns + self._getHazardHdlns(info, hazards, hdln, areas)
        t=""
        t+=self._headlines(info, hdlns, qualifier=True)
        return t

    def _getHazardHdlns(self, info, hazards, hdln, areas):
        # For the overview --
        # Return a list of (key, areaList) tuples for the given hazards
        #   where key is (hdln, act, phen, sig)
        # Use ignoreActions and then separate w.r.t. NEW, etc versus CON
        hazardTable = self._argDict["hazards"]
        newAreas = []
        conAreas = []
        sortedHazards = copy.deepcopy(hazards)
        sortedHazards.sort()
        if len(hazards) > 1:
            # If we are testing for more than one hazard (e.g. TR.W and HU.A)
            #  If an area has both hazards with the same action, then the normal
            #   algorithm will catch it.
            #  However, if one is CON and the other NEW, then we will allow
            #    mixing of CON and NEW
            tryMixingConNew_flag = True
        else:
            tryMixingConNew_flag = False
        #print "\n Checking for ", hazards
        for area in areas:
            #print "   Area ", area
            # For each area determine the set of newHazards and conHazards
            areaHazards = hazardTable.getHazardList([area])
            #print "      hazards", areaHazards
            newHazards = []
            conHazards = []
            for areaHaz in areaHazards:
                act = areaHaz['act']
                if act in self._ignoreActions():
                    continue
                phenSig = areaHaz['phen'] + "." + areaHaz['sig']
                if phenSig == "HU.S":
                    continue
                if act == "CON": conHazards.append(phenSig)
                else: newHazards.append(phenSig)
            newHazards.sort()
            conHazards.sort()
            if newHazards == sortedHazards:
                newAreas.append(area)
            elif conHazards == sortedHazards:
                conAreas.append(area)
            elif tryMixingConNew_flag:
                newHazards = newHazards + conHazards
                newHazards.sort()
                if newHazards == sortedHazards:
                    newAreas.append(area)
        #print "new con Areas", newAreas, conAreas
        # Compose hdln lists
        new = []
        con = []
        phen, sig = hazards[0].split('.')
        if len(newAreas) > 0:
            key = (hdln, "NEW", phen, sig)
            new = [(key, newAreas)]
        if len(conAreas) > 0:
            key = (hdln, "CON", phen, sig)
            con = [(key, conAreas)]
        #print "new, con", new, con
        return new + con

    def _overview_HU_S_headlines(self, info, HU_S_Hdlns):
        # Gather and report the HU_S headlines
        t = ""
        fmtDict = self._overviewFormat()
        if fmtDict["land"]=="generic" and fmtDict["marine"]=="generic":
            return t

        if len(HU_S_Hdlns) == 0:
            return t

        for key, areaList, areaType in HU_S_Hdlns:
            # Report only if there is a non-empty areaList and
            # overview format is "listAreas" i.e. specific
            if len(areaList) == 0 or fmtDict[areaType] == "generic":
                continue
            if areaType == "marine": t+="\nFor marine interests..."
            else:                    t+="\n"
            t+="Although tropical cyclone watches or warnings are not in effect for "
            t+= self._describeLocations(info, areaList, end="...")
            t+= "possible impacts from related hazards are still a concern.\n"
        return t

    def _additional_headlines(self, info):
        # Report additional headlines
        t=""
        self._hazardHdlns, self._huAreas = self._getAdditionalHazards(info)
        t+=self._getAdditionalHeadlines(self._hazardHdlns, self._huAreas, info)
        return t

    def _getAdditionalHazards(self, info):
        argDict = self._argDict
        argDict['definition'] = self._definition
        altHazards = self._getHazardsTable(argDict, self._altFilterMethod)
        conTable = altHazards.consolidatedTableByID()

        # Consolidate across action codes
        hazDict = {}
        for hazard in conTable:
            hdln=hazard['hdln']
            phen=hazard['phen']
            sig=hazard['sig']
            act=hazard['act']
            if act in self._ignoreActions():
                continue
            for area in hazard['id']:
                hazDict.setdefault((hdln, phen, sig), []).append(area)

        #print "hazDict", hazDict
        hazardHdlns=[]
        huAreas = []
#        print "\nAdditional Hazard Headlines"
        for key in hazDict.keys():
            hdln, phen, sig = key
            huAreas = huAreas + hazDict[key]
            hazardHdln = ((hdln, "NEW", phen,sig), hazDict[key], [],[],[])
            #print "   ", hazardHdln, hazDict[key]
            hazardHdlns.append(hazardHdln)
        return hazardHdlns, huAreas

    def _getAdditionalHeadlines(self, hazardHdlns, huAreas, info):
        # We have a list of hazardHdlns and can use checkHazards
        # Additional Hazards
        t=""
        hdlnList = self._checkHazard(hazardHdlns, [("FA","A"),("FF","A")], returnList=True)
        print "hdlnList", hdlnList
        if len(hdlnList) > 0:
            t+="\n"
            t+=self._headlines(info, hdlnList, self._allPortions, ending=". ")
            t+="Please listen closely for any Flood Warnings that might be in effect for your area.\n"

        hdlnList = self._checkHazard(hazardHdlns, [("TO","A")], returnList=True)
        print "hdlnList", hdlnList
        if len(hdlnList) > 0:
            t+="\n"
            t+=self._headlines(info, hdlnList, self._allPortions, ending=". ")
            t+="Please listen closely for any Tornado Warnings that might be in effect for your area.\n"

        # Check additional hazards
        checkHazards = [("CF","W"), ("CF","A"),("CF","Y"),("RP","S"),("SU","W"),("SU","A"),("SU","Y"),
                        ("SR","W"),("SR","A"),("GL","W"),("GL","A"),
                        ("SC","Y"),("SI","Y"),("SW","Y"), ("RB","Y")]
        hazList = self._checkHazard(hazardHdlns, checkHazards, returnList=True)
        if len(hazList) > 0:
            t+= "\nPlease check the latest public and marine forecasts for detailed information about additional hazards.\n"
        return t

    def _allPortions(self, info, hazAreas, prefix="", suffix=""):
        # Used for overview headlines
        descriptor, checkAreas = self._determineDescriptor(info, hazAreas)
        portions = prefix + "portions of " + suffix
        allPortions = self._checkAreaInclusion(checkAreas, hazAreas, "ALL OF ", portions)
        return allPortions + descriptor

    def _allParts(self, info, hazAreas):
        # Used for overview additional headlines
        descriptor, checkAreas = self._determineDescriptor(info, hazAreas)
        allParts = self._checkAreaInclusion(checkAreas, hazAreas, "all ", "part ")
        return allParts + "OF " + descriptor + ". "

    def _entirePortions(self, info, hazAreas):
        # Used by the optional template for optional sections
        return self._checkAreaInclusion(
            info.allAreas, hazAreas, "the entire area. ", "portions of the area. ")

    def _checkAreaInclusion(self, compareAreas, hazAreas, allWords, partWords):
        words = allWords
        for area in compareAreas:
            if area not in hazAreas:
                words = partWords
                break
        return words

    def _headlines(self, info, headlineList, areaWordMethod=None,
                   ending="\n\n", qualifier=False):
        # Create the headlines from list of (key, hazAreas)
        #      where key is (hdln, act, phen, sig)
        t = ""
        for key, hazAreas in headlineList:
            hdln, act, phen, sig = key
            if act == "CON": actWords = " continues for "
            else:            actWords = " is in effect for "
            # Skip HU.S headines
            if (phen =='HU' and sig =='S'):
                continue
            if hdln[0] in ["A","I"]:a='an '
            elif hdln.find("for") == 0: a = ' '
            else:               a ='a '

            #print "\n Headline", hdln, phen
            if areaWordMethod is not None:
                areaWords=areaWordMethod(info, hazAreas)
            else:
                areaWords=self._describeLocations(info, hazAreas, qualifier=qualifier)
            t+= a+hdln + actWords + areaWords + ending
        return t

    def _areaWords(self, areas):
        if areas == []:
            return ""
        names = []
        areaDict = self._areaDict
        areas.sort()
        for area in areas:
            name = areaDict[area].get('altName', areaDict[area].get('ugcName', ''))
            names.append(name)
        areaTypeWords = ""
        areaWords = self.formatCountyString("", names)[1:]
        return areaWords

    def _describeLocations(self, info, areaList, end=". ", prefix="",
                           suffix="", qualifier=False):
        t = ""
        fmtDict = self._overviewFormat()
        inland, coastal, marine = self._areaType(areaList)
        #print "inland, coastal, marine", inland, coastal, marine, areaList
        if inland or coastal: fmt = fmtDict["land"]
        else:                 fmt = fmtDict["marine"]

        if fmt == "generic":
            suffix = ""
            if qualifier:
                if inland: suffix = "inland "
                elif coastal: suffix = "coastal "
            t+= self._allPortions(info, areaList, prefix, suffix)
        else:
            t+= "The following locations..." + self._areaWords(areaList)
        t+= end
        return t

    #####################################################################################
    def StormInformation(self, title, sectionDict, info):
        t = title

        st = self._StormInfo
#        if st.find("N/A (unnamed)") >= 0:
#            t+="Although the system of concern has not been named..."
#            t+="it is being actively monitored for signs of tropical cyclone development. "
#
#        elif st.find("N/A (downgraded)")>= 0:
#            t+=self._stormTypeName+" has been downgraded to below tropical storm strength..."
#            t+="but will continue to be monitored until it no longer threatens the area. "
#
#        else:
        t+="At "+ self._stormTime + "...the center of "

        #  Fix the grammar if dealing with "remnants"
        if re.search("(?i)remnants", self._stormTypeName) is not None:
            t+="The "

        t+=self._stormTypeName + " was located near "
        t+=self._stormLocation

        #  if we kept the national reference
        if self._stormReference.strip() != "":
            t+= "...OR " + self._stormReference

        #  Finish off the storm location sentence
        t+= ". "

        #  Now add the local references
        localRefs = self._stormLocalReferences
        if len(localRefs) > 0:
            t+= "This was "
            for localRef in self._stormLocalReferences:
                if localRefs.index(localRef) > 0:
                    orStr = "...or "
                else:
                    orStr = ""
                t+= orStr + localRef
            t+= ". "

        #  Do not place storm motion and intensity on separate lines of text
#        t+="\n"
        t = t.replace("miles...", "miles ")
        sm = self._stormMovementTrend
        si = self._stormIntensityTrend

        #  Combine the storm motion and intensity before we frame them

        smi = ""
        if sm != "": smi += sm + '.'
        if si != "": smi += ' ' + si + '.'
##        t+= self._frame(smi)
        t += smi
        return t

    #####################################################################################
    def SituationOverview(self, title, sectionDict, info):
        t = title
        un = self._Uncertainty
        ec = self._EventContext
        if ec == "Abbreviated":
            hdlns = info.hazardHdlns
            #print "\n Headlines"
            reported = 0
            for hazardHdln in hdlns:
                key, landList, marineList, coastalList, inlandList = hazardHdln
                #print "hazard", hazardHdln
                hdln, act, phen, sig = key
                if phen == "HU" and sig == "S":
                    continue
                if act in self._ignoreActions():
                    continue
                if hdlns.index(hazardHdln) > 0:
                    t+= " and "
                t+= "A " + hdln
                reported += 1
            if reported > 0:
                if reported > 1: t+= " have "
                else:            t+= " has "
                t+="now been issued. "
            t+="A more detailed statement will follow shortly.\n"

        if ec in ["PreEvent","Watch","Warning"]:
            if un=="High":
                t+="It is vital that you do not focus on the exact forecast track. "
                t+="To do so could result in bad decisions and place you or those you are "
                t+="responsible for at greater risk. "
            elif un == "Average":
                t+="When making decisions...do not focus on the exact forecast track. "

        if ec != "Abbreviated": t+=self._frame("Succinctly describe the expected evolution of the event for the CWA & MAOR; which hazards are of greater (or lesser) concern, forecast focus, etc.")+ "\n"

        if ec in ["PreEvent", "Watch"]:
            if info.anyLand:
                t+="It is too early to provide exact wind and surge forecast values for specific locations. "
                damage = self._getCategoryDamage(info.maxWind_CWA_MAOR)
                if damage.strip() != "":
                    t+="A general concern should be for the possibility of "+damage+" somewhere within "\
                    + self._cwa_descriptor() + ". "

        return t
   #####################################################################################
    def Overview_PrecautionaryPreparednessActions(self, title, sectionDict, info):
        t = title
        ec = self._EventContext
        if ec == "NonEvent": t+=self.overview_pp_nonEvent(info)
        elif ec == "PreEvent": t+= self.overview_pp_preEvent(info)
        elif ec == "Abbreviated": t+=self._overview_pp_abbrev(info)
        elif ec == "Watch": t+=self._overview_pp_watch(info)
        elif ec == "Warning": t+=self._overview_pp_warning(info)
        elif ec == "Conditions": t+=self._overview_pp_conditions(info)
        elif ec == "PostEvent": t+=self._overview_pp_postEvent(info)
        elif ec == "PostTropical": t+=self._overview_pp_postTropical(info)
        endStr = sectionDict.get("endStr", "")
        return t + endStr

    def overview_pp_nonEvent(self, info):
        t=""
        if info.anyInland or info.anyCoastal:
            t+= """
People are urged to remain informed and listen for any
significant changes to the forecast. Do not listen to rumors or
uninformed opinions. Rather...seek authoritative information from
your local National Weather Service office and emergency
management.

"""

        if info.anyCoastal or info.anyMarine:
            t+= """
Mariners should keep informed of the latest coastal waters
forecast.
"""
        return self._frame(t.strip())

    def overview_pp_preEvent(self, info):
        t = ""
        if info.anyInland or info.anyCoastal:
            t+= """
Even before the issuance of watches or warnings...it may become
necessary for local authorities to render evacuation orders. If
told to leave...do so as soon as possible.

This is a good time for residents to go over their hurricane
disaster plan. Visitors are encouraged to check with hotel
management or with local officials regarding any actions they
should take.

The following are suggested actions that can be taken at this
time...
- check batteries for radios and flashlights.
- stock up on drinking water and canned or dried food.
- ensure you have a manual can opener.
- have enough for at least three to five days per person.
- gather medicines...toiletries...and first aid supplies.
- have a sufficient amount of cash on hand since credit cards and
  automated cash machines do not work without power.
- check fuel levels on automobiles...generators...and chain saws.
- if you need to make a trip to the hardware store...the grocery
  store...or the gas station...do so as early as possible.
- determine where you should seek shelter if the storm approaches
  your area.
- consider whether you live in a potential evacuation zone. If
  so...identify prescribed evacuation routes which lead out of the
  threatened areas.
- learn the locations of official shelters.

Please visit www.ready.gov for a more complete list of items to
include in an emergency preparedness kit.

In all cases...heed the advice of local officials and comply with
any orders that are issued.

"""
        if info.anyCoastal or info.anyMarine:
            t+= """
Mariners should monitor the coastal waters forecast for unsafe
conditions. Consider early steps for securing your craft. If
small craft must go out and current conditions allow...do not
venture far from port and do not stay out very long. Return to
port quickly if a watch or warning is issued.

"""
        return self._frame(t.strip())

    def _overview_pp_abbrev(self, info):
        t=""

##        print "\n\n" + "*"*80
##
##        print info.hazardHdlns

        #-----------------------------------------------------------------------
        #  Determine if this is a downgrade

        downgradeWarning = 0    #  flag to track any downgrades of a warning
        upgradeWarning = 0    #  flag to track any downgrades of a warning

        #  If we have more than one hazard
        if len(info.hazardHdlns) > 1:

            #  Set aside the hazard info for comparison
            (baseTitle, baseAction, basePhen, baseSig) = info.hazardHdlns[0][0]
            baseAreas =[]

            #  Combine all the areas affected by this hazard into one list
            for areaList in info.hazardHdlns[0][1:]:
                baseAreas = baseAreas + areaList

            #  Look through all the hazards we have - after the first one
            for hazard in xrange(1, len(info.hazardHdlns)):

                print "\nworking on hazard index -> ", hazard
                print info.hazardHdlns[hazard], "\n"

                #  Split up the hazard info for this hazard
                (title, action, phen, sig) = info.hazardHdlns[hazard][0]
                areas =[]

                #  Combine all the areas affected by this hazard into one list
                for areaList in info.hazardHdlns[hazard][1:]:
                    areas = areas + areaList

                print "baseAreas = ", baseAreas
                print "areas = ", areas
                print "basePhen = ", basePhen, "   baseSig = ", baseSig, "   baseAction = ", baseAction
                print "    phen = ", phen, "       sig = ", sig, "       action = ", action

                #  Look specifically for the case where we are downgrading from
                #  a hurricane/typhoon warning to a tropical storm warning
                if ((basePhen in ["HU", "TY"] and baseSig == "W" and
                     baseAction == "CAN" and phen == "TR" and sig == "W" and
                     action in ["NEW", "EXA"]) or
                    (basePhen == "TR" and baseSig == "W" and
                     baseAction in ["NEW", "EXA"] and phen in ["HU", "TY"] and
                     sig == "W" and action == "CAN")):

                    print "\nWorking on an downgrade here."

                    #  See if the current zone combination is part of downgrade
                    for area in areas:

                        #  If this zone segment is part of the downgrade
                        if area in baseAreas:

                            #  Indicate the downgrade and move on
                            downgradeWarning = 1
                            break

                #  Look specifically for the case where we are upgrading from
                #  a tropical storm warning to a hurricane warning
                if ((basePhen == "TR" and baseSig == "W" and
                     baseAction == "UPG" and phen in ["HU", "TY"] and
                     sig == "W" and action in ["NEW", "EXA"]) or
                    (basePhen in ["HU", "TY"] and baseSig == "W" and
                     baseAction in ["NEW", "EXA"] and phen== "TR" and
                     sig == "W" and action == "UPG")):

                    print "\nWorking on an upgrade here."

                    #  See if the current zone combination is part of downgrade
                    for area in areas:

                        #  If this zone segment is part of the downgrade
                        if area in baseAreas:

                            #  Indicate the downgrade and move on
                            upgradeWarning = 1
                            break

        print "upgrade = ", upgradeWarning, "\tdowngrade = ", downgradeWarning
        #  If there are and land or coastal sites
        if info.anyInland or info.anyCoastal:

            #  If there are no upgrades or downgrades
            if not upgradeWarning and not downgradeWarning:

                #  Completely new watches/warnings
                t+="""
For those under a watch or warning...now is the time to initiate
preparations according to your hurricane disaster plan specific
to your home or business.

For those nearby...review your hurricane disaster plan and
become ready to act if a watch or a warning is later issued for
your area.

It is important to actively listen for forthcoming information
from your local National Weather Service office and emergency
management agency.

"""

            #  If this is an upgraded warning
            if upgradeWarning and not downgradeWarning:

                #  upgraded warning
                t+="""
For those now under the new warning...now is the time to
initiate preparations according to your hurricane disaster plan
specific to your home or business...if you have not already
done so.

It is important to actively listen for forthcoming information
from your local National Weather Service office and emergency
management agency.

"""

            #  If this is a downgraded warning
            if downgradeWarning and not upgradeWarning:

                #  Downgraded warning
                t+="""
While the intensity of this storm is no longer expected to be
as strong...there is still a threat to life and property. For
those still under a warning...continue to implement your
hurricane disaster plan specific to your home or business.

It is important to actively listen for forthcoming information
from your local National Weather Service office and emergency
management agency.

"""

            #  If this is a upgrade and downgraded warning
            if downgradeWarning and upgradeWarning:

                #  Huh?! warning
                t+="""
There is still a threat to life and property. Continue to
implement your hurricane disaster plan specific to your home or
business.

It is important to actively listen for forthcoming information
from your local National Weather Service office and emergency
management agency.

"""

        #  Marine zones
        if info.anyMarine:

            #  If there are no upgrades or downgrades
            if not upgradeWarning and not downgradeWarning:

                #  Completely new watches/warnings
                t+= """
Mariners are urged to make all necessary preparations to return
to port...seek safe harbor...and secure their craft. Now is the
time to initiate preparations according to your emergency plan
for tropical systems. Monitor weather broadcasts for changes to
the latest forecast and listen for further statements from local
officials.

"""


            #  If this is an upgraded warning
            if upgradeWarning and not downgradeWarning:

                #  upgraded warning
                t+="""
Mariners are urged to return to port...seek safe harbor...and
secure their craft. Now is the time to complete preparations
according to your emergency plan for tropical systems. Monitor
weather broadcasts for changes to the latest forecast and
listen for further statements from local officials.

"""

            #  If this is a downgraded warning
            if downgradeWarning and not upgradeWarning:

                #  Downgraded warning
                t+="""
While the intensity of this storm is no longer expected to be
as strong...there is still a threat to life and property.
Mariners are urged to remain in port and secure their craft.
Continue to implement your emergency plan for tropical systems.
Monitor weather broadcasts for changes to the latest forecast
and listen for further statements from local officials.

"""

            #  If this is a upgrade and downgraded warning
            if downgradeWarning and upgradeWarning:

                #  Huh?! warning
                t+="""
There is still a threat to life and property. Continue to
implement your emergency plan for tropical systems. Monitor
weather broadcasts for changes to the latest forecast and
listen for further statements from local officials.

"""

        return self._frame(t.strip())


    def _overview_pp_watch(self, info):
        t=""
        public_A= self._checkHazard(info.hazardHdlns,
                                 [("HU","A"),("TR","A"),("TY","A")], ["land"])
        coastal_A=self._checkHazard(info.hazardHdlns,
                                    [("HU","A"),("TR","A"),("TY","A")], ["coastal"])
        marine_A=self._checkHazard(info.hazardHdlns,
                                   [("HU","A"),("TR","A"),("TY","A")], ["marine"])
        if public_A:
            t+= """
For those under a watch...now is the time to begin preparing your
home or business according to your hurricane disaster plan.
Listen for possible warnings and be ready to evacuate if
necessary. Heed the advice of local officials and comply with any
orders that are issued.

"""
        if coastal_A:
            t+= """
For interests at ports...docks...and marinas...it is recommended
that you perform the prescribed preparations according to your
emergency operations plan for tropical cyclones. If you live on a
boat...begin to safely secure your craft and make plans to leave
it for adequate land based shelter. Listen for possible warnings.

"""
        if coastal_A or marine_A:
            t+= """
Regarding the coastal waters under a watch...small craft should
return to port or seek safe harbor.

Closely monitor NOAA weather radio or other local news outlets
for official storm information. Listen for possible changes to
the forecast.

"""
        if public_A:
            t+= """
For additional precautionary and preparedness information...
Please refer to the detailed recommendations relative to your
location as further described by your local National Weather
Service office and your local emergency management.

"""
        return self._frame(t.strip())

    def _overview_pp_warning(self, info):
        t=""
        public_W= self._checkHazard(info.hazardHdlns,
                                 [("HU","W"),("TR","W"),("TY","W")], ["land"])
        coastal_W=self._checkHazard(info.hazardHdlns,
                                 [("HU","W"),("TR","W"),("TY","W")], ["coastal"])
        marine_W=self._checkHazard(info.hazardHdlns,
                                     [("HU","W"),("TR","W"),("TY","W")], ["marine"])
        public_A= self._checkHazard(info.hazardHdlns,
                                 [("HU","A"),("TR","A"),("TY","A")], ["land"])
        coastal_A=self._checkHazard(info.hazardHdlns,
                                    [("HU","A"),("TR","A"),("TY","A")], ["coastal"])
        marine_A=self._checkHazard(info.hazardHdlns,
                                   [("HU","A"),("TR","A"),("TY","A")], ["marine"])
        if public_W:
            t+= """
For those under a warning...now is the time to rush to completion
preparations for the protection of life and property. Evacuate if
directed to do so by local officials...or if your home is
vulnerable to high winds or flooding.

"""
        if coastal_W:
            t+= """
For interests at ports...docks...and marinas...urgently complete
prescribed preparations according to your emergency operations
plan for tropical cyclones. If you live on a boat...make final
preparations for securing your craft before leaving it. Be sure
to account for the possible closure of bridges and causeways.

"""
        if coastal_W or marine_W:
            t+= """
Regarding any coastal waters under a warning...small craft should
remain in port and well secured.

"""
        if public_A:
            t+= """
For those under a watch...continue with your preparations and
listen for possible warnings.

"""
        if coastal_A or marine_A:
            t+= """
Regarding any coastal waters under a watch...small craft should
return to port or seek safe harbor. Determine the best strategy
for securing your craft.

Closely monitor NOAA weather radio or other local news outlets
for official storm information. Listen for possible changes to
the forecast.

"""
        if public_W:
            t+= """
For additional precautionary and preparedness information...
Please refer to the detailed recommendations relative to your
location as further described by your local National Weather
Service office and local emergency management.

"""
        return self._frame(t.strip())

    def _overview_pp_conditions(self, info):
        t=""
        if info.anyLand:
            t+=  """
During the storm...stay inside and away from windows. Do not
venture outside when high winds are occurring or during temporary
lulls as flying debris can easily...and suddenly...cause serious
injury.

Have a well-charged cell phone nearby...keeping network
communications as open as possible for emergencies.

Closely monitor NOAA weather radio or other local news outlets
for official storm information. Listen for possible changes to
the forecast.

"""
        if info.anyMarine:
            t+= """
For small craft who failed to make it to safe harbor or port...
And are now in distress...radio your situation according to
maritime protocol. If appropriate...deploy your emergency
distress beacon. Ensure that everyone is wearing life jackets...
And survival suits if available.

"""
        return self._frame(t.strip())

    def _overview_pp_postEvent(self, info):
        t=""
        if info.anyLand:
            t+=  """
Many casualties occur after a storm has passed. Be smart and use
caution. Continue to heed the advice of local officials as they
conduct rescue and recovery efforts. Wait for the all-clear
signal before re-entering evacuation zones or any area that
received significant damage or flooding.

Pay attention for possible road closures and stay away from
downed power lines. Listen for any boil water alerts.

"""
        if info.anyCoastal or info.anyMarine:
            t+= """
Mariners should check the latest coastal waters forecast before
making any definite plans.

"""
        return self._frame(t.strip())

    def _overview_pp_postTropical(self, info):
        t=""
        if info.anyLand:
            t+= """
Everyone is urged to stay informed of the situation. Remain
diligent in your efforts to protect life and property.

"""
        if info.anyCoastal or info.anyMarine:
            t+= """
Mariners are advised to keep their guard up while closely
monitoring the latest coastal waters forecast. Small craft should
remain in port until this storm passes.

"""
        return self._frame(t.strip())

#####################################################################################
    def NextUpdate(self, title, sectionDict, info):
        t = title
        wfo = self._wfoCity
        if self._NextUpdate == "Shortly":
            t+= "The next local statement will be issued by the National Weather Service in "
            t+= wfo
            t+= " shortly. It will provide important details regarding the evolving tropical cyclone threats and their potential impacts upon the area. "
        elif self._NextUpdate == "Enter":
            t+="The next local statement will be issued by the National Weather Service in "
            t+= wfo
            t+=" around "
            t+= self._NextUpdate_entry
            t+="...or sooner if conditions warrant. "
        elif self._NextUpdate == "Conditions":
            t+="The next local statement will be issued by the National Weather Service in "
            t+=wfo
            t+=" as conditions warrant. "
        elif self._NextUpdate == "LastIssuance":
            t+="As it pertains to this event...this will be the last local statement issued by the National Weather Service in "
            t+=wfo
            t+=" regarding the effects of tropical cyclone hazards upon the area. "
        return t

    #####################################################################################
    #####################################################################################
    #######  SEGMENT Sections

    def NewInformation(self, title, argDict, segment, section, info):
        t=""
        segmentNum, segmentAreas, situation, scenario, sections, extraInfo = segment
        if situation=="Abbreviated":
            hdlns = info.hazardHdlns
            #print "\n Headlines"
            reported = 0
            for hazardHdln in hdlns:
                key, landList, marineList, coastalList, inlandList = hazardHdln
                #print "hazard", hazardHdln
                hdln, act, phen, sig = key
                if phen == "HU" and sig == "S":
                    continue
                if act in self._ignoreActions():
                    continue
                if hdlns.index(hazardHdln) > 0:
                    t+= " and "
                t+= "A " + hdln
                reported += 1
            if reported > 0:
                if reported > 1: t+= " have "
                else:            t+= " has "
                t+="now been issued. "
            t+="A more detailed statement will follow shortly.\n"
        else:
            t+= self._frame("Please enter new information here.") + "\n"
        return title + t

    #####################################################################################
    def PrecautionaryPreparednessActions(self, title, argDict, segment, section, info):
        t=""
        segmentNum, segmentAreas, situation, scenario, sections, extraInfo = segment
        #  NonEvent
        if situation == "NonEvent":
            if scenario=="ActiveNonEvent":
                if info.anyLand:
                    t+=self._pp_dict("NonEvent", ["ActiveNonEvent", "land"])
                if info.anyCoastal or info.anyMarine:
                    t+=self._pp_dict("NonEvent", ["ActiveNonEvent", "marine"])
            elif scenario=="EndNonEvent":
                if info.anyLand or info.anyMarine:
                    t+=self._pp_dict("NonEvent", ["EndNonEvent", "everywhere"])

        # PreEvent
        elif situation=="PreEvent":
            if scenario=="Advancing":
                if info.anyLand:
                    t+=self._pp_dict("PreEvent", ["Advancing", "land"])
                if info.anyCoastal or info.anyMarine:
                    t+=self._pp_dict("PreEvent", ["Advancing", "marine"])
            elif scenario=="Peripheral":
                if info.anyLand:
                    t+=self._pp_dict("PreEvent", ["Peripheral", "land"])
                if info.anyCoastal or info.anyMarine:
                    t+=self._pp_dict("PreEvent", ["Peripheral", "marine"])
            elif scenario=="InSitu":
                if info.anyLand:
                    t+=self._pp_dict("PreEvent", ["InSitu", "land"])
                if info.anyCoastal or info.anyMarine:
                    t+=self._pp_dict("PreEvent", ["InSitu", "marine"])

        # Abbreviated
        elif situation=="Abbreviated":
            HU_A = self._checkHazard(info.hazardHdlns, [("HU","A"),("TY","A")])
            TR_W = self._checkHazard(info.hazardHdlns, [("TR","W")])
            if self._checkHazard(info.hazardHdlns, [("HU","W"),("TY","W")]):
                if info.anyLand:
                    t+= self._pp_dict("Abbreviated", ["HU_W", "land"])
                if info.anyMarine:
                    t+=self._pp_dict("Abbreviated", ["HU_W", "marine"])
            elif HU_A and TR_W:
                if info.anyLand:
                    t+=self._pp_dict("Abbreviated", ["TR_W_HU_A", "land"])
                if info.anyMarine:
                    t+=self._pp_dict("Abbreviated", ["TR_W_HU_A", "marine"])
            elif self._checkHazard(info.hazardHdlns, [("HU","A")]):
                if info.anyLand:
                    t+=self._pp_dict("Abbreviated", ["HU_A", "land"])
                if info.anyMarine:
                    t+=self._pp_dict("Abbreviated", ["HU_A", "marine"])
            elif TR_W:
                if info.anyLand:
                    t+=self._pp_dict("Abbreviated", ["TR_W", "land"])
                if info.anyMarine:
                    t+=self._pp_dict("Abbreviated", ["TR_W", "marine"])
            elif self._checkHazard(info.hazardHdlns, [("TR","A")]):
                if info.anyLand:
                    t+=self._pp_dict("Abbreviated", ["TR_A", "land"])
                if info.anyMarine:
                    t+=self._pp_dict("Abbreviated", ["TR_A", "marine"])


        elif situation=="Watch":
            if self._checkHazard(info.hazardHdlns, [("HU","A"),("TY","A")]):
                if scenario == "Advancing":
                    if info.anyLand:
                        t+=self._pp_dict("Watch", ["HU_A", "Advancing", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Watch", ["HU_A", "Advancing", "marine"])
                elif scenario == "Peripheral":
                    if info.anyLand:
                        t+=self._pp_dict("Watch", ["HU_A", "Peripheral", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Watch", ["HU_A", "Peripheral", "land"])
                else: # In Situ
                    if info.anyLand:
                        t+=self._pp_dict("Watch", ["HU_A", "InSitu", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Watch", ["HU_A", "InSitu", "marine"])
            if self._checkHazard(info.hazardHdlns, [("TR","A")]):
                if scenario == "Advancing":
                    if info.anyLand:
                        t+=self._pp_dict("Watch", ["TR_A", "Advancing", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Watch", ["TR_A", "Advancing", "marine"])
                elif scenario == "Peripheral":
                    if info.anyLand:
                        t+=self._pp_dict("Watch", ["TR_A", "Peripheral", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Watch", ["TR_A", "Peripheral", "land"])
                else: # In Situ
                    if info.anyLand:
                        t+=self._pp_dict("Watch", ["TR_A", "InSitu", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Watch", ["TR_A", "InSitu", "marine"])
        # Warning
        elif situation=="Warning":
            HU_W = self._checkHazard(info.hazardHdlns, [("HU","W"),("TY","W")])
            TR_W = self._checkHazard(info.hazardHdlns, [("TR","W")])
            HU_A = self._checkHazard(info.hazardHdlns, [("HU","A"),("TY","A")])
            if HU_W:
                if scenario == "Advancing":
                    if info.anyLand:
                        t+=self._pp_dict("Warning", ["HU_W", "Advancing", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Warning", ["HU_W", "Advancing", "marine"])
                elif scenario == "Peripheral":
                    if info.anyLand:
                        t+=self._pp_dict("Warning", ["HU_W", "Peripheral", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Warning", ["HU_W", "Peripheral", "land"])
                else: # In Situ
                    if info.anyLand:
                        t+=self._pp_dict("Warning", ["HU_W", "InSitu", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Warning", ["HU_W", "InSitu", "marine"])
            elif TR_W and HU_A:
                if scenario == "Advancing":
                    if info.anyLand:
                        t+=self._pp_dict("Warning", ["TR_W_HU_A", "Advancing", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Warning", ["TR_W_HU_A", "Advancing", "marine"])
                elif scenario == "Peripheral":
                    if info.anyLand:
                        t+=self._pp_dict("Warning", ["TR_W_HU_A", "Peripheral", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Warning", ["TR_W_HU_A", "Peripheral", "land"])
                else: # In Situ
                    if info.anyLand:
                        t+=self._pp_dict("Warning", ["TR_W_HU_A", "InSitu", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Warning", ["TR_W_HU_A", "InSitu", "marine"])
            elif TR_W:
                if scenario == "Advancing":
                    if info.anyLand:
                        t+=self._pp_dict("Warning", ["TR_W", "Advancing", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Warning", ["TR_W", "Advancing", "marine"])
                elif scenario == "Peripheral":
                    if info.anyLand:
                        t+=self._pp_dict("Warning", ["TR_W", "Peripheral", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Warning", ["TR_W", "Peripheral", "land"])
                else: # In Situ
                    if info.anyLand:
                        t+=self._pp_dict("Warning", ["TR_W", "InSitu", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Warning", ["TR_W", "InSitu", "marine"])

        # Conditions
        elif situation=="Conditions":
            if scenario=="Imminent":
                if self._checkCategory(info.maxWind, "Cat3"):
                    if info.anyLand:
                        t+=self._pp_dict("Conditions", ["Imminent", "Cat3", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Conditions", ["Imminent", "Cat3", "marine"])
                elif self._checkCategory(info.maxWind, "Cat1"):
                    if info.anyLand:
                        t+=self._pp_dict("Conditions", ["Imminent", "Cat1", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Conditions", ["Imminent", "Cat1", "marine"])
                elif info.maxWind >= 34:
                    if info.anyLand:
                        t+=self._pp_dict("Conditions", ["Imminent", "34", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Conditions", ["Imminent", "34", "marine"])

            elif scenario == "Ongoing":
                if self._checkCategory(info.maxWind, "Cat3"):
                    if info.anyLand:
                        t+=self._pp_dict("Conditions", ["Ongoing", "Cat3", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Conditions", ["Ongoing", "Cat3", "marine"])
                elif self._checkCategory(info.maxWind, "Cat1"):
                    if info.anyLand:
                        t+=self._pp_dict("Conditions", ["Ongoing", "Cat1", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Conditions", ["Ongoing", "Cat1", "marine"])
                elif info.maxWind >= 34:
                    if info.anyLand:
                        t+=self._pp_dict("Conditions", ["Ongoing", "34", "land"])
                    if info.anyMarine:
                        t+=self._pp_dict("Conditions", ["Ongoing", "34", "marine"])

            elif scenario == "Diminishing":
                if info.anyLand:
                    if info.maxWind >= 64:
                        desc = "Hurricane"
                        somewhat = ""
                    elif info.maxWind >= 34:
                        desc = "Tropical storm"
                        somewhat = "Somewhat "
                    else:
                        desc = "Strong wind"
                        somewhat = "Somewhat "
                    landStr=self._pp_dict("Conditions", ["Diminishing", "land"])
                    landStr = landStr.replace("{desc}", desc)
                    landStr = landStr.replace("{somewhat} ", somewhat)
                    t+=landStr
                if info.anyMarine:
                    t+=self._pp_dict("Conditions", ["Diminishing", "marine"])

        # PostEvent
        elif situation=="PostEvent":
            if scenario=="Immediate":
                if info.anyLand:
                    t+=self._pp_dict("PostEvent", ["Immediate", "land"])
                if info.anyCoastal or info.anyMarine:
                    t+=self._pp_dict("PostEvent", ["Immediate", "marine"])
            elif scenario== "NoImpact":
                if info.anyLand or info.anyMarine:
                    t+=self._pp_dict("PostEvent", ["NoImpact", "general"])
            elif scenario=="LongTerm":
                if info.anyLand:
                    t+=self._pp_dict("PostEvent", ["LongTerm", "land"])
                if info.anyCoastal or info.anyMarine:
                    t+=self._pp_dict("PostEvent", ["LongTerm", "marine"])

        # PostTropical
        elif situation=="PostTropical":
            if scenario=="InProgress":
                t+=self._pp_dict("PostTropical", ["InProgress"])
            else:
                t+=self._pp_dict("PostTropical", ["Completed"])

        return title + t

    #####################################################################################
    def Probability(self, title, argDict, segment, section, info):
        t=""
        segmentNum, segmentAreas, situation, scenario, sections, extraInfo = segment
        if situation=="NonEvent":
            t+=self._frame("As currently assessed...the onset of either tropical storm or hurricane conditions is unlikely to occur.")+ "\n"

        elif situation=="PreEvent":
            if scenario=="Advancing":  t+=self._prob_stmts(info) + "\n"
            elif scenario=="Peripheral":  t+=self._prob_stmts(info, ifWording=True) + "\n"
            else:
                t+="At this time...the probability of either tropical storm or hurricane conditions cannot be determined until the system becomes an active tropical cyclone.  However...based on the latest outlook...the chance of tropical cyclone formation is "
                t+= self._frame("low/medium/high from twoxxx. ")
        elif situation=="Abbreviated":
                pass
        elif situation in ["Watch", "Warning"]:
            if scenario=="Advancing":  t+=self._prob_stmts(info) + "\n"
            elif scenario in ["Peripheral", "InSitu"]:
                t+=self._prob_stmts(info, ifWording=True) + "\n"
        elif situation in ["Conditions", "PostEvent", "PostTropical"]:
                pass

        return title + t

    #####################################################################################
    def Wind(self, title, argDict, segment, section, info):
        t=""
        segmentNum, segmentAreas, situation, scenario, sections, extraInfo = segment
        # NonEvent
        if situation=="NonEvent":
            t+=self._wind_NonEvent(info)+ "\n"
        # PreEvent
        elif situation=="PreEvent":
            if scenario=="Advancing":
                t+=self._wind_PreEvent_Advancing(info)
            elif scenario=="Peripheral":
                t+=self._wind_PreEvent_Peripheral(info)
            else: # In Situ
                t+=self._wind_PreEvent_InSitu(info)
            t+=self._genericImpact_stmt(info) + "\n"

        # Abbreviated
        elif situation=="Abbreviated":
                pass

        # Watch
        elif situation=="Watch":
            if scenario=="Advancing":
                t+=self._wind_Watch_Advancing(info)
            elif scenario=="Peripheral":
                t+=self._wind_Watch_Peripheral(info)
            else: # In Situ
                t+=self._wind_Watch_InSitu(info)
            t+=self._genericImpact_stmt(info) + "\n"

        # Warning
        elif situation=="Warning":
            if scenario=="Advancing":
                t+=self._wind_Warning_Advancing(info)
            elif scenario=="Peripheral":
                t+=self._wind_Warning_Peripheral(info)
            else: # In Situ
                t+=self._wind_Warning_InSitu(info)
            t+=self._potentialImpact_stmt(info) + "\n"

        # Conditions
        elif situation=="Conditions":
            if scenario=="Imminent":
                t+=self._wind_Conditions_Imminent(info)
            elif scenario == "Ongoing":
                t+=self._wind_Conditions_Ongoing(info)
            elif scenario == "Diminishing":
                t+=self._wind_Conditions_Diminishing(info)
            t+=self._potentialImpact_stmt(info) + "\n"

        # PostEvent
        elif situation=="PostEvent":
            t+= self._wind_PostEvent(info, scenario) + "\n"

        # PostTropical
        elif situation=="PostTropical":
            if scenario=="InProgress":
                t+=self._wind_PostTropical_InProgress(info)
            elif scenario == "Completed":
                t+=self._wind_PostTropical_Completed(info)
            t+=self._potentialImpact_stmt(info) + "\n"

        if info.anyMarine:
            t+=self._frame("Add Wording for Seas Here") + "\n"

        return title + t

    #####################################################################################
    def _optionalSection_template(self, argDict, segment, info, hazardList, listenList=[],
                                  checkAreaTypes=[]):
        t=""
        if hazardList != []:
            try:
                hazardHdlns = self._hazardHdlns
            except:
                self._hazardHdlns, self._huAreas = self._getAdditionalHazards(info)
            segmentNum, segmentAreas, situation, scenario, sections, extraInfo = segment
            hdlnList = self._checkHazard(
                self._hazardHdlns, hazardList, checkAreas=segmentAreas, returnList=True)
            if len(hdlnList) > 0:
                t+=self._headlines(info, hdlnList, self._entirePortions, ending="")
                t+="See latest forecast for latest information. "
                for listen in listenList:
                    t+=listen + "\n"
        t+=self._frame("Additional free edit area with relevant info here.") + "\n"
        t+=self._frame("Potential impact statement from impact library for specific hazard.")+ "\n"
        t+="\n"
        return t

    #####################################################################################
    def StormSurgeTide(self, title, argDict, segment, section, info):
#        hazards = [("CF","W"), ("CF","A"), ("CF","Y"), ("SU","W"),("SU","Y")]
#        listenList = []
#        t=self._optionalSection_template(argDict, segment, info, hazards, listenList,
#                                         checkAreaTypes=["coastal"])

        if info.surgeHtPlusTide is None:
            return title + self._frame("Enter surge text here")

        t= ""
        segmentNum, segmentAreas, situation, scenario, sections, extraInfo = segment
        # NonEvent
        if situation=="NonEvent":
            pass    #    t+=self._surge_NonEvent(info)+ "\n" ??
        # PreEvent
        elif situation=="PreEvent":
            if scenario=="Advancing":
                t+=self._surge_PreEvent_Advancing(info)
            elif scenario=="Peripheral":
                t+=self._surge_PreEvent_Peripheral(info)
            else: # In Situ
                t+=self._surge_PreEvent_InSitu(info)
            t+="\n"

        # Abbreviated
        elif situation=="Abbreviated":
                pass

        # Watch
        elif situation=="Watch":
            if scenario=="Advancing":
                t+=self._surge_Watch_Advancing(info)
            elif scenario=="Peripheral":
                t+=self._surge_Watch_Peripheral(info)
            else: # In Situ
                t+=self._surge_Watch_InSitu(info)
            if info.surgeHtPlusTide > 0 and scenario != "InSitu":
                t+= self._surge_Watch_Impact_stmt(info, segment)
            t+= "\n"

        # Warning
        elif situation=="Warning":
            if scenario=="Advancing":
                t+=self._surge_Warning_Advancing(info)
            elif scenario=="Peripheral":
                t+=self._surge_Warning_Peripheral(info)
            else: # In Situ
                t+=self._surge_Warning_InSitu(info)
            if info.surgeHtPlusTide > 0 and scenario != "InSitu":
                t+=self._surge_Impact_stmt(info, segment)
            t+= "\n"

        # Conditions
        elif situation=="Conditions":
            if scenario=="Imminent":
                t+=self._surge_Conditions_Imminent(info)
            elif scenario == "Ongoing":
                t+=self._surge_Conditions_Ongoing(info)
            elif scenario == "Diminishing":
                t+=self._surge_Conditions_Diminishing(info)
            if info.surgeHtPlusTide > 0 and scenario != "Diminishing":
                t+=self._surge_Impact_stmt(info, segment)
            t+="\n"

        # PostEvent
        elif situation=="PostEvent":
            t+= self._surge_PostEvent(info, scenario)
            t+= "\n"

        # PostTropical
        elif situation=="PostTropical":
            if scenario=="InProgress":
                t+=self._surge_PostTropical_InProgress(info)
            elif scenario == "Completed":
                t+=self._surge_PostTropical_Completed(info)
            if info.surgeHtPlusTide > 0:
                t+=self._surge_Impact_stmt(info, segment)
            t+= "\n"

        return title + t

    #####################################################################################
    def InlandFlooding(self, title, argDict, segment, section, info):
        hazards = [("FF", "A"), ("FA","A")]
        listenList = [
            "Listen for possible flood warnings for your location...and be ready to act if flooding rains occur. "
            ]
        t=self._optionalSection_template(argDict, segment, info, hazards, listenList,
                                         checkAreaTypes=["land"])
        return title + t

    #####################################################################################
    def Tornadoes(self, title, argDict, segment, section, info):
        hazards = [("TO", "A")]
        listenList = [
            "Listen for possible Tornado Warnings for your location...and be ready to act quickly if a tornado approaches. "
            ]
        t=self._optionalSection_template(argDict, segment, info, hazards, listenList)
        return title + t

    #####################################################################################
    def Marine(self, title, argDict, segment, section, info):
        hazards = [('SR','W'), ('SR','A'), ('GL','W'), ('GL','A'), ('RB','Y'),
                   ('SC','Y'), ('SI','Y'), ('SW','Y'), ('HF','W'), ('HF','A')]
        listenList = []
        t=self._optionalSection_template(argDict, segment, info, hazards, listenList,
                                         checkAreaTypes=["marine"])
        return title + t

    #####################################################################################
    def _extractTitle(self, info, title):
        #  Extract correct title for Public vs. Marine segments
        if type(title) is types.TupleType:
            if info.anyMarine: title = title[1]
            else: title = title[0]
        return title


    #####################################################################################
    ## Precautionary Preparedness Statement Dictionaries
    ##
    ## To keep from cluttering the code, the text is in these dictionaries
    ## That way, the code logic can be more easily seen

    def _pp_dict(self, situation, keys):
         exec "textDict = self._" + situation + "_textDict()"
         return self._accessDict(textDict, keys)

    def _NonEvent_textDict(self):
        return {
            "ActiveNonEvent":{
                "land":self._frame("Take advantage of this opportunity to review your hurricane disaster plan. If you do not have a plan...make one. If you need assistance with your plan...contact the National Weather Service...local emergency management...or American Red Cross.\n\nStore adequate food and drink supplies for each member of the family for at least three to five days. Replace batteries in flashlights and portable radios. Fix loose and clogged rain gutters and downspouts. Trim overhanging trees and shrubbery. Also...acquire plywood or other materials to protect your home or business. Review your insurance policy...updating it if necessary.")+"\n",
                "marine":self._frame("Boat owners and captains of small craft should take this opportunity to review their emergency operations plan for tropical cyclones and evaluate their state of readiness for this season.")+"\n",
                },
            "EndNonEvent":{
                "everywhere":self._frame("THIS EVENT IS NO LONGER EXPECTED TO HAVE AN IMPACT ACROSS THE AREA AT THIS TIME.\n\nadd other wording here.")+"\n",
                }
             }

    def _PreEvent_textDict(self):
        return {
            "Advancing": {
                "land":self._frame("Everyone is strongly urged to stay informed. If early evacuation orders are issued for your area...stay calm and take the necessary steps to leave as soon as possible and in an orderly fashion.\n\nMake plans to evacuate if you live on the immediate coast and barrier islands...or in a high rise building...or in a mobile home...or in a place that floods easily. Be ready to act if a watch or warning is issued for your area.") + "\n",
                "marine":self._frame("As soon as possible...small craft are urged to return to port or to seek safe harbor. Take early steps to secure your craft.") + "\n",
                },
            "Peripheral": {
                "land":self._frame("Stay informed and listen for changes to the forecast. Be ready to act if watches or warnings become necessary for your area.")+"\n",
                "marine":self._frame("Small craft should consider returning to port or seeking safe harbor.")+"\n",
                },
            "InSitu": {
                "land":self._frame("Stay informed of the latest forecast. Do not get caught off guard and be ready to act quickly if watches or warnings become necessary for your area.")+"\n",
                "marine":self._frame("As soon as possible...small craft are urged to return to port or to seek safe harbor. Take early steps to secure your craft.")+"\n",
                }
             }

    def _Abbreviated_textDict(self):
        return {
            "HU_A": {
                "land":self._frame("Now is the time to begin implementing your hurricane disaster plan. Additional recommendations for your area will be offered shortly.") + "\n",
                "marine":self._frame("For marine interests...implement actions according to your emergency operations plan for possible hurricane conditions.") + "\n",
                },
            "TR_A": {
                "land":self._frame("This is a good time to begin implementing your disaster plan for possible tropical storm conditions. Additional recommendations for your area will be offered shortly.") + "\n",
                "marine":self._frame("For marine interests...implement actions according to your emergency operations plan for possible tropical storm conditions.")+"\n",
                },
            "HU_W": {
                "land":self._frame("According to your hurricane disaster plan...preparations to protect life and property should be nearing completion. Additional recommendations for your area will be offered shortly.")+"\n",
                "marine":self._frame("For marine interests...urgently complete actions according to your emergency operations plan for hurricane conditions.") + "\n",
                },
            "TR_W": {
                "land":self._frame("According to your disaster plan for tropical storm conditions...preparations to protect life and property should be nearing completion. Additional recommendations for your area will be offered shortly.")+"\n",
                "marine":self._frame("For marine interests...urgently complete actions according to your emergency operations plan for tropical storm conditions.")+"\n",
                },
            "TR_W_HU_A": {
                "land":self._frame("Diligently complete actions according to your hurricane disaster plan for tropical storm warnings. Be ready to implement your plan for hurricane warnings should this warning be upgraded in the future. Additional recommendations for your area will be offered shortly.")+"\n",
                "marine":self._frame("For marine interests...urgently complete actions according to your mariners emergency operations plan for tropical storm warnings...but also be ready to implment your plan for hurricane warnings should this warning be upgraded in the future.")+"\n",
                }
            }

    def _Watch_textDict(self):
        return {
            "HU_A": {
                "Advancing": {
                    "land":self._frame("Stay calm and keep informed. Comply with any evacuation orders that are issued for your area. If your home is vulnerable to high winds...or you live in a surge zone or any location prone to flooding...evacuate to a designated shelter or ride out the storm in the sturdy home of family or friends outside of evacuation zones.\n\nItems to bring to a shelter include a first aid kit...medicines and prescriptions...baby food and diapers...games and books...toiletries...a battery powered radio...a cell phone...flashlights with extra batteries...a blanket or sleeping bag for each person...personal identification...copies of key papers such as insurance policies...available cash and credit cards. Remember...pets are not allowed in most public shelters...so check ahead with your intended shelter.\n\nRegarding your home or business...cover all windows and doors with shutters or plywood. Move patio furniture and other loose objects indoors. Brace all exterior doors...including garage doors. Do this as early as possible.\n\nIf you need to make a trip to the hardware store...the grocery store...or the gas station...do so as early as possible.")+"\n",
                    "marine": self._frame("Boat owners and captains of small craft need to determine the best strategy for securing their craft.")+"\n",
                    },
                "Peripheral": {
                    "land":self._frame("Keep informed and listen for possible changes to the forecast. Comply with any evacuation orders issued for your area. If you live in a mobile home...make plans to evacuate.\n\nGather clothes...important papers...medicines...and small valuables and keep them ready to go on short notice. Gas up your vehicles and have extra cash on hand.\n\nRegarding your home or business...cover all windows and doors with shutters or plywood. Move patio furniture and other loose objects indoors. Brace all exterior doors...including garage doors.")+ "\n",
                    "marine":self._frame("Boat owners and captains of small craft need to determine the best strategy for securing their craft.")+"\n",
                    },
                "InSitu": {
                    "land":self._frame("Do not get caught unprepared as conditions are subject to change rapidly. The potential impacts are simply too great to ignore the threat. Err on the side of caution and take appropriate actions for possible hurricane conditions.")+"\n",
                    "marine":self._frame("Boat owners and captains of small craft should not allow themselves to get caught unprepared. Err on the side of caution and take protective actions. Determine the best strategy for securing their craft.")+"\n",
                    },
                },
            "TR_A": {
                "Advancing": {
                    "land": self._frame("Preparations should be made as soon as possible...before conditions deteriorate. Keep informed while listening for possible warnings. Secure loose outdoor objects which can be blown around. Strongly consider evacuating if you live in a mobile home...and do so if ordered by local officials.") + "\n",
                    "marine":self._frame("Boat owners and captains of small craft need to determine the best strategy for securing their craft.") + "\n",
                    },
                "Peripheral":{
                    "land": self._frame("Stay informed and listen for possible changes to the forecast. Preparations for this storm should be made as soon as possible.") + "\n",
                    "marine": self._frame("Boat owners and captains of small craft need to determine the best strategy for securing their craft.") + "\n",
                    },
                "InSitu":{
                    "land": self._frame("Do not get caught unprepared. Err on the side of caution and take appropriate actions for possible tropical storm conditions.") + "\n",
                    "marine": self._frame("Boat owners and captains of small craft should not allow themselves to get caught unprepared. Err on the side of caution and take protective actions. Determine the best strategy for securing their craft.") + "\n",
                    },
                },
            }

    def _Warning_textDict(self):
        return {
            "HU_W": {
                "Advancing": {
                    "land":self._frame("Make the final preparations to protect life and property. Rush to completion the hardening of your home or business by closing shutters and bracing garage doors.\n\nIf evacuating...leave as soon as possible. Guard against being stuck out on roadways when dangerous winds and heavy rains arrive. Again...do not stay in a mobile or manufactured home. Remember...pets are not allowed in most official shelters...so check ahead with your intended shelter.\n\nIf staying in a home...turn the refrigerator to maximum cold and keep it closed. Turn off propane tanks and unplug small appliances. Fill the bathtub with water in case the tap water becomes unavailable after the storm. This is for cleaning and flushing purposes. Do not drink it.") +"\n",
                    "marine": self._frame("Boat owners and captains of small craft should rush to completion the securing of their craft.") +"\n",
                    },
                "Peripheral": {
                    "land":self._frame("Make preparations to protect life and property. Complete the hardening of your home or business by closing shutters and bracing garage doors.\n\nIf evacuating...leave as soon as possible. Guard against being stuck out on roadways when dangerous winds and heavy rains arrive. Again...do not stay in a mobile or manufactured home. Remember...pets are not allowed in most official shelters...so check ahead with your intended shelter.") +"\n",
                    "marine":self._frame("Boat owners and captains of small craft should rush to completion the securing of their craft.") +"\n",
                    },
                "InSitu": {
                    "land":self._frame("This is a dangerous and rapidly developing situation. Err on the side of caution and urgently take actions to protect life and property. Comply with any evacuation orders issued by local authorities for your area. If you live in a mobile home...leave it for more substantial shelter.") +"\n",
                    "marine":self._frame("Boat owners and captains of small craft should rush to completion the securing of their craft.") +"\n",
                    },
                },
            "TR_W": {
                "Advancing": {
                    "land": self._frame("Final preparations to protect life and property should be completed before conditions deteriorate. The onset of gusty winds and heavy rains can cause outside activities to become dangerous. Secure loose outdoor objects which can be blown around. If you live in a mobile home...leave it for more substantial shelter.") +"\n",
                    "marine":self._frame("Boat owners and captains of small craft should rush to completion the securing of their craft.") +"\n",
                    },
                "Peripheral":{
                    "land": self._frame("Outside preparations should be completed as soon as possible before the onset of gusty winds and heavy rains which can cause outside activities to become dangerous.") +"\n",
                    "marine": self._frame("Boat owners and captains of small craft should rush to completion the securing of their craft.") +"\n",
                    },
                "InSitu":{
                    "land": self._frame("This is a potentially dangerous and rapidly developing situation. Err on the side of caution and complete preparations for tropical storm conditions.") +"\n",
                    "marine": self._frame("Boat owners and captains of small craft should rush to completion the securing of their craft.") +"\n",
                    },
                },
            "TR_W_HU_A": {
                "Advancing": {
                    "land":self._frame("Final actions to protect life and property should be completed before conditions deteriorate. Cover windows and doors with shutters or plywood. Move patio furniture and other loose outdoor objects inside. Brace all exterior doors...including garage doors.\n\nComply with any evacuation orders issued for your area. If you live in a mobile home...leave it for more substantial shelter. If your home is vulnerable to high winds...or you live in a surge zone or any location prone to flooding...evacuate to a designated shelter or ride out the storm in the sturdy home of family or friends outside of evacuation zones.") +"\n",
                    "marine":self._frame("Boat owners and captains of small craft should rush to completion the securing of their craft.") +"\n",
                    },
                "Peripheral":{
                    "land": self._frame("Preparations to protect life and property should be completed as soon as possible since the onset of gusty winds and heavy rains can cause outside activities to become dangerous. Cover windows and doors with shutters or plywood. Move patio furniture and other loose outdoor objects inside. Brace all exterior doors...including garage doors.\n\nComply with any evacuation orders issued for your area. If you live in a mobile home...leave it for more substantial shelter.") +"\n",
                    "marine": self._frame("Boat owners and captains of small craft should rush to completion the securing of their craft.") +"\n",
                    },
                "InSitu":{
                    "land": self._frame("This is a potentially dangerous and rapidly developing situation. Do not get caught unprepared. Err on the side of caution and complete preparations for tropical storm conditions and possible hurricane conditions.") +"\n",
                    "marine": self._frame("Boat owners and captains of small craft should rush to completion the securing of their craft.") +"\n",
                    },
                },
            }

    def _Conditions_textDict(self):
        return {
            "Imminent": {
                "Cat3": {
                    "land": self._frame("Very dangerous conditions will soon occur. Move to an interior room on the lowest floor of your home or shelter...and stay away from windows and external doors. Listen for extreme wind warnings which are issued for the imminent onset of extreme winds greater than 115 mph. If issued...act quickly to take that final step to protect yourself and others...and possibly save lives.")+"\n",
                    "marine":self._frame("Small craft should already be in port and well secured. Crews should be inside land based shelters. Do not attempt to ride out this storm on your vessel.")+"\n",
                    },
                "Cat1": {
                    "land":self._frame("Dangerous hurricane conditions will soon occur. Everyone should be quickly moving to safety within their home or shelter. Once inside...ensure all windows and doors are secured before dangerous winds arrive. Move to an interior room on the lowest floor.\n\nDo not go outside into the eye of hurricanes. Within the eye...conditions can become temporarily calm...which can be misleading. Once the eye passes...the winds will change direction and quickly increase again to dangerous speeds.\n\nBe aware that the loss of commercial power can happen quickly. Keep emergency gear handy.") + "\n",
                    "marine":self._frame("Small craft should already be in port and well secured. Captains of small craft and their crews should already be safely within land based shelters. Do not attempt to ride out this storm on your vessel.")+"\n",
                    },
                "34": {
                    "land":self._frame("Tropical storm conditions will soon occur. All evacuees should quickly arrive to their designated shelter. Everyone should remain alert and move inside.\n\nListen for possible flood or tornado warnings.") + "\n",
                    "marine":self._frame("Small craft should already be in port and well secured. Captains of small craft and their crews should already be safely within land based shelters.")+"\n",
                    },
                },

            "Ongoing": {
                "Cat3": {
                    "land":self._frame("Very dangerous conditions are occurring now. Go to the safest place within your home or shelter and stay there. Be ready to protect your head and body in case your shelter fails.") + "\n",
                    "marine":self._frame("Small craft should be in port and well secured. Crews should be inside land based shelters. Do not attempt to ride out this storm on your vessel.")+"\n",
                    },
                "Cat1": {
                    "land":self._frame("Dangerous hurricane conditions are occurring now. Remain in an interior room on the lowest floor. Stay away from windows and external doors. Keep emergency gear handy.") + "\n",
                    "marine":self._frame("Small craft should be in port and well secured. Crews should be inside land based shelters. Do not attempt to ride out this storm on your vessel.")+"\n",
                    },
                "34": {
                    "land":self._frame("Tropical storm conditions are occurring. Remain alert and stay inside.\n\nListen for possible flood or tornado warnings.") + "\n",
                    "marine":self._frame("Small craft should be in port and well secured.")+"\n",
                    },
                },
            "Diminishing": {
                "land":self._frame("As {desc} conditions diminish...do not go outside to check for damage or to implement temporary repairs as the wind situation will remain {somewhat} dangerous until high winds fully subside. Do not open the doors of your home or shelter. Wait for the all-clear signal.\n\nStay inside and listen for possible flood and tornado warnings.")+"\n",
                "marine":self._frame("Small craft should stay in port and remain well secured.")+"\n",
                },
            }

    def _PostEvent_textDict(self):
        return {
            "Immediate": {
                "land":self._frame("If you or someone else needs emergency help...call 9 1 1.\n\nAs soon as you are able...check in with your points of contact among family and friends. Inform them of your status and condition. Be a good samaritan and check in on your neighbors.\n\nListen to NOAA weather radio and other local news media for the latest information on storm impacts.\n\nIf you are using a portable generator...observe all safety precautions to avoid carbon monoxide poisoning...electrocution...or fires. Portable generators should be operated outdoors...in a dry and well ventilated place. Do not store fuel inside your home or garage.\n\nIf you received roof damage...do not go up on the roof until the threat of gusty winds and heavy rain has fully subsided. If operating chain saws and portable generators...review the operators manual and observe all safety precautions.\n\nStay out of flooded areas as the water may be contaminated or the road might have been washed away. Test drinking water before using...particularly from wells. Stay away from downed power lines too.")+"\n",
                "marine":self._frame("Small craft should remain in port or safe harbor until winds and seas subside. For any small craft who are in distress...or if you see someone else in distress...radio your situation according to maritime protocol. If appropriate...deploy your emergency distress beacon.")+"\n",
                },
            "NoImpact": {
                "general": self._frame("This event is no longer expected to have an impact across the area at this time. Use the opportunity to revise preparedness plans and remain prepared for future events.\n\nAdd other wrap-up wording here.")+"\n",
                },
            "LongTerm": {
                "land": self._frame("Continue to listen to NOAA weather radio and other local news media for the latest information on storm impacts.\n\nIf you are using a portable generator...observe all safety precautions to avoid carbon monoxide poisoning...electrocution...or fires. Portable generators should be operated outdoors...in a dry and well ventilated place. Do not store fuel inside your home or garage.\n\nChain saws can be very helpful when removing fallen trees and large branches. Yet...operating a chain saw is dangerous work. Be sure to review operating procedures for safe cutting. To reduce the chance of mishap or injury...work with another person who has experience.\n\nDo not go sight seeing into areas which have been hardest hit as you may hinder ongoing rescue and recovery operations.\n\nStay out of flooded areas as the water may be contaminated or the road might have been washed away. Test drinking water before using...particularly from wells. Stay away from downed power lines too.")+"\n\n",
                "marine": self._frame("Small craft should ensure that winds and seas have fully subsided before venturing out.")+"\n\n",
                "general": self._frame("For the latest information regarding the threat of hazardous weather of any type...listen to NOAA weather radio or visit your local National Weather Service web site.")+"\n",
                },
            }

    def _PostTropical_textDict(self):
        return {
            "InProgress": self._frame(
"""
Although the system is losing its tropical characteristics...the
potential impacts are similar to those previously indicated
regardless of its nature. Continue with readiness actions as
recommended."""),
            "Completed": self._frame(
"""
Although the system has become non-tropical...the potential
impacts are similar to those previously indicated. Continue with
readiness actions as recommended."""),
            }

    #####################################################################################
    ## Wind Situation/Scenario methods

    ############

    def _wind_NonEvent(self, info):
        t=""
        t+=self._frame("Tropical cyclone watches or warnings are currently not in effect...nor are they likely under present circumstances.\n\nThe latest forecast is for maximum winds to remain below tropical storm force. At this time...remain calm and stay informed.")
        return t
   ##############

    def _wind_PreEvent_Advancing(self, info):
        t=""
        t+="Tropical cyclone watches or warnings are likely to be issued in the near future. As " + self._stormTypeName + " moves closer...the threat for sustained high winds will likely increase. "
        t+=self._wind_stmt(info)+ ". "
        t+=self._beginWind_stmt(info.maxWind, 50, info.windDur[34], end=". ")
        return t

    def _wind_Watch_Advancing(self, info):
        t=""
        t+="AS "+self._stormTypeName+" moves closer...the threat for sustained high winds is likely to increase. "
        t+=self._wind_stmt(info)+". "
        t+=self._beginWind_stmt(info.maxWind, 50, info.windDur[34], end=". ")
        return t

    def _wind_Warning_Advancing(self, info):
        t=""
        if self._formatPeriod(info.windDur[34]) == "":
            t+="|* these zones are not within the 34kt radii. Rerun and choose peripheral. *|"
        else:
            t+="AS "+self._stormTypeName+" approaches...sustained tropical storm force winds are expected to begin "
            t+=self._formatPeriod(info.windDur[34])
            if info.maxWind >= 64:
                t+= " and hurricane force winds " + self._formatPeriod(info.windDur[64]) + ". "
                t+=self._specific_wind_stmt(info, intro="Hurricane force winds are forecast to last",
                                            duration=True, reportWindValues=False,
                                            windDur=info.windDur[64], end=". ")
            else:
                t+= ". "

        t+=self._specific_wind_stmt(info, intro="Maximum winds are forecast to be in the ",
                                    addRange=True)
        t+=". "
        return t

    ############
    def _wind_PreEvent_Peripheral(self,info):
        t= ""
        t+="At this time...the issuance of tropical cyclone watches or warnings is uncertain. As " + self._stormTypeName + " passes nearby...the threat for sustained high winds should not increase. However...some tropical storm force gusts may still occur. Since there is still uncertainty...closely monitor the forecast for any significant changes. "
        return t

    def _wind_Watch_Peripheral(self, info):
        t= ""
        t+="AS "+self._stormTypeName+" passes nearby...the threat for sustained high winds should not increase. However...there is still some possibility for tropical storm force winds. Since there is still uncertainty...closely monitor the forecast for any significant changes. "
        return t

    def _wind_Warning_Peripheral(self, info):
        t=""
        t+=self._specific_wind_stmt(info) + ". "
        t+="However...as "+self._stormTypeName+" approaches...stronger winds are still possible. Continue to closely monitor the forecast for any significant changes and be ready to act. "
        return t

    ############
    def _wind_PreEvent_InSitu(self, info):
        t=""
        t+="Tropical cyclone watches or warnings are currently not in effect for the area. However...if tropical cyclone development becomes likely then they could be quickly needed.\n\n"
        t+=self._wind_stmt(info)+ ". "
        t+="Since there is still uncertainty...closely monitor the forecast for any significant changes. "
        return t

    def _wind_Watch_InSitu(self, info):
        t=""
        t+="AS "+self._stormTypeName+" develops...the threat for sustained high winds may increase. Since there is still uncertainty...closely monitor the forecast for any significant changes. "
        return t

    def _wind_Warning_InSitu(self, info):
        t=""
        t+="AS "+self._stormTypeName+" continues to develop...the threat for sustained high winds may increase soon. "
        t+=self._specific_wind_stmt(info)+ ". "
        t+="Since there is still uncertainty...closely monitor the forecast for any significant changes. "
        return t

    ################
    def _wind_Conditions_Imminent(self, info):
        t=""
        if self._checkCategory(info.maxWind, "Cat3"):
            catInfo = self._getCategoryInfo(info.maxWind)
            t+="As the center of "+self._stormTypeName+" approaches..."+catInfo
            t+=" winds are imminent. "
            t+=self._specific_wind_stmt(
                info, intro="Maximum winds of ", end=" are expected. ")
            t+=self._fallBelow_stmt(info, end=". ")

        elif info.maxWind >= 34:
            catInfo = self._getCategoryInfo(info.maxWind)
            t+="AS "+self._stormTypeName+" approaches...sustained "+catInfo
            t+="Winds are imminent. "
            t+=self._specific_wind_stmt(
                info, intro="Maximum winds of ",end=" are expected. ")
            t+=self._fallBelow_stmt(info, end=". ")
        return t

    def _wind_Conditions_Ongoing(self, info):
        t=""
        period = info.windDur[info.maxWind]
        if self._checkCategory(info.maxWind, "Cat3"):
            catInfo = self._getCategoryInfo(info.maxWind)
            t+=self._windContinue_stmt(info, period, catInfo + "Will continue ", end=". ")
            if info.maxWind >= 50:  t+=self._fallBelow_stmt(info, end=". ")

        elif info.maxWind >= 34:
            t+=self._specific_wind_stmt(info, intro="Sustained winds of ")
            t+=self._windContinue_stmt(info, period, intro=" will continue ", end=". ")
            if info.maxWind>= 50:  t+=self._fallBelow_stmt(info, end=". ")
        return t

    def _wind_Conditions_Diminishing(self, info):
        t=""
        t+="AS "+self._stormTypeName+" exits the area...high winds will continue to diminish. Warnings will be discontinued as soon as the threat completely subsides. "
        return t

    ##############
    def _wind_PostEvent(self, info, scenario):
        t=""
        if scenario=="Immediate":
            t+="Tropical cyclone warnings have been discontinued. Sustained high winds are no longer expected but strong wind gusts may still occur. "
        else:
            t+="Sustained high winds or wind gusts are no longer expected. Please refer to the latest National Weather Service forecast for wind information. "
        return t

    ############
    def _wind_PostTropical_InProgress(self, info):
        t=""
        if info.anyLand:
            t+="The remnants of "+self._stormTypeName
            if info.maxWind >= 34:
                t+=" will still impact the region with sustained winds equivalent to "
                t+=self._windDesc(info) + "Winds. "
            else:
                t+=" could still impact the region with tropical storm force winds. "
            t+=self._specific_wind_stmt(info, intro="Maximum winds of ", end=" are expected. ")
            t+=self._fallBelow_stmt(info, end=". ")

        if not info.anyLand and info.anyMarine:
            t+="The remnants of "+self._stormTypeName
            if info.maxWind >= 34:
                t+=" will still impact the region with sustained winds equivalent to "
                t+=self._marineWindDesc(info) + "Winds. "
            else:
                t+=" could still impact the region with gale force winds. "
            t+=self._specific_wind_stmt(info, intro="Maximum winds of ", end=" are expected. ")
            t+=self._fallBelow_stmt(info, end=". ")
        return t

    def _wind_PostTropical_Completed(self, info):
        t=""
        if info.anyLand:
            t+="As the remnants of "+self._stormTypeName+" affect the area..."
            if info.maxWind >= 34:
                t+=" sustained winds equivalent to "
                windDesc = self._windDesc(info)
                t+=windDesc + "winds are still possible. "
            else:
                "Tropical storm force winds could still impact the region. "
            t+=self._specific_wind_stmt(info, intro="Maximum winds of ", end=" are expected. ")
            t+=self._fallBelow_stmt(info, end=". ")

        if not info.anyLand and info.anyMarine:
            t+="As the remnants of "+self._stormTypeName+" affect the area..."
            if info.maxWind >= 34:
                t+=" sustained winds equivalent to "
                windDesc = self._marineWindDesc(info) +" winds are still expected. "
            else:
                t+=" gale force winds could still impact the region. "
            t+=self._specific_wind_stmt(info, intro="Winds of ", end=" are expected")+ ". "
            t+=self._fallBelow_stmt(info, marine=True, end=". ")
        return t


    #####################################################################################
    ## Storm Surge and Storm Tide Situation/Scenario methods

    def _surge_PreEvent_Advancing(self, info):
        t = ""
        t += "It is too early to determine the exact heights of combined storm surge and tide waters for specific locations within the forecast area to be caused by """ + self._stormTypeName
        t += ". Much depends on the precise size...intensity and track of this system as it approaches the coast. At this time...there is a general concern for the chance of "
        t += self._frame("( minor | moderate | major -- you should base this on your MEOWS)") + " coastal flooding."
        return t

    def _surge_PreEvent_Peripheral(self, info):
        t = ""
        t+= "It is too early to determine the exact heights of combined storm surge and tide waters for specific locations within the forecast area to be caused by "
        t+= self._stormTypeName
        t+= ". Much depends on the precise size...intensity and track of this system as it passes nearby. At this time...there is a general concern for the chance of "
        t+= self._frame("( minor | moderate | major -- you should base this on your MEOWS)") + " coastal flooding."
        return t

    def _surge_PreEvent_InSitu(self, info):
        t = ""
        t+= "It is too early to determine the exact heights of combined storm surge and tide waters for specific locations within the forecast area to be caused by "
        t+= self._stormTypeName
        t+= ". Much depends on the precise size...intensity...and track of the system if it more fully develops. Since there is considerable uncertainty...closely monitor the latest forecast."

        return t

    ##############

    #  Changed 02/10/2011 (MHB) - Modified to not use "inundation up to 3 to 5 ft"
    #  terminology.  Will only use "up to" wording with inundation values < 2 ft.
    #  Otherwise will use "inundation of 3 to 5 ft" wording instead.  This
    #  change will impact all of the surge wording templates.

    def _surge_Watch_Advancing(self, info):
        t=""
        t+="It is still too early to determine the exact heights of combined storm surge and tide waters for specific locations within the forecast area to be caused by " + self._stormTypeName
        t+=". Much depends on the precise size...intensity and track of the system as it approaches the coast. "
        if info.surgeHtPlusTide > 0:
            t+="Given the latest forecast...there is a chance for combined storm surge and astronomical tide waters up to "
            t+=`info.surgeHtPlusTideWTopo` +" feet above mean sea level within areas closer to the coast..."
            t+="Resulting in worst case flood inundation "
            if info.surgeHtPlusTide > 2:
                t+=" OF " + `info.deltaSurge`+" TO "+`info.surgeHtPlusTide`+" feet above ground somewhere within the surge zone."
            else:
                t+="UP TO " + `info.surgeHtPlusTide`+" feet above ground somewhere within the surge zone."
        else:
            t+=self._frame("According to the latest surge grids, coastal flooding is likely to be negligible. Please further describe your coastal flooding concerns here or delete this paragraph or consider deleting the whole storm surge and tide section.")
        return t

    def _surge_Watch_Peripheral(self, info):
        t=""
        t+="It is still too early to determine the exact heights of combined storm surge and tide waters for specific locations within the forecast area to be caused by " + self._stormTypeName
        t+=". Much depends on the precise size...intensity and track of the system as it approaches the coast and passes nearby. "
        if info.surgeHtPlusTide > 0:
            t+="Given the latest forecast...there is a chance for combined storm surge and astronomical tide waters up to "
            t+=`info.surgeHtPlusTideWTopo` +" feet above mean sea level within areas closer to the coast..."
            t+="Resulting in worst case flood inundation "
            if info.surgeHtPlusTide > 2:
                t+="OF " +`info.deltaSurge`+" TO "+`info.surgeHtPlusTide`+" feet above ground somewhere within the surge zone."
            else:
                t+="UP TO "+`info.surgeHtPlusTide`+" feet above ground somewhere within the surge zone."
        else:
            t+=self._frame("According to the latest surge grids...coastal flooding is likely to be negligible. Please further describe your coastal flooding concerns here or delete this paragraph or consider deleting the whole storm surge and tide section.")
        return t


    def _surge_Watch_InSitu(self, info):
        t=""
        t+="It is too early to determine if there will be any appreciable coastal flooding within the forecast area from combined storm surge and tide waters associated with "
        t+=self._stormTypeName + ". Much depends on the precise size...intensity...and track of the system if it more fully develops. Since there is considerable uncertainty...closely monitor the latest forecast. "
        return t


    ################

    def _surge_Warning_Advancing(self, info):
        t = ""
        if info.surgeHtPlusTide > 0:

            t+="As "+self._stormTypeName+" approaches the coast...there is an increasing chance for combined storm surge and astronomical tide waters up to "
            t+=`info.surgeHtPlusTideWTopo`+" feet above mean sea level within areas closer to the coast...resulting "
            t+="In worst case flood inundation "

            if info.surgeHtPlusTide > 2:
                t+="OF " +`info.deltaSurge`+" TO "+`info.surgeHtPlusTide`+" feet above ground somewhere within the surge zone."
            else:
                t+="UP TO "+`info.surgeHtPlusTide`+" feet above ground somewhere within the surge zone."

            t+= "\n\nThe locations most likely to realize the greatest flooding include "
            t+=self._frame("Relative to the segment...explicitly list locations of greatest concern relative to inundation as that is what the impact statement below is based on for the worst affected area, include inland reach of the inundation waters. Further describe inundation elsewhere within the surge zone as applicable. Be aware that locations experiencing the highest storm surge and tide may not realize the greatest inundation. ")
            t+="The most likely period of impact will be "
            t+=self._frame("Be sure to cite the expected period of onset. Remember surge waters often arrive well before the core winds and can rise very quickly. ")

        else:
            t+="The impact from combined storm surge and tide waters is expected to be minimal. "
            t+=self._frame("According to the latest surge grids...coastal flooding is likely to be negligible. Please further describe your coastal flooding concerns here...leave this statement as is or delete the storm surge section all together. ")

        return t


    def _surge_Warning_Peripheral(self, info):
        t = ""
        if info.surgeHtPlusTide > 0:

            t+="Although the core of "+self._stormTypeName+" is not currently forecast to move across coastal sections of the forecast area at this time..."
            t+="There is still a chance for combined storm surge and astronomical tide waters up to "
            t+=`info.surgeHtPlusTideWTopo`+" feet above mean sea level within areas closer to the coast...resulting "
            t+="In worst case flood inundation "

            if info.surgeHtPlusTide > 2:
                t+="of "+`info.deltaSurge`+" to "+`info.surgeHtPlusTide`+" feet above ground somewhere within the surge zone."
            else:
                t+="up to "+`info.surgeHtPlusTide`+" feet above ground somewhere within the surge zone."

            t+= "\n\nThe locations most likely to realize the greatest flooding include "
            t+=self._frame("Relative to the segment...explicitly list locations of greatest concern relative to inundation as that is what the impact statement below is based on for the worst affected area, include inland reach of the inundation waters. Further describe inundation elsewhere within the surge zone as applicable. Be aware that locations experiencing the highest storm surge and tide may not realize the greatest inundation. ")
            t+="The most likely period of impact will be "
            t+=self._frame("Be sure to cite the expected period of onset. Remember surge waters often arrive well before the core winds and can rise very quickly. ")

        else:
            t+="The impact from combined storm surge and tide waters is expected to be minimal. "
            t+=self._frame("According to the latest surge grids...coastal flooding is likely to be negligible. Please further describe your coastal flooding concerns here...leave this statement as is or delete the storm surge section all together. ")

        return t

    def _surge_Warning_InSitu(self, info):
        t=""
        t+="As "+self._stormTypeName+" continues to develop...combined storm surge and tide waters may increase suddenly. Since there is considerable uncertainty...continue to closely monitor the latest forecast. "
        t+="At this time...there is a general concern for the chance of "
        t+=self._frame("(minor| moderate| major)")+ " coastal flooding. "
        return t

    ###############

    def _surge_Conditions_Imminent(self, info):
        t = ""
        if info.surgeHtPlusTide > 0:
            t+="With the imminent arrival of "+self._stormTypeName+"...combined storm surge and "
            t+="astronomical tide waters up to "+`info.surgeHtPlusTideWTopo`+" feet above mean sea "
            t+="level within areas closer to the coast will result in worst case flood inundation "

            if info.surgeHtPlusTide > 2:
                t+="OF "+`info.deltaSurge`+" TO "+`info.surgeHtPlusTide`+" feet above ground somewhere within parts of the surge zone."
            else:
                t+="UP TO "+`info.surgeHtPlusTide`+" feet above ground somewhere within parts of the surge zone."

            t+="\n\nThe locations most likely to realize the greatest flooding include "
            t+=self._frame("Relative to the segment...explicitly list locations of greatest concern relative to inundation as that is what the impact statement below is based on for the worst affected area, include inland reach of the inundation waters; further describe inundation elsewhere within the surge zone as applicable; be aware that locations experiencing the highest storm surge and tide may not realize the greatest inundation. Also stress the rapid water rises that are likely. ")
        else:
            t+="The impact from combined storm surge and tide waters is expected to be minimal. "
            t+=self._frame("According to the latest surge grids, coastal flooding is negligible. Please further describe your coastal flooding concerns here, leave this statement as is or delete the storm surge section all together.")
        return t

    def _surge_Conditions_Ongoing(self, info):
        t = ""
        if info.surgeHtPlusTide > 0:
            t+="Combined storm surge and astronomical tide waters up to "+`info.surgeHtPlusTideWTopo`
            t+=" feet above mean sea level are likely being realized within areas closer to the coast"
            t+=" and resulting in worst case flood inundation "

            if info.surgeHtPlusTide > 2:
                t+="OF "+`info.deltaSurge`+" TO "+`info.surgeHtPlusTide`+" feet above ground somewhere within parts of the surge zone."
            else:
                t+="UP TO "+`info.surgeHtPlusTide`+" feet above ground somewhere within parts of the surge zone."

            t+="\n\nThe locations most likely realizing the greatest flooding include "
            t+=self._frame("Relative to the segment...explicitly list locations of greatest concern relative to inundation as that is what the impact statement below is based on for the worst affected area, include inland reach of the inundation waters; further describe inundation elsewhere within the surge zone as applicable; be aware that locations experiencing the highest storm surge and tide may not realize the greatest inundation.")
        else:
            t+= "Minimal storm tide impacts are being observed. "
            t+=self._frame("According to the latest surge grids...coastal flooding is negligible. Please further describe your coastal flooding concerns here, leave this statement as is or delete the storm surge section all together.")
        return t


    def _surge_Conditions_Diminishing(self, info):
        t = ""
        if info.surgeHtPlusTide > 0:
            t+="Although coastal flood waters will soon begin to partially recede..."
            t+="do not attempt to return to evacuated areas until official confirmation is "
            t+="received that it is safe to do so. "
            t+="\n\nCombined storm surge and astronomical tide waters up to "+`info.surgeHtPlusTideWTopo` + " "
            t+=self._frame("This is one case where more deterministic values can be drawn from slosh or other sources.")
            t+=" feet above mean sea level were likely realized within areas closer to the coast resulting "
            t+="in worst case flood inundation "

            if info.surgeHtPlusTide > 2:
                t+="of "+`info.deltaSurge`+" to "+`info.surgeHtPlusTide`
            else:
                t+="up to "+`info.surgeHtPlusTide`

            t+=self._frame("Same comment here as above but relative to inundation.")
            t+=" feet above ground. "
        else:
            t+="Minimal storm tide impacts are being observed. "
            t+=self._frame("According to the latest surge grids...coastal flooding is negligible. Please further describe your coastal flooding concerns here, leave this statement as is or delete the storm surge section all together.")
        return t


    #############
    def _surge_PostEvent(self, info, scenario):
        t = ""
        if scenario == "Immediate":
            if info.surgeHtPlusTide > 0:
                t+="As wind conditions associated with "+self._stormTypeName+" continue to improve...coastal flood waters will be slower to recede. Certain areas may still be inundated. Do not attempt to return to evacuated areas until official confirmation is received that it is safe to do so."
                t+="\n\nThe locations which realized the greatest flooding include "
                t+=self._frame("Relative to the segment...explicitly list locations that experienced greatest inundation flooding remember that in the absence of tidal or other kind of observations the real time slosh run at the time of landfall is likely your best source of information here, not necessarily the psurge data; further describe inundation elsehwere within the surge zone as applicable; describe any known impacts.")
            else:
                t+="Minimal storm tide impacts are being observed. "
                t+=self._frame("According to the latest surge grids...coastal flooding is negligible. Please further describe your coastal flooding concerns here, leave this statement as is or delete the storm surge section all together.")
        return t

    #############
    def _surge_PostTropical_InProgress(self, info):
        t = ""
        if info.surgeHtPlusTide > 0:
            t+="As "+self._stormTypeName+" impacts the forecast area...combined storm surge and astronomical tide waters up to "
            t+=`info.surgeHtPlusTideWTopo`
            t+=" feet above mean sea level within areas closer to the coast will likely result in worst case flood inundation "
            if info.surgeHtPlusTide > 2:
                t+="of "+`info.deltaSurge`+" to "+`info.surgeHtPlusTide`
            else:
                t+="up to "+`info.surgeHtPlusTide`
            t+=" feet above ground somewhere within the surge zone. "
            t+="\n\nThe locations which will likely realize the greatest flooding include "
            t+=self._frame("Relative to the segment...explicitly list locations of greatest inundation concerns, including inland reach; further describe inundation elsewhere within the surge zone as applicable.")
        else:
            t+="The impact from combined storm surge and tide waters is expected to be minimal. "
            t+=self._frame("According to the latest surge grids...coastal flooding is likely to be negligible. Please further describe your coastal flooding concerns here, leave this statement as is or delete the storm surge section all together.")
        return t

    def _surge_PostTropical_Completed(self, info):
        t = ""
        if info.surgeHtPlusTide > 0:
            t+="As former "+self._stormTypeName+" impacts the forecast area...combined storm surge and astronomical tide waters up to "
            t+=`info.surgeHtPlusTideWTopo`+" feet above mean sea level within areas closer to the coast will likely result in worst case flood inundation "
            if info.surgeHtPlusTide > 2:
                t+="of "+`info.deltaSurge`+" to "+`info.surgeHtPlusTide`
            else:
                t+="up to "+`info.surgeHtPlusTide`
            t+=" feet above ground somewhere within the surge zone."
            t+="\n\nThe locations which will likely realize the greatest flooding include "
            t+=self._frame("Relative to the segment...explicitly list locations of greatest inundation concerns, including inland reach; further describe inundation elsewhere within the surge zone as applicable.")
        else:
            t+="The impact from combined storm surge and tide waters is expected to be minimal. "
            t+=self._frame("According to the latest surge grids...coastal flooding is likely to be negligible. Please further describe your coastal flooding concerns here, leave this statement as is or delete the storm surge section all together.")
        return t

    ##############
    ####### Total Water Level thresholds and statements
    #######  NOTE: Thresholds are being compared to the SurgeHtPlusTide values
    ############## Impact Statements

    def _surge_Watch_Impact_stmt(self, info, segment):
        t=""
        water_dict = self._totalWaterLevel_dict(info, segment)
        if info.surgeHtPlusTide  >= water_dict.get("Extreme", 7):
            damage="Widespread major"

        elif info.surgeHtPlusTide >= water_dict.get("High", 5):
            damage="Areas of major"

        elif info.surgeHtPlusTide  >= water_dict.get("Moderate", 3):
            damage="Areas of moderate"

        elif info.surgeHtPlusTide  >= water_dict.get("Low", 1):
            damage="Areas of minor"
        else:
            damage = None
        if damage is not None:
            t+="\n\n"+self._frame("At this time...there is a general concern" +
                                  " for the chance of "+ damage +
                                  " coastal flooding.")
        return t

    def _surge_Impact_stmt(self, info, segment):
        t=""
        water_dict = self._totalWaterLevel_dict(info, segment)
        if info.surgeHtPlusTide  >= water_dict.get("Extreme", 7):
            damage= self._totalWaterLevel_Extreme_stmt(info, segment)

        elif info.surgeHtPlusTide >= water_dict.get("High", 5):
            damage= self._totalWaterLevel_High_stmt(info, segment)

        elif info.surgeHtPlusTide  >= water_dict.get("Moderate", 3):
            damage= self._totalWaterLevel_Moderate_stmt(info, segment)

        elif info.surgeHtPlusTide  >= water_dict.get("Low", 1):
            damage= self._totalWaterLevel_Low_stmt(info, segment)
        else:
            damage ="Minor coastal flood damage"
        t+="\n\n"+self._frame(damage)
        return t

    def _totalWaterLevel_byZone_dict(self):
        # Enter customized values for land and marine zones
        return {
            "zone1": {
                "Extreme": 7,
                "High": 5,
                "Moderate": 3,
                "Low": 1,
                },
            "default": {
                "Extreme": 7,
                "High": 5,
                "Moderate": 3,
                "Low": 1,
                },
            }

    def _totalWaterLevel_dict(self, info, segment):
        # SurgeHtPlusTide thresholds for Total Water Level statements.
        #  The threshold values for the segment will be determined by
        #  examining the thresholds for each zone in the segment and choosing
        #  the values for the zone which has the *lowest* Extreme value
        segmentNum, segmentAreas, situation, scenario, sections, extraInfo = segment
        twl_dict = self._totalWaterLevel_byZone_dict()
        return_dict = None
        for zoneName in segmentAreas:
            zone_dict = twl_dict.get(zoneName, twl_dict["default"])
            if return_dict is None: return_dict = zone_dict
            elif zone_dict["Extreme"] < return_dict["Extreme"]:
                return_dict = zone_dict
        return return_dict

    def _totalWaterLevel_Extreme_stmt(self, info, segment):
        t = ""
        t+= """
There is an extreme threat to life and property from coastal
flooding...potentially having a catastrophic impact. The concern
is for the chance of widespread major coastal flooding to occur
within the surge zone...resulting in devastating and life-
threatening inundation. If realized...people within the
threatened areas who failed to heed official evacuation orders
will likely die.

Coastal communities will likely be devastated...with numerous
homes and businesses near the shore completely destroyed.
Shoreside condominiums and hotels may also be destroyed...
Especially those with inadequate structural support. Flood waters
are likely to extend well inland...further expanding the overall
threat to life and property. Vehicles of any type will likely be
submerged or swept away.

Roads and bridges will likely be damaged or washed out by the
combined effects of storm surge and tide waters...battering
waves...and floating debris. This could leave entire flood-prone
coastal communities cut off...perhaps for several weeks or
more...and with no power or water.
"""
        return t

    def _totalWaterLevel_High_stmt(self, info, segment):
        t = ""
        t+= """
There is a critical threat to life and property from coastal
flooding...potentially having a high impact. The concern is for
the chance of major coastal flooding to occur in areas within the
surge zone...resulting in very damaging and life-threatening
inundation. If realized...people within the threatened areas who
failed to heed official evacuation orders will have needlessly
placed their lives in grave danger and may be swept away.

Most coastal communities will likely be heavily damaged...with
many homes and businesses near the shore destroyed by battering
waves and floating debris. Some shoreside condominiums and hotels
may also be damaged...especially those with inadequate structural
support. Flood waters are likely to extend well inland...further
expanding the overall threat to life and property. Most vehicles
of any type will likely be submerged or swept away.

Severe beach erosion will occur. Most roads and some bridges will
likely be damaged or washed out...leaving entire flood-prone
coastal communities cut off...perhaps for a week or more...and
with no power or water.
"""
        return t

    def _totalWaterLevel_Moderate_stmt(self, info, segment):
        t = ""
        t+= """
There is a significant threat to life and property from coastal
flooding...potentially having a moderate impact. The concern is
for the chance of moderate coastal flooding to occur in areas
within the surge zone...resulting in damaging and
life-threatening inundation. If realized...people within the
threatened areas who failed to heed official evacuation orders
will have needlessly placed their lives in danger. This is
especially true for those staying behind in vulnerable locations
such as homes and businesses near the shore...and one story
dwellings in flood-prone areas.

Several coastal communities will likely be damaged...with those
structures not raised or protected by a seawall being subject to
significant flooding...especially during high tide. Large waves
and pounding surf will accentuate property damage in exposed
locations. Flood waters may extend well inland in spots. Many
cars will likely be submerged or washed away.

Substantial beach erosion will occur. Many roads will likely be
damaged or washed out by the flood waters...leaving sections of
coastal communities in flood prone areas temporarily cut off.
Roadway travel may be dangerous with several roads closed.
"""
        return t

    def _totalWaterLevel_Low_stmt(self, info, segment):
        t = ""
        t+= """
There is an elevated threat to life and property from coastal
flooding...potentially having a low but notable impact. The
concern is for the chance of minor coastal flooding to occur in
areas within the surge zone...resulting in shallow inundation. If
realized...people within the threatened areas who failed to act
according to their personal disaster plan will have needlessly
placed themselves at some measure of risk.

Many homes and businesses along the shoreline...or in flood-prone
areas...will likely experience some water entering inside...
Especially for those structures not raised or protected by a
seawall. Higher waves and pounding surf will increase the
likelihood of property damage near the coast...especially in
exposed locations. Some cars may take on water or even become
displaced.

Moderate beach erosion will occur...which may become substantial
if conditions extend through multiple high tides. Several roads
in flood-prone areas will likely be closed.
"""
        return t

    #####################################################################################
    ### Segment statements and thresholds -- these are the templates for phrasing
    ### I am calling them stmts to distinguish from the text product "phrases" that
    ### use the "tree, node" infrastructure.
    ### These stmts are simpler (at least at this point)...

    ## In general, "stmt" methods do not add periods or carriage returns
    ## It is up to the calling method to do so

    def _prob_stmts(self, info, ifWording=False):
        t=""
        probHurricane = self._probHurricane_stmt(info, end=". ")
        probTropStorm = self._probTropStorm_stmt(info, end=". ")
        if probHurricane != "" and probTropStorm != "":
            t+=probHurricane + "Also..." + probTropStorm
        else:
            t+= probHurricane
            t+= probTropStorm
        t+=self._probTrend_stmt(info, end=". ")
        if self._checkOnsetTime(info):
            t+=self._onsetTropStorm_stmt(info, ifWording, end=". ")
            t+=self._onsetHurricane_stmt(info, ifWording, end=". ")
        return t

    def _probHurricane_thresholds(self):
        return {
            "littleChance": 3,
            "chance": 6,
            "onset": 6, # To trigger onset statement
            }

    def _probTropStorm_thresholds(self):
        return {
            "littleChance":10,
            "chance": 20,
            "onset": 20,  # To trigger onset statement
            }

    def _prob_threshold(self):
        # If the difference between min and max probabilities are greater
        # than this, a range will not be reported.  For example,
        # Instead of "20 to 50 PERCENT", we would say "up to 50 percent"
        return 10

    def _probStorm_stmt(self, info, thresholds, minProb, maxProb,
                        conditions="Hurricane", end=""):
        t=""
        if minProb is None or maxProb is None:
            return t
        little = thresholds.get('littleChance', 3)
        chance = thresholds.get('chance', 6)
        minProb = int(minProb)
        maxProb = int(maxProb)
        if maxProb < little:
            t+="There is little chance for "+conditions+" conditions at this time"
        else:
            t+="The chance for "+conditions+" conditions at this time is "
            if maxProb > chance:
                if minProb < little:
                    t+="less than or equal to "+ `maxProb`
                elif minProb >= maxProb:
                    t+= `maxProb`
                elif maxProb-minProb <= self._prob_threshold():
                    t+= `minProb` + ' TO '+ `maxProb`
                else:
                    t+= 'UP TO ' + `maxProb`
                t+= " percent"
            else: t+="very small"
        return t + end

    def _probHurricane_stmt(self, info, end=""):
        thresholds = self._probHurricane_thresholds()
        return self._probStorm_stmt(
            info, self._probHurricane_thresholds(), info.minProb64, info.maxProb64,
            conditions="Hurricane", end=end)

    def _probTropStorm_stmt(self, info, end=""):
        thresholds = self._probTropStorm_thresholds()
        return self._probStorm_stmt(
            info, self._probTropStorm_thresholds(), info.minProb34, info.maxProb34,
            conditions="Tropical Storm", end=end)

    def _probTrend_stmt(self, info, end=""):
        t=""
        if info.pwstrend is None:
            t+=self._frame("This represents a general {upward/downward/steady} trend since the last forecast" + end)

        else:
            t+="This represents a general "
            if info.pwstrend > 3: t+= " upward"
            elif info.pwstrend > -3 and info.pwstrend < 3: t+= " steady"
            else: t+= " downward"
            t+=" trend since the last forecast" + end
        return t

    def _checkOnsetTime(self, info):
        # Check onset time.  If <= 24 hours from current time, return False
        if info.wind34Time is None:
            return False
        curTime = AbsTime.AbsTime(self._argDict["creationTime"])
        if info.wind34Time.startTime() <= curTime + 24*3600:
            return False
        return True

    def _onsetHurricane_stmt(self, info, ifWording=False, end=""):
        thresholds = self._probHurricane_thresholds()
        t=""
        if ifWording:
            condition = info.maxProb64 > thresholds.get('onset', 6)
        else:
            condition = info.maxWind >= 64
        if condition:
            if ifWording:
                t+="If hurricane conditions were to occur...the most likely period of onset is "
            else:
                #t+="The most likely period of onset of hurricane conditions is "
                t+="The onset of hurricane conditions could start as early as "
            t+=self._formatPeriod(info.maxINTprob64, resolution=6)
            t+=end
        return t

    def _onsetTropStorm_stmt(self, info, ifWording=False, end=""):
        thresholds = self._probTropStorm_thresholds()
        t=""
        if ifWording:
            condition = info.maxProb34 > thresholds.get('onset', 20)
        else:
            condition = info.maxWind >= 34

        if condition:
            if ifWording:
                t+="If tropical storm conditions were to occur...the most likely period of onset is "
            else:
                #t+="The most likely period of onset of tropical storm conditions is "
                t+="The onset of tropical storm conditions could start as early as "
            t+=self._formatPeriod(info.maxINTprob34, resolution=6)
            t+=end
        return t

    def _beginWind_stmt(self, value, threshold, timeRange, intro=None, end=""):
        t=""
        if value >= threshold:
            if intro is None:
                intro="Tropical storm force winds are currently forecast to begin affecting the area "
            t+=intro + self._formatPeriod(timeRange)
            t+=end
        return t

    def _windContinue_stmt(self, info, period, intro=None, end=""):
        t=""
        if intro is not None:  t+=intro
        else:                  t+="Winds will continue "
        if period is None: return t + end
        t+="through " + self._formatPeriod(period, useEndTime=True) + end
        return t

    def _fallBelow_stmt(self, info, intro=None, marine=False, end=""):
        t= ""
        if info.windDur[64] is None and info.windDur[34] is None: return t
        if intro is None: intro = "Winds are not forecast to fall below "
        hurricane = False
        t+=intro
        if info.maxWind >= 64 and info.windDur[64] is not None:
            t+="hurricane force until "+ self._formatPeriod(info.windDur[64], useEndTime=True)
            hurricane = True
        if info.windDur[34] is not None:
            if hurricane: t+="...and below "
            if marine: t+="gale force until "
            else: t+="tropical storm force until "
            t+=self._formatPeriod(info.windDur[34], useEndTime=True)
        t+=end
        return t

    def _formatPeriod(self, period, wholePeriod=False, shiftToLocal=True, useEndTime=False,
                      resolution=3):
        # Format period (a timeRange) resulting in
        #     DAY + MORNING / AFTERNOON / EVENING / OVERNIGHT.
        # If wholePeriod, format FROM ... TO...

        #print "\nFormat period", wholePeriod, period
        if period is None:  return ""
        if useEndTime:
            startTime = period.endTime()
        else:
            startTime = period.startTime()
        result = self._getTimeDesc(startTime, resolution, shiftToLocal)
        #print "result", result
        if wholePeriod:
            endResult = self._getTimeDesc(period.endTime(), resolution, shiftToLocal)
            #print "endResult", endResult
            if result != endResult:
                result=result + " TO "+ endResult
        return result

    def _getTimeDesc(self, startTime, resolution=3, shiftToLocal=True):
        # Create phrase such as Tuesday morning
        # Handle today/tonight and "this" morning/afternoon/etc..
        #
        print "\n\n**************Formatting Period for GMT starttime ", startTime
        labels = self.Labels()["SimpleWorded"]
        currentTime = self._issueTime
        print "   currentTime", currentTime
        if shiftToLocal:
            currentLocalTime, shift = self.determineTimeShift()
            startTime = startTime + shift
            currentTime = currentTime + shift
            print "  shift, shifted start, current", shift/3600, startTime, currentTime
        hour = startTime.hour
        prevDay = False
        prevDay, partOfDay = self._getPartOfDay(hour, resolution)
        if prevDay:
            startTime = startTime - 24*3600
        todayFlag = currentTime.day == startTime.day
        if todayFlag:
            if partOfDay.find("midnight")>0: todayWord = "Tonight"
            else: todayWord = "This"
            weekday = todayWord
        else:
            weekday = labels["Weekday"][startTime.weekday()]
        if partOfDay.find("<weekday>") >= 0:
            result = partOfDay.replace('<weekday>', weekday)
        else:
            result =  weekday + " " + partOfDay
        print "Result", result
        return result

    def _getPartOfDay(self, hour, resolution):
        prevDay = False
        if resolution == 3:
            if hour < 3:
                prevDay = True
                partOfDay = "After midnight"
            elif hour < 6:
                partOfDay = "early <weekday> morning"
            elif hour < 9:
                partOfDay = "morning"
            elif hour < 12:
                partOfDay = "late <weekday> morning"
            elif hour < 15:
                partOfDay = "early <weekday> afternoon"
            elif hour < 18:
                partOfDay = "late <weekday> afternoon"
            elif hour < 21:
                partOfDay = "early <weekday> evening"
            else:
                partOfDay = "late <weekday> evening"
        else:
            if hour < 6:
                prevDay = True
                partOfDay = "After midnight"
            elif hour < 12: partOfDay = "Morning"
            elif hour < 18: partOfDay =  "Afternoon"
            else: partOfDay = "Evening"
        return prevDay, partOfDay

    def _wind_stmt_type(self):
        # return "categorical"
        return "specific"

    def _wind_stmt(self, info, intro=None, units=None, withTiming=True):
        t=""
        if intro is None:
            intro="The latest forecast is for "
        t+=intro
        descriptor, duration = self._categorical_wind_info(info)
        t+= descriptor
        if withTiming and duration is not None:
            t+= " from " + self._formatPeriod(duration, wholePeriod=True)
        return t

    def _categorical_wind_info(self, info):
        t=""
        if info.maxWind >= 64:
            t+="Hurricane force winds"
            duration = info.windDur[64]
        elif info.maxWind >=50:
            t+="Strong tropical storm force winds"
            duration = info.windDur[50]
        elif info.maxWind >=34:
            t+="Tropical storm force winds"
            duration = info.windDur[34]
        else:
            t+="Winds to remain below tropical storm force"
            duration = None
        return t, duration

    def _specific_wind_stmt(self, info, units=None, intro=None, duration=False, windDur=None,
                            addRange=False, end=None, reportWindValues=True):
        t=""
        if info.maxWind is None: return t
        if intro is None:
            intro = "The latest area forecast is for maximum winds of "
        t+= intro

        if reportWindValues:
            t+=self._formatWindRange(info, info.maxWind, units, "Wind")
            if addRange: t+= " range"
            if info.maxGust is not None:
                t+=" with gusts to "
                t+=self._formatWindValue(info, info.maxGust, units, "WindGust")
        if windDur is None:
            windDur = info.windDur[info.maxWind]
        if duration and windDur is not None:
            t+= " for "
            duration = windDur.duration()/3600
            if duration <= 3: t+= "a few "
            elif duration <= 6: t+= "several "
            else: t+= "many "
            t+= "hours"
        if end is not None: t+=end
        return t

    def _formatWindValue(self, info, value, units=None, element="Wind"):
        if value is None: return ""
        if self._getUnits(info, units) == "mph":
            value = self._ktToMph(value, element)
            units = " mph"
        else:
            units = " knots"
        return `int(value)` + units

    def _getUnits(self, info, units=None):
        #{UNIT} = equal to MPH if public zone segment or KNOTS if marine segment.
        # If in Overview or a combined segment (Not possible this season) then default to MPH.
        # If called from overview, set units == "mph"
        if units is not None: return units
        if info.anyLand: return "mph"
        else: return "kts"

    def _formatWindRange(self, info, windKts, units, element):
        # Add a range to hiVal and report it
        if windKts is None: return ""
        units = self._getUnits(info, units)
        if units == "mph":
            hiVal = self._ktToMph(windKts, element)
            unitStr = " mph"
        else:
            hiVal = windKts
            unitStr = " knots"
        lowVal = self._windRange_value(windKts, hiVal)
        return `int(lowVal)` + " TO " + `int(hiVal)` + unitStr

    def _windRange_value(self, windKts, windValue):
        # Given windValue in kts, return the lower range value
        if windKts > 52:  return windValue - 20
        elif windKts > 34:  return windValue - 10
        return windKts - 5

    def _hurricaneWind_categories(self):
        # Dictionary representing wind thresholds in kts
        # for category 1, 2, 3, 4 or 5 hurricanes.
        return {
            'Cat1': (64, 83),
            'Cat2': (83, 96),
            'Cat3': (96, 114),
            'Cat4': (114, 136),
            'Cat5': (136, 250),
            }

    def _checkCategory(self, wind, category):
        minVal, maxVal = self._hurricaneWind_categories().get(category, (None, None))
        if wind >=minVal:
            return True
        return False

    def _getCategoryInfo(self, wind):
        catDict = self._hurricaneWind_categories()

        for key, label in [
            ("Cat5","Catastrophic category 5 hurricane force "),
            ("Cat4", "Destructive category 4 hurricane force "),
            ("Cat3","Very dangerous category 3 hurricane force "),
            ("Cat2", "Category 2 hurricane force "),
            ("Cat1", "Category 1 hurricane force "),
            ]:
            minVal, maxVal = catDict[key]
            if wind >= minVal:
                return label
        if wind >= 50:
            return "Strong tropical storm force "
        elif wind >= 34:
            return "Tropical storm force "
        return "Strong "

    def _getCategoryDamage(self, wind):
        # Convert from knots to mph
        wind_mph = self._ktToMph(wind, "Wind")
        if wind_mph > 130:
            return "Catastrophic damage"
        elif wind_mph > 110:
            return "Devastating damage"
        elif wind_mph > 90:
            return "At least extensive damage"
        elif wind_mph > 75:
            return "At least widespread damage"
        elif wind_mph > 60:
            return "At least damaging winds"
        elif wind_mph > 50:
            return "At least damaging winds likely"
        elif wind_mph > 40:
            return "At least minor to locally moderate damage"
        elif wind > 30:
            return "At least minor damage"
        else:
            return ""

##         catDict = self._hurricaneWind_categories()
##         for key, label in [
##             ("Cat5","Catastrophic damage"),
##             ("Cat4", "At least devastating damage"),
##             ("Cat3", "At least extensive damage"),
##             ("Cat2", "At least widespread damage"),
##             ("Cat1", "At least moderate damage"),
##             ]:
##             minVal, maxVal = catDict[key]
##             if wind >= minVal:
##                 return label
##         if wind >= 50:
##             return "At least minor to locally moderate"
##         elif wind >= 34:
##             return "At least minor damage"
##         return "Damage"

    def _windDesc(self, info):
        if info.maxWind >= 64:
            return "hurricane force "
        elif info.maxWind >= 50:
            return "strong tropical storm force "
        elif info.maxWind >= 34:
            return "tropical storm force "
        else:
            return "strong "

    def _marineWindDesc(self, info):
        if info.maxWind >= 64:
            return "hurricane force "
        elif info.maxWind >= 48:
            return "storm force "
        elif info.maxWind >= 34:
            return "gale force "
        else:
            return "strong "

    def _potentialImpact_thresholds(self):
        # Units are mph
        return {
            'noImpact': 30,
            'minor': 40,
            'moderate': 50,
            'damageLikely': 60,
            'damageExpected': 75,
            'danger': 90,
            'extremeDanger': 110,
            'devastating': 130,
            }

    def _potentialImpact_stmt(self, info):
        if info.allMarine:  # No impact statements for marine yet.
            return ""

        thresholds = self._potentialImpact_thresholds()
        t=""
        if info.maxWind is None: return t
        # Convert to mph -- use avg Wind value
        wind_mph = self._ktToMph(info.maxWind, "Wind")
        if wind_mph <= thresholds.get('noImpact', 30):
            return t
        t+="\n"
        if wind_mph <= thresholds.get('minor', 40):
            t+="Minor damage may occur to older mobile homes. Residents should move loose items indoors...such as garbage cans and outdoor furniture...as they will be blown around.  Newly planted or young trees and shrubs may be uprooted if not secured properly.  Isolated power outages will be possible.\n"
        elif wind_mph <= thresholds.get('moderate', 50):
            t+="Minor to moderate damage is likely to many mobile homes...especially those that have canopies...awnings...or carports.  Poorly constructed homes may sustain minor wall damage and partial roof removal.  Other homes may have minor roof and siding damage.  Some loose outdoor items will be tossed around and may cause additional damage.  A few power lines will be knocked down resulting in scattered power outages.  Some large branches of healthy trees will be snapped.  Most newly planted trees and shrubs will be damaged or uprooted.\n"
        elif wind_mph <= thresholds.get('damageLikely', 60):
            t+="Damaging winds are likely.  Most poorly anchored mobile homes will be damaged...some severely.   Other homes may have damage to shingles...siding...gutters and windows...especially if these items are not properly secured.  Loose outdoor items will become airborne...causing additional damage and possible injury.  Some power lines will be knocked down by falling trees...resulting in scattered power outages.  Many large branches of trees will be snapped...and a few trees will be uprooted.\n"
        elif wind_mph <= thresholds.get('damageExpected', 75):
            t+="Damaging winds are expected.  Poorly anchored mobile homes may be destroyed...along with those of old or poor construction.  Some well anchored mobile homes will have substantial damage to roofs...walls...and windows...and could become uninhabitable.  Some homes of frame construction will sustain partial wall and roof failure...and possibly blown out windows.  Loose outdoor items will become projectiles...causing additional damage and possible injury.  Many areas will experience power outages with some downed power poles.  Numerous large branches of healthy trees will snap.  Some trees will be uprooted...especially where the ground is saturated.\n"
        elif wind_mph <= thresholds.get('danger', 90):
             t+="Very dangerous winds will produce widespread damage.  Airborne debris will cause damage.  Persons struck by debris may be injured or possibly killed.  The majority of mobile homes will be severely damaged...overturned and uninhabitable.  Some homes of frame construction will experience major damage...including roofs being lifted off and walls partially collapsing...leaving them uninhabitable.  Well constructed homes will have damage to shingles...siding...and gutters.  Windows will be blown out if not properly covered.  Partial roof failure is expected at some industrial parks...especially to those buildings with light weight steel and aluminum coverings.  Some low rise apartment building roofs may be torn off...along with siding and shingle damage.  A number of glass windows in high rise buildings will be blown out.  Loose outdoor items will become projectiles...causing additional damage and possible injury.  Extensive damage to power lines and poles will likely result in widespread power outages that could last from several days to weeks.  Numerous large branches will break.  Many trees will be uprooted or snapped.\n"
        elif wind_mph <= thresholds.get('extremeDanger', 110):
             t+="Extremely dangerous winds will cause extensive damage.  Structural collapse of some homes could cause severe injuries or possible death.  Persons struck by airborne debris risk injury and possible death.  Most mobile homes will be destroyed.  Numerous homes of poor to average construction will be destroyed or severely damaged...leaving them uninhabitable.  Considerable damage to well constructed homes is expected.  A number of roofs and exterior walls will fail.  Many metal roofs will be torn off buildings at industrial parks.  Partial roof and exterior wall failures are likely at low rise apartment buildings.  Many windows in high rise buildings will be blown out.  Falling and broken glass will pose a significant danger even after the storm.  Near total power loss is expected.  Potable water could become scarce as filtration systems begin to fail.  Many trees will be snapped or uprooted and block numerous roads.\n"
        elif wind_mph <= thresholds.get('devastating', 130):
             t+="Devastating damage is expected.  Collapse of some residential structures will put lives at risk.  Airborne debris will cause extensive damage.  Persons...pets...and livestock struck by the wind blown debris will be injured or killed.  Nearly all mobile homes will be destroyed.  Most homes will sustain severe damage with potential for complete roof failure and wall collapse.  Most industrial buildings will be destroyed...with others experiencing partial roof and wall damage.  Most low rise apartment buildings will be severely damaged or destroyed...and others will have partial roof and wall failure.  Numerous windows will be blown out of high rise buildings resulting in falling glass...which will pose a threat for days to weeks after the storm.  Considerable structural damage to large buildings is possible.  Electricity and water will be unavailable for days and perhaps weeks after the storm passes.  Most trees will be snapped or uprooted.  Fallen trees may cut off residential areas for days to weeks.\n"
        else:
             t+="Catastrophic damage is expected.  Collapse of residential structures will put lives at risk. Severe injury or death is likely for persons...pets...and livestock struck by wind blown debris.  Most of the area will be uninhabitable for weeks...perhaps longer.  Most homes will be destroyed...with total roof failure and wall collapse.  Nearly all industrial buildings and low rise apartment buildings will be severely damaged or destroyed.  Nearly all windows will be blown out of high rise buildings resulting in falling glass...which will pose a threat for days to weeks after the storm.  Considerable structural damage to large buildings is likely.  Nearly all trees will be snapped or uprooted and power poles downed.  Fallen trees and power poles will isolate residential areas.  Power outages will last for weeks to possibly months.  Long term water shortages will increase human suffering.\n"
        return self._frame(t)

    def _genericImpact_stmt(self, info):
        if info.allMarine:
            return ""
        t=""
        damage = self._getCategoryDamage(info.maxWind)
        if damage.strip() == "": return t
        t+="\n"
        t+=self._frame("A general concern should be for the possibility of " + damage + " somewhere within the area.")
        return t

    ###############################################################
    ### Example TCP product for automated testing
    ###############################################################
    ## Used for testing and debugging
    def _useTestTCP(self):
        #return True
        return False

    def _TCP_Product(self):
        return"""
ZCZC MIATCPAT2 ALL
TTAA00 KNHC DDHHMM
BULLETIN
HURRICANE KATRINA ADVISORY NUMBER  10
NWS TPC/NATIONAL HURRICANE CENTER Miami FL
11 PM EDT THU AUG 25 2005

...Eye of Katrina moving southwestward across Miami-Dade county...


Summary of 1100 PM EDT...0300 UTC...information
-----------------------------------------------
Location...25.5N 80.7W
About 35 miles...55 km SW of Miami Florida
About 20 miles...30 km NW of Homestead Florida
Maximum sustained winds...75 mph...120 km/hr
Present movement...west-southwest or 265 degrees at 8 mph...13 km/hr
Minimum central pressure...984 mb...29.06 inches


Watches and Warnings
--------------------
Changes with this advisory...

*The tropical storm warning and tropical storm watch along the east
coast of Florida north of Jupiter have been discontinued.


Summary of Warnings and Watches in effect...

A Hurricane Warning is in effect for...
*The southeast Florida coast from Jupiter Inlet southward to
Florida City...including Lake Okeechobee. Preparations to protect
life and property should have been completed.

A tropical storm warning is in effect for...
*All the Florida Keys and Florida Bay from Key West northward
*The gulf coast of Florida from Longboat Key south and eastward
to south of Florida City.

A Tropical Storm Watch is in effect for...
*The Florida west coast from north of Longboat Key to Anclote Key.

Interests elsewhere along the gulf coast of the United States should
monitor the progress of Katrina.

For storm information specific to your area...including possible
inland watches and warnings...please monitor products issued
by your local weather office.


Discussion and 48-hour outlook
------------------------------
At 11 PM EDT...0300 UTC...the eye of hurricane Katrina was located
near latitude 25.5 north...longitude  80.7 west.  Katrina is moving
toward the southwest near 8 mph...13 km/hr and this motion is
expected to continue during the next several hours. Katrina is
expected to move over the Gulf of Mexico Friday and Saturday.

Maximum sustained winds are near 75 mph...130 km/hr with higher
gusts. Katrina is a category one hurricane on the Saffir-Simpson
scale.  Some additional weakening is anticipated while Katrina is
over land...and it could weaken to a tropical storm early on Friday.
Restrengthening is expected on Friday or Saturday...and Katrina
could become a dangerous hurricane in the Gulf of Mexico in 2 to
3 days.

Hurricane force winds extend outward up to 10 miles from the
center...and tropical storm force winds extend outward up to
70 miles.  A wind gust to 87 mph...140 km/hr was recorded at Miami
National Weather Service Forecast Office/National Hurricane Center
and 81 mph...131 km/hr at the TaMiami airport this evening.

Estimated minimum central pressure is 984 mb...29.06 inches.


Storm hazards
-------------
Storm surge flooding...2 to 4 feet above normal tide levels...can be
expected along the west coast of Florida in areas of onshore flow
south of Venice and in Florida bay. Storm surge should begin to
decrease along the east coast of Florida.

Rainfall...Katrina is expected to produce a significant heavy
rainfall event over south Florida...and the Florida Keys. Total
rainfall accumulations of 6 to 10 inches with isolated maximum
amounts of 15 to 20 inches are possible.

Tornadoes...isolated tornadoes will also be possible over eastern
Florida and the Florida Keys.


Next advisory
--------------
Next intermediate advisories...100 AM and 300 AM EDT.
Next complete advisory...500 AM EDT.

$$
forecaster Avila

NNNN
"""
##        return """
##ZCZC MIATCPEP5 ALL
##TTAA00 KNHC DDHHMM
##BULLETIN
##HURRICANE LINDA ADVISORY NUMBER  12
##NWS TPC/NATIONAL HURRICANE CENTER MIAMI FL   EP152009
##800 PM PDT WED SEP 09 2009
##
##...Linda becomes a hurricane...the sixth hurricane of the eastern
##Pacific season...
##
##
##Summary of 800 PM PDT...0300 UTC...information
##----------------------------------------------
##Location...17.1n 129.4w
##About 1325 miles...2135 km wsw of the southern tip of Baja California
##Maximum sustained winds...80 mph...130 km/hr
##Present movement...northwest or 320 degrees at 6 mph...9 km/hr
##Minimum central pressure...984 mb...29.06 inches
##
##
##Watches and Warnings
##--------------------
##There are no coastal tropical cyclone watches or warnings in effect.
##
##
##Discussion and 48-hour outlook
##------------------------------
##At 800 PM PDT...0300 UTC...the center of hurricane Linda was located
##near latitude 17.1 north...longitude 129.4 west.  Linda is moving
##toward the northwest near 6 mph...9 km/hr...and this general motion
##is expected to continue for the next couple of days.
##
##Maximum sustained winds are near 80 mph...130 km/hr...with higher
##gusts.  Little change in strength is expected tonight and Thursday...
##with Linda forecast to weaken Thursday night and Friday.
##
##Hurricane force winds extend outward up to 25 miles...35 km...from
##the center...and tropical storm force winds extend outward up to
##125 miles...205 km.
##
##Estimated minimum central pressure is 984 mb...29.06 inches.
##
##
##Storm Hazards
##-------------
##None affecting land.
##
##
##Next advisory
##--------------
##Next complete advisory...200 AM PDT.
##
##$$
##forecaster Beven
##NNNN
##"""



    #####################################################################################
    #####################################################
    ### HLS GUI Processing and Configurable Dictionaries for
    #   the Overview GUI, Situations and Scenarios

    def _processVariableList(self, definition, parent):
        # Get Definition variables
        for key in definition.keys():
            exec "self._" + key + "= definition[key]"

        segmentList, argDict = self._determineSegments(definition, parent)
        if len(segmentList) == 0:
            return {("segments", "segments"):[]}

        # Overview GUI
        while True:
            overviewDict = self._displayGUI(argDict, segmentList, "Overview")
            if overviewDict == "UsePrev":
                return {("UsePrev", "UsePrev"): True}
            elif overviewDict == "Reset":
                continue  # Display Overview GUI again
            if overviewDict is None:
                return None
            break

        # Situation GUI (per segment)
        situationDict = self._displayGUI(argDict, segmentList, "Situation", overviewDict)
        if situationDict is None:
            return None

        # Scenario GUI (per segment)
        scenarioDict = self._displayGUI(argDict, segmentList, "Scenario", situationDict)
        if scenarioDict is None:
            return None

        # Consolidate information from GUI's
        varDict = overviewDict
        varDict[("segments:","segments")] = scenarioDict["segments"]
        return varDict

    def _determineSegments(self, definition, parent):
        # Get the segments based on hazards "overlaid" with combinations file
        argDict = {}

        dataMgr = parent
        argDict['dataMgr'] = dataMgr
        argDict["databaseID"] = self._getDbId(dataMgr, definition['database'])
        argDict["ifpClient"] = PyFPClient(VizApp.getWsId(), dataMgr.getSiteID())
        import VTECMessageType
        vtecMode = VTECMessageType.getVTECMessageType(self._pil)
        argDict["vtecMode"] = vtecMode
        gfeMode = dataMgr.getOpMode().name()
        if gfeMode == "PRACTICE":
            argDict["vtecActiveTable"] = "PRACTICE"
        else:
            argDict["vtecActiveTable"] = "active"
        argDict['creationTime'] = int(time.time()/60)*60.0
        argDict["definition"] = definition
        accessor = ModuleAccessor.ModuleAccessor()
        dfEditAreas = self._defaultEditAreas
#        print "dfEditAreas", dfEditAreas
        dfEditAreas = accessor.variable(dfEditAreas, "Combinations")
        if dfEditAreas is None:
            LogStream.logVerbose("Combination file not found: " + dfEditAreas)
            return [], None

        # Need to check hazards against all edit areas in the CWA MAOR
        allAreas = self._inlandAreas()+self._marineAreas()+self._coastalAreas()
        argDict["combinations"]= [(allAreas,"Region1")]
        #print "\n****************determineSegments calling getHazardsTable"
        hazards = self._getHazardsTable(argDict, self.filterMethod)
        argDict["hazards"] = hazards

        # Get the segments resulting from Hazards

        #print "\nRaw Analyzed", hazards.rawAnalyzedTable()
        hazSegments = self.organizeHazards(hazards.rawAnalyzedTable())
        print "\nSegments from HazardsTable organizeHazards", hazSegments
        combos = dfEditAreas
        print "\nSegments from Zone Combiner", combos
        # "Overlay" the forecaster-entered combinations onto the segments
        segmentList = self._refineSegments(hazSegments, combos)
        print "\nNew segments", segmentList

        # Check for all CON
        allCON = True
        segmentAreas = []
        for segmentAreas in hazSegments:
            hazardList = hazards.getHazardList(segmentAreas)
            for hazard in hazardList:
                action = hazard['act']
                #print "hazard", hazard
                if action != "CON":
                    allCON = False
                    break
            if not allCON: break
        argDict["allCON"] = allCON
        #print "allCON", allCON

        # Determine if we should have Event Context limited to Abbreviated.
        # Here are the rules:
        #   --If there are no Continuations, limit to abbreviated UNLESS
        #   --If all are HU.S, do not limit to abbreviated, but do limit to Pre or Non Event
        #   --IF all are CAN, UPG (ignoreActions), do not limit to abbreviated

        noCON = True
        allHUS = True
        allIgnoreActions = True

        segmentAreas = []
        for segmentAreas in hazSegments:
            hazardList = hazards.getHazardList(segmentAreas)
            for hazard in hazardList:
                action = hazard['act']
                sig = hazard['sig']
                #print "hazard", hazard
                if action == "CON":
                    noCON = False
                if sig != "S":
                    allHUS = False
                if action not in self._ignoreActions():
                    allIgnoreActions = False
        forceAbbrev = noCON
        if allHUS or allIgnoreActions: forceAbbrev = False
        argDict["forceAbbrev"] = forceAbbrev
        argDict["allHUS"] = allHUS
        #print "noCON", noCON
        #print "allHUS", allHUS
        #print "allIgnoreActions", allIgnoreActions
        #print "forceAbbrev", forceAbbrev

        # Determine if sigs are watches and/or statements to limit

        watchEC = True
        segmentAreas = []
        for segmentAreas in hazSegments:
            hazardList = hazards.getHazardList(segmentAreas)
            for hazard in hazardList:
                sig = hazard['sig']
                if sig == "W":
                    watchEC = False
                    break
            if not watchEC: break
        argDict["watchEC"] = watchEC


        ### Determine if all actions are cancel to limit to Post Event ot Tropical

        allCAN = True
        segmentAreas = []
        for segmentAreas in hazSegments:
            hazardList = hazards.getHazardList(segmentAreas)
            for hazard in hazardList:
                action = hazard['act']
                #print "hazard", hazard
                if action != "CAN":
                    allCAN = False
                    break
            if not allCAN: break
        argDict["allCAN"] = allCAN

        return segmentList, argDict

    def _refineSegments(self, hazSegments, combos):
        """Break down each segment further according to combos given.
        Make sure the resulting segments follow the ordering of the combos.
        """
        if combos == []:
            return hazSegments
        newSegments = []  # list of lists
        newAreas = []
        for combo, label in combos:
            # Each combination will be tested to see if it can stay intact
            # i.e. if all areas in the combo are in the same segment
            # else split it into like segments
            #
            # segmentMapping is a list where each entry is
            #   the hazSegment in which the corresponding combo area appears.
            # (We need to define self._segmentList for the mapping function
            #   to use)
            self._segmentList = hazSegments
            segmentMapping = map(self._findSegment, combo)
            #print "   segmentMapping", segmentMapping

            # segmentDict keys will be the hazSegments and
            #   we will gather all the areas of the combos that appear
            #   in each of these hazSegments
            segmentDict = {}
            keyList = []
            for areaName in combo:
                #print "       Adding", areaName
                key = tuple(segmentMapping[combo.index(areaName)])
                if key == ():  # If no hazard for area, do not include
                    continue
                if key not in keyList:
                    keyList.append(key)
                segmentDict.setdefault(key,[]).append(areaName)
            #print "   segmentDict", segmentDict

            # Keep track of the areas that we are including
            for key in keyList:
                segAreas = segmentDict[key]
                newAreas = newAreas + segAreas
                newSegments.append(segAreas)
        #print "   newSegments", newSegments
        # Now add in the hazAreas that have not been accounted for
        #   in the combinations
        hazAreas = []
        for hazSegment in hazSegments: hazAreas = hazAreas + hazSegment
        for hazSegment in hazSegments:
            newSeg = []
            for hazArea in hazSegment:
                if hazArea not in newAreas:
                    newSeg.append(hazArea)
            if newSeg != []:
                newSegments.append(newSeg)
        return newSegments

    def _getDbId(self, dataMgr, db):
        pm = dataMgr.getParmManager()
        if db in ['Fcst', 'Fcst_Prac','Fcst_Test']: return str(pm.getMutableDatabase())
        elif db == 'Official': return str(pm.getProductDB())
        elif db == 'ISC':
            dbs = pm.getIscDatabases()
            if len(dbs):
                iscDB = str(dbs[-1])   #last one is the real one by convention
            else:
                iscDB = str(DatabaseID.databaseID_default().toJavaObj())

            return iscDB

    def _findSegment(self, areaName):
        for segment in self._segmentList:
            if areaName in segment:
                return segment
        return []

    def _getHazardsTable(self, argDict, filterMethod, editAreas=None):
        # Set up edit areas as list of lists
        # Need to check hazards against all edit areas in the CWA MAOR
        allAreas = self._inlandAreas()+self._marineAreas()+self._coastalAreas()
        argDict["combinations"]= [(allAreas,"Region1")]
        dfEditAreas = argDict["combinations"]
        editAreas = []
        for area, label in dfEditAreas:
            if type(area) is types.ListType:
                editAreas.append(area)
            elif type(area) is types.TupleType: #LatLon
                editAreas.append([self.__getLatLonAreaName(area)])
            else:
                editAreas.append([area])
        # Get Product ID and other info for HazardsTable
        pil = self._pil
        stationID4 = self._fullStationID
        productCategory = pil[0:3]   #part of the pil
        definition = argDict['definition']
        sampleThreshold = definition.get("hazardSamplingThreshold", (10, None))
        # Process the hazards
        accurateCities = definition.get('accurateCities', 0)
        cityRefData = []
        import HazardsTable
        hazards = HazardsTable.HazardsTable(
          argDict["ifpClient"], editAreas, productCategory, filterMethod,
          argDict["databaseID"],
          stationID4, argDict["vtecActiveTable"], argDict["vtecMode"], sampleThreshold,
          creationTime=argDict["creationTime"], accurateCities=accurateCities,
          cityEditAreas=cityRefData, dataMgr=argDict['dataMgr'])
        return hazards

    ###################################################################################
    ###################################################################################
    ##  TK GUI Classes
    ##
    ## IF you want to override the GUI, you must include all the code
    ## from here on.  This includes the calling method _displayGUI

    def _displayGUI(self, argDict, segmentList, dialogName, infoDict=None):
        if dialogName == "Overview":
            dialogClass = HLS_Overview
        elif dialogName == "Situation":
            dialogClass = HLS_Situation
        elif dialogName == "Scenario":
            dialogClass = HLS_Scenario
        dialog = dialogClass(self, argDict, segmentList, infoDict)
        status = dialog.status()
        LogStream.logVerbose("status="+status)
        if status == "Cancel":
            return None
        elif status in ["Reset", "UsePrev"]:
            return status
        else:
            return dialog.getVarDict()


import Tkinter, copy, re


class AutoScrollbar(Tkinter.Scrollbar):
    # a scrollbar that hides itself if it's not needed.  only
    # works if you use the grid geometry manager.
    def set(self, lo, hi):
        if float(lo) <= 0.0 and float(hi) >= 1.0:
            # grid_remove is currently missing from Tkinter!
            self.tk.call("grid", "remove", self)
        else:
            self.grid()
        Tkinter.Scrollbar.set(self, lo, hi)
    def pack(self, **kw):
        raise Tkinter.TclError, "cannot use pack with this widget"
    def place(self, **kw):
        raise Tkinter.TclError, "cannot use place with this widget"


class ScrolledBox(Tkinter.Frame,):
    def __init__(self, parent=None, side='right', **kw):
        """Scrolled Box widget with vertical scrollbar on the right or left.
        """
        Tkinter.Frame.__init__(self, parent, **kw)
        if side == 'right':
            ysbCol=1
            csbCol=0
        else:
            ysbCol=0
            csbCol=1

        self.grid_rowconfigure(0, weight=1)
        self.grid_columnconfigure(csbCol, weight=1)

        xscrollbar = AutoScrollbar(self, orient=Tkinter.HORIZONTAL)
        xscrollbar.grid(row=1, column=csbCol, sticky=Tkinter.E+Tkinter.W)

        yscrollbar = AutoScrollbar(self)
        yscrollbar.grid(row=0, column=ysbCol, sticky=Tkinter.N+Tkinter.S)

        canvas = Tkinter.Canvas(self, bd=0,relief=Tkinter.SUNKEN,
                                xscrollcommand=xscrollbar.set,
                                yscrollcommand=yscrollbar.set)
        self._interior = Tkinter.Frame(canvas)
        canvas.create_window(0, 0, window=self._interior, anchor='nw')

        self._canvas = canvas
        canvas.bind('<Configure>', self.configCB)
        canvas.grid(row=0, column=csbCol, sticky='nsew')

        xscrollbar.config(command=canvas.xview)
        yscrollbar.config(command=canvas.yview)

    def interior(self):
        return self._interior

    def configCB(self, event):
        self._canvas.config(scrollregion=self._canvas.bbox(Tkinter.ALL))


class HLS_Dialog(StartupDialog.IFPDialog):
    def __init__(self, parent, argDict, segmentList, infoDict=None):
        self._status = "Cancel"    # exception, or user-cancels
        self._tkObject_dict = {}   # place to store reference to tk objects
        self._varDict = {}         # all end results must be saved here
        self._argDict = argDict
        self._segmentList = segmentList
        self._infoDict = infoDict
        self._parent = parent
        StartupDialog.IFPDialog.__init__(self, parent=None, title="HLS")

    def getVarDict(self):
        return self._varDict

    def _makeRadioOrCheckList(self, master, label, elementList, default=None,
                        buttonSide=Tkinter.TOP, frameSide=Tkinter.LEFT, entryField=None,
                        headerFG=None, headerFont=None, boxType="radio",
                              listFrameRelief=Tkinter.GROOVE):
        listFrame = Tkinter.Frame(master, relief=listFrameRelief, borderwidth=1)

        if label != "":
            listLabel = Tkinter.Label(listFrame, text=label, fg=headerFG, font=headerFont)
            listLabel.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO, padx=10)

        ivar = Tkinter.IntVar()
        defaultIndex = 0
        ivarList = []
        for element in elementList:
            index = elementList.index(element)
            if type(element) is types.TupleType:
                element, key = element
            if boxType== "radio":
                button = Tkinter.Radiobutton(listFrame, variable=ivar, text=element, value=index)
            else:
                ivar = Tkinter.IntVar()
                if default is not None and element in default: ivar.set(1)
                else: ivar.set(0)
                button= Tkinter.Checkbutton(listFrame, variable=ivar, text=element)
                ivarList.append(ivar)
            button.pack(side=buttonSide, anchor=Tkinter.W, expand=Tkinter.YES, padx=4)
            # Look for default
            if element == default:
                defaultIndex = index

        entryObject = None
        if entryField is not None:
            entryObject = self._makeEntry(listFrame, entryField)
        # packing
        listFrame.pack(side=frameSide, expand=Tkinter.NO, fill=Tkinter.Y) #, anchor=Tkinter.N)
        #listFrame.pack(side=frameSide, expand=Tkinter.YES, fill=Tkinter.Y, anchor=Tkinter.N)

        if boxType == "radio":
            ivar.set(defaultIndex) # set the default
        if boxType == "check":
            ivar = ivarList
        return ivar, entryObject

    def _makeCheckList(self, master, label, elementList, side=Tkinter.TOP,
                       segmentAreas=None, maxLen=None, colLabels=True):
       """Custom Checklists for Scenario GUI, Step 9b.
       """
       listFrame = Tkinter.Frame(master, relief=Tkinter.GROOVE, borderwidth=2)

       if len(label)>0:
           listLabel = Tkinter.Label(listFrame, text=label)
           listLabel.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO, padx=10)

       # Frames for each column. Nothing fancy, make all the same with optional
       # label at the top. Define columns and labels in colL dict. Frames will
       # be put in colF dict to use below. Order of columns will be done at the end
       # when the columns are packed.
       colL={'order': '',
             'prev':'Prev',
             'import':'Import',
             'section':'Section',
            }
       colF = {}
       for c in colL.keys():
           colF[c]=Tkinter.Frame(listFrame, relief=Tkinter.FLAT, borderwidth=0)
           if colLabels:
               Tkinter.Label(colF[c],text=colL[c]).pack(side=Tkinter.TOP,anchor=Tkinter.W)

       # Do for all rows, filling in all columns.
       ivarList = []
       for eleDict in elementList:
           name = eleDict.get('name', "")
           label = eleDict.get('label', "")
           #print "********* _makeCheckList dict=",eleDict

           #--- Order Entry box
           frame=colF['order']
           if eleDict.get("orderBox", False):
               iOrder = Tkinter.Entry(frame, relief=Tkinter.SUNKEN, width=2)
           else:
               iOrder = Tkinter.Label(frame,text='', width=2)
           iOrder.pack(side=Tkinter.TOP, anchor=Tkinter.W,expand=Tkinter.YES)

           #--- usePrev Checkbutton
           frame=colF['prev']
           if eleDict.get("usePrev", False):
               prevVar = Tkinter.IntVar()
               prevVar.set(0)
               iPrev = Tkinter.Checkbutton(frame, variable=prevVar, text="")
           else:
               iPrev = Tkinter.Label(frame,text='', width=3)
               prevVar = None
           iPrev.pack(side=Tkinter.TOP, anchor=Tkinter.W,expand=Tkinter.NO)

           #--- importMethod - Checkbutton
           frame=colF['import']
           if eleDict.get("importMethod", False) or eleDict.get("importPIL", False):
               importVar = Tkinter.IntVar()
               importVar.set(0)
               iImport = Tkinter.Checkbutton(frame, variable=importVar,text="")
           else:
               iImport = Tkinter.Label(frame,text='', width=1)
               importVar = None
           iImport.pack(side=Tkinter.TOP, anchor=Tkinter.W,expand=Tkinter.YES)

           #--- Section name Checkbutton
           frame=colF['section']
           ivar = Tkinter.IntVar()
           defaultOn = eleDict.get("defaultOn", 0)
           if type(defaultOn) is types.MethodType:
               defaultOn = defaultOn(name, segmentAreas)
           ivar.set(defaultOn)
           button = Tkinter.Checkbutton(frame, variable=ivar, text=label)
           button.pack(side=Tkinter.TOP, anchor=Tkinter.W, expand=Tkinter.YES, padx=0)

           #print "\nAppending", name, button, iOrder, iPrev
           ivarList.append((name, ivar, iOrder, prevVar, importVar))

       # packing
       listFrame.pack(side=Tkinter.LEFT,expand=Tkinter.YES,fill=Tkinter.Y,anchor=Tkinter.N)

       # Change the order of the colums by the order of list
       for c in ['order','prev','section', 'import']:
       #for c in ['order','prev','import','section']:
           colF[c].pack(side=Tkinter.LEFT,expand=Tkinter.YES,
                        fill=Tkinter.Y,anchor=Tkinter.N)
       return ivarList

    def _makeEntry(self, frame, text, width=20):
        label = Tkinter.Label(frame, text=text)
        label.pack(side=Tkinter.LEFT, fill=Tkinter.X,  expand=Tkinter.NO)
        entry = Tkinter.Entry(frame, relief=Tkinter.SUNKEN, width=width)
        entry.pack(side=Tkinter.LEFT, fill=Tkinter.X, expand=Tkinter.NO)
        return entry

    def _makeSegmentColumns(self, segNum, frame, segmentAreas):
        # Need standard widths so the columns line up across segments
        sn_width = 4
        hz_width = 15

        widgets = []

        segNumFrame = Tkinter.Frame(frame, relief=Tkinter.FLAT, width=sn_width)
        label = Tkinter.Label(segNumFrame, text=`segNum`)
        label.pack(side=Tkinter.LEFT, fill=Tkinter.X, expand=Tkinter.YES)
        #segNumFrame.pack(side=Tkinter.LEFT, expand=Tkinter.NO,
        #  fill=Tkinter.Y, anchor=Tkinter.N)
        widgets.append(segNumFrame)

        landAreas = self._parent._inlandAreas() + self._parent._coastalAreas()
        areaDisplayType_land, width1 = self._parent._areaDisplayType_land()
        areaDisplayType_marine, width2 = self._parent._areaDisplayType_marine()
        zf_width = max(width1, width2)

#        sb = ScrolledBox(frame)
        sb = ScrolledBox(frame, side="left")
        interior = sb.interior()

        zoneFrame = Tkinter.Frame(interior, relief=Tkinter.FLAT, width=zf_width)
        if areaDisplayType_land != 'ugcCode' or areaDisplayType_marine != 'ugcCode':
            accessor = ModuleAccessor.ModuleAccessor()
            areaDict = accessor.variable(self._parent._areaDictionary,"AreaDictionary")

        segmentAreas.sort()
        for area in segmentAreas:
            if area in landAreas:
                areaDisplayType = areaDisplayType_land
            else:
                areaDisplayType = areaDisplayType_marine
            if areaDisplayType != "ugcCode":
                try: area = areaDict[area].get(areaDisplayType)
                except: pass
            if area is None:
                area = ""
            area= self._linebreak(area, zf_width)
            label = Tkinter.Label(zoneFrame, text=area, width=zf_width, anchor=Tkinter.W)
            label.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO)
        zoneFrame.pack(side=Tkinter.LEFT, expand=Tkinter.NO,
          fill=Tkinter.Y, anchor=Tkinter.N)
        #sb.pack(side=Tkinter.LEFT, fill=Tkinter.BOTH, expand=Tkinter.NO)
        interior.update()
        h=interior.winfo_reqheight()
        sizeDict = self._parent._GUI_sizing_dict()
        zoneLines = sizeDict["zoneLines"]
        charSize = sizeDict["charSize"]
        heightLimit = zoneLines * charSize
        if h > heightLimit:
            h = heightLimit
        w=interior.winfo_reqwidth()
        sb._canvas["height"] = h
        sb._canvas["width"] = w
        widgets.append(sb)
        #sb.pack(side=Tkinter.LEFT)

        hazardFrame = Tkinter.Frame(frame, relief=Tkinter.FLAT, width=hz_width)
        hazardTable = self._argDict["hazards"]
        hazards = hazardTable.getHazardList(segmentAreas)
        if hazards == []:
            hazards = [{'phensig':'None'}]
        hazardKeys = []
        addEntry=False
        # Updated code below to make Situation selection smarter
        for hazard in hazards:
            hazKey = hazard['phensig'] + " " + hazard['act']
            sitKey = hazard['act'] + hazard['phen'] + "." + hazard['sig']
            hazardKeys.append(hazard['phen']+"."+hazard['sig'])
            label = Tkinter.Label(hazardFrame, text=hazKey,width=hz_width)
            label.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO)
            if hazard['phen'] == "HU" and hazard['sig'] == "S":
                addEntry = True
        #hazardFrame.pack(side=Tkinter.LEFT, expand=Tkinter.NO,
        #  fill=Tkinter.Y, anchor=Tkinter.N)
        widgets.append(hazardFrame)
        print "\n\n***********************"
        print "sitKey is ",  sitKey
        return widgets, hazardKeys, sitKey, addEntry

    def _linebreak(self, phrase, linelength, breakStr=[" ", "..."]):
        # Break phrase into lines the given linelength
        if len(phrase) <= linelength: return phrase
        start = 0
        str = ""
        further = 0
        while start < len(phrase):
            end = start + linelength + further
            if end >= len(phrase):
                str = str + phrase[start:len(phrase)] + "\n"
                break
            breakFound = 0
            #search for break characters in string
            for breakChars in breakStr:
                ind = string.rfind(phrase, breakChars, start, end)
                if ind >= 0:
                    breakFound = 1
                    break
            #if not found, then we need to search further, this makes the
            #line too long, but it is better than simply splitting a word
            #in the middle of it.
            if breakFound == 0:
                further = further + 1
                continue

            if breakChars != " ":
                # We want to preserve the break characters, not drop them
                includeInd = ind + len(breakChars)
            else:
                includeInd = ind

            str = str + phrase[start:includeInd] + "\n"
            start = ind + len(breakChars)
            further = 0
        return str

    def _makeLine(self, interior, row, columnspan, width=200, char="-"):
        row = row+1
        lineFrame = Tkinter.Frame(interior, relief=Tkinter.FLAT)
        text=""
        for i in range(width): text = text + char
        label = Tkinter.Label(lineFrame, text=text)
        label.pack(side=Tkinter.LEFT, fill=Tkinter.X,  expand=Tkinter.NO)
        lineFrame.grid(row=row, columnspan=columnspan)
        return row

    def cancelCB(self):
        self._status = "Cancel"
        #self.cancel()
        self.withdraw()
        self.destroy()

    def _entryName(self, name):
        return name+"_entry"

    def _makeTuple(self,str):
        str = re.sub('(?im)[^_a-z]', '', str)
        return (str+":",str)

    def _setVarDict(self, key, value, options=None):
        if options is not None:
            value = options[value]
            if type(value) is types.TupleType:
                value = value[1]
        self._varDict[self._makeTuple(key)] = value

    def status(self):
        return self._status

    def buttonbox(self):
        # override the existing ok/cancel button box, removing it.
        # we do this so that we can attach our own hooks into the functions.
        pass


class HLS_Overview(HLS_Dialog):
    def __init__(self, parent, argDict, segmentList, infoDict=None):
        HLS_Dialog.__init__(self, parent, argDict, segmentList, infoDict)

    def body(self, master):
        # build the main display dialog
        tkObject_dict = self._tkObject_dict
        overviewList = self._parent._overview_list(self._argDict)
        endInstructions = self._parent._overviewEndInstructions()
        fontDict = self._parent._font_GUI_dict()

        #  OVERVIEW header
        headerFG, headerFont = fontDict["headers"]
        frame = Tkinter.Frame(master, relief=Tkinter.GROOVE, borderwidth=1)
        label = Tkinter.Label(frame, text="OVERVIEW", fg=headerFG, font=headerFont)
        label.pack(side=Tkinter.LEFT, fill=Tkinter.X,  expand=Tkinter.NO)
        frame.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO)

        numBoxes = 3

        boxes = []
        for i in range(numBoxes):
            newBox = Tkinter.Frame(master)
            newBox.pack(side=Tkinter.TOP, expand=Tkinter.NO,
              fill=Tkinter.Y, anchor=Tkinter.W)
            boxes.append(newBox)

        for infoDict in overviewList:
            name = infoDict["name"]
            label = infoDict["label"]
            options = infoDict.get("options", [])
            entryField = infoDict.get("entryField", None)
            default = infoDict.get("default", None)
            optionType = infoDict.get("optionType", "radio")

            index = overviewList.index(infoDict)
            if index < 3:
                boxNum = 0
                buttonSide=Tkinter.TOP
                frameSide = Tkinter.LEFT
            elif index in [3,4]:
                boxNum = 1
                buttonSide=Tkinter.LEFT
                frameSide=Tkinter.TOP
            elif index in [5,6]:
                boxNum = 2
                buttonSide=Tkinter.TOP
                frameSide=Tkinter.LEFT

            box = boxes[boxNum]

            if name == "MainHeadline": entryField = None

            tkObject_dict[name], entryObject = self._makeRadioOrCheckList(
                    box, label, options, default, buttonSide=buttonSide, frameSide=frameSide,
                    entryField=entryField, headerFG=headerFG,
                    headerFont=headerFont, boxType=optionType)
            if entryObject is not None:
                tkObject_dict[self._entryName(name)] = entryObject

            if name == "MainHeadline":
                frame = Tkinter.Frame(box, relief=Tkinter.GROOVE, borderwidth=1)
                tkObject_dict[self._entryName(name)] = self._makeEntry(frame, "", 80)
                frame.pack(fill=Tkinter.X, expand=Tkinter.YES)

        # End Instructions and Buttons
        fg, font = fontDict["instructions"]
        frame = Tkinter.Frame(master, relief=Tkinter.GROOVE, borderwidth=1)
        label = Tkinter.Label(frame, text=endInstructions, fg=fg, font=font)
        label.pack(side=Tkinter.LEFT, fill=Tkinter.X,  expand=Tkinter.NO)
        self._makeButtons(frame)
        frame.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO)

##############  Graying PreviousHLS button
    def _makeButtons(self, master):
        frame = Tkinter.Frame(master)
        buttonList = self._parent._GUI1_configDict().get("buttonList", [])
        for button, label in buttonList:
            state = Tkinter.NORMAL
            if button == "PreviousHLS":
                command = self.previousCB
                allCON = self._argDict.get("allCON", False)
                if not allCON: state = Tkinter.DISABLED
            elif button == "Reset":
                command = self.resetCB
            elif button == "Next":
                command = self.okCB
            else: # Cancel
                command = self.cancelCB
            Tkinter.Button(frame, text=label, command=command, width=10,
                           state=state).pack(side=Tkinter.LEFT, pady=5, padx=10)
        frame.pack()

    def resetCB(self):
        self._status = "Reset"
        self.ok()

    def previousCB(self):
        self._status = "UsePrev"
        self.ok()

    def okCB(self):
        # pull the data from the tkObject_dict before they get toasted
        tkObject_dict  = self._tkObject_dict
        overviewList = self._parent._overview_list(self._argDict)
        for infoDict in overviewList:
            name = infoDict["name"]
            label = infoDict["label"]
            options = infoDict.get("options", [])
            entryField = infoDict.get("entryField", None)
            default = infoDict.get("default", None)
            optionType = infoDict.get("optionType", "radio")

            if optionType == "check":
                checkList = []
                ivarList = tkObject_dict[name]
                for i in range(len(options)):
                    if ivarList[i].get():
                        checkList.append(options[i])
                value = checkList
                self._setVarDict(name, value)
            else:
                value = tkObject_dict[name].get()
                self._setVarDict(name, value, options)

            if entryField is not None:
                entryName = self._entryName(name)
                self._setVarDict(entryName, tkObject_dict[entryName].get())
        # close window and set status "Ok"
        self._status = "Ok"
        self.ok()

class HLS_Situation(HLS_Dialog):
    def __init__(self, parent, argDict, segmentList, infoDict=None):
        HLS_Dialog.__init__(self, parent, argDict, segmentList, infoDict)

    def body(self, master):
        tkObject_dict = self._tkObject_dict
        situations = self._parent._situation_list()
        self._situationLabels = [entry['label'] for entry in situations]
        fontDict = self._parent._font_GUI_dict()
        headerFG, headerFont = fontDict["headers"]
        guiLabels = self._parent._GUI_labels()

        sizeDict = self._parent._GUI_sizing_dict()
        heightLimit = sizeDict["GUI_height_limit"]
        width = sizeDict["GUI_2_width"]
        zoneLines = sizeDict["zoneLines"]

        sb = ScrolledBox(master)
        interior =  sb.interior()

        row = 0
        columns=4
        #  SITUATION header
        frame = Tkinter.Frame(master, relief=Tkinter.GROOVE, borderwidth=1)
        label = Tkinter.Label(frame, text="SITUATIONS", fg=headerFG, font=headerFont)
        label.pack(side=Tkinter.LEFT, fill=Tkinter.X,  expand=Tkinter.NO)
        frame.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO)
##        frame.grid(row=0, columnspan=columns, sticky=Tkinter.W)
##        row = row + 1

        # Labels
        frame = Tkinter.Frame(master, relief=Tkinter.GROOVE, borderwidth=1)
        text = guiLabels['GUI_2']
        label = Tkinter.Label(frame, fg=headerFG, font=headerFont, text=text)
        label.pack(side=Tkinter.LEFT, fill=Tkinter.X, expand=Tkinter.NO)
        frame.pack(side=Tkinter.TOP,fill=Tkinter.X, expand=Tkinter.NO)
##        frame.grid(row=row, columnspan=columns, sticky=Tkinter.W)
##        row=row+1

        uiSegments = []
        self._segNum = 0
        for segmentAreas in self._segmentList:
            widgets, uiSegment = self._makeSegmentFrame(interior, segmentAreas)
            uiSegments.append(uiSegment)
            column = 0
            for widget in widgets:
                widget.grid(sticky=Tkinter.N+Tkinter.W, row=row, column=column)
                column = column +1
            row=self._makeLine(interior, row, columnspan=columns, width=160)
            row = row+1
        tkObject_dict["segments"] = uiSegments
        #segBox.pack(fill=Tkinter.X, expand=Tkinter.YES)
        bframe = Tkinter.Frame(master, relief=Tkinter.GROOVE, borderwidth=0)
        self._makeButtons(bframe, row=row, columnspan=columns)

        # Get the requested size of the interior frame.
        # While winfo_reqheight should get the size the widget is requesting,
        # It seems you still have to call update first, and calling update
        # certainly won't hurt - PJ
        interior.update()
        h=interior.winfo_reqheight()
        if h > heightLimit:
            h = heightLimit
        w=interior.winfo_reqwidth()
        if w > width:
            w = width
        sb._canvas["height"] = h
        sb._canvas["width"] = w

        sb.pack(side=Tkinter.TOP, fill=Tkinter.BOTH, expand=Tkinter.YES)
        bframe.pack(side=Tkinter.BOTTOM,fill=Tkinter.X, expand=Tkinter.NO)

    def _makeSegmentFrame(self, master, segmentAreas):
        #frame = Tkinter.Frame(master, relief=Tkinter.GROOVE, borderwidth=1)
        self._segNum+=1
        segNum = self._segNum
        widgets, hazardKeys, sitKey, addEntry = self._makeSegmentColumns(segNum, master, segmentAreas)

        # Find situations for segment hazards
        # IF the Event Context is "Abbreviated", "NonEvent" or "PostEvent"
        #   only allow that matching situation
        # Further refine the choices based on the action/phen/sig combo
        #   and the EC

        ec = self._infoDict[("EventContext:", "EventContext")]
        onlySituation = None
        if ec in ["Abbreviated", "NonEvent", "PostEvent"]:
            onlySituation = ec
        situationDicts = self._parent._situation_list()
        situations = []
        for sitDict in situationDicts:
            sitEC = sitDict.get("ec",  [])
            sitPairs = sitDict.get("hazPairings",  [])
##            sitActions = sitDict.get("action",  [])
            if sitKey in sitPairs and ec in sitEC:
##                for hazardKey in hazardKeys:
##                    if hazardKey in sitHazards:
                if sitDict not in situations:
                    sitName = sitDict['name']
                    if onlySituation and sitName != onlySituation:
                        continue
                    situations.append(sitDict)

        print "situations are: ", situations
        situationLabels = [sitDict['label'] for sitDict in situations]
        situationEntryFrame = Tkinter.Frame(master, relief=Tkinter.FLAT, borderwidth=1)
        situationFrame = Tkinter.Frame(situationEntryFrame, relief=Tkinter.FLAT, borderwidth=1)
        uiSituation, entryObj = self._makeRadioOrCheckList(
            situationFrame, " ",situationLabels, buttonSide=Tkinter.LEFT, frameSide=Tkinter.LEFT,
            listFrameRelief=Tkinter.FLAT)
        situationFrame.pack(side=Tkinter.TOP, expand=Tkinter.YES,
          fill=Tkinter.Y, anchor=Tkinter.W)

        if addEntry:
            # Add an entry field for headline plus option to Use previous
            entryFrame = Tkinter.Frame(situationEntryFrame, relief=Tkinter.GROOVE, borderwidth=1)
            uiEntry = self._makeEntry(entryFrame, "Headline", width=45)
            ivarList = self._makeCheckList(
                entryFrame, "", [{"name":"Use Prev", "label":"UsePrev"}], colLabels=False)
            name, uiUsePrev, iOrder, iPrev, iImport = ivarList[0]
            entryFrame.pack(side=Tkinter.TOP)
        else:
            uiEntry = None
            uiUsePrev = None
        #situationEntryFrame.pack(side=Tkinter.LEFT, expand=Tkinter.YES,
        #  fill=Tkinter.Y, anchor=Tkinter.W)
        widgets.append(situationEntryFrame)
##
##        frame.pack(fill=Tkinter.X, expand=Tkinter.YES)
        return widgets, (segNum, segmentAreas, uiSituation, situations, uiEntry, uiUsePrev)

    def _makeButtons(self, master, row, columnspan):
        frame = Tkinter.Frame(master)
        buttonList = self._parent._GUI2_configDict().get("buttonList", [])
        for button, label in buttonList:
            if button == "Next":
                command = self.okCB
            else: # button == "Cancel":
                command = self.cancelCB
            Tkinter.Button(frame, text=label, command=command, width=10,
                           state=Tkinter.NORMAL).pack(side=Tkinter.LEFT, pady=5, padx=10)
        frame.grid(row=row, columnspan=columnspan)

    def okCB(self):
        # pull the data from the tkObject_dict before they get toasted
        tkObject_dict  = self._tkObject_dict
        segments = []
        #print "\nsegments", tkObject_dict["segments"]
        for segNum, segment, uiSituation, situationDicts, uiEntry, uiUsePrev in tkObject_dict["segments"]:
            index = uiSituation.get()
            sitDict = copy.deepcopy(situationDicts[index])
            if uiEntry is not None: sitDict['userHeadline_HU_S'] = uiEntry.get()
            if uiUsePrev is not None: sitDict['usePrev_HU_S_Headline'] = uiUsePrev.get()
            segments.append((segNum, segment, sitDict))
        self._varDict["segments"] = segments

        #print "varDict", self._varDict
        # close window and set status "Ok"
        self._status = "Ok"
        self.ok()

class HLS_Scenario(HLS_Dialog):
    def __init__(self, parent, argDict, segmentList, infoDict=None):
        HLS_Dialog.__init__(self, parent, argDict, segmentList, infoDict)

    def body(self, master):

        sizeDict = self._parent._GUI_sizing_dict()
        heightLimit = sizeDict["GUI_height_limit"]
        width = sizeDict["GUI_3_width"]
        segments = self._infoDict["segments"]

        sb = ScrolledBox(master)
        interior =  sb.interior()

        # build the main display dialog
        columns=6
        tkObject_dict = self._tkObject_dict
        #box = Tkinter.Frame(interior)
        row, tkObject_dict["segments"] = self._makeScenarioGUI(interior)
        bframe = Tkinter.Frame(master, relief=Tkinter.GROOVE, borderwidth=0)
        self._makeButtons(bframe, row=row, columnspan=columns)
        #box.pack(side=Tkinter.TOP, fill=Tkinter.BOTH, expand=Tkinter.YES)

        interior.update()
        h=interior.winfo_reqheight()
        if h > heightLimit:
            h = heightLimit
        w=interior.winfo_reqwidth()
        if w > width:
            w = width
        sb._canvas["height"] = h
        sb._canvas["width"] = w

        sb.pack(side=Tkinter.TOP, fill=Tkinter.BOTH, expand=Tkinter.YES)
        bframe.pack(side=Tkinter.BOTTOM,fill=Tkinter.X, expand=Tkinter.NO)

    def _makeScenarioGUI(self, master):
       # Entry for each segment

       #segBox = Tkinter.Frame(master)
       # Labels
       row=0
       columns=6
       segments = self._infoDict["segments"]
       guiLabels = self._parent._GUI_labels()
       fontDict = self._parent._font_GUI_dict()
       headerFG, headerFont = fontDict["headers"]
       texta = guiLabels['GUI_3a']
       textb = guiLabels['GUI_3b']
       labela = Tkinter.Label(master, fg=headerFG, font=headerFont, text=texta)
       labela.grid(row=0, columnspan=5)
       labelb = Tkinter.Label(master, fg=headerFG, font=headerFont, text=textb)
       labelb.grid(row=0, column=5, sticky=Tkinter.W)
       #frame.pack(fill=Tkinter.X, expand=Tkinter.YES)
       #frame.grid(row=0, columnspan=columns, sticky=Tkinter.W)
       row=row+1

       # Segments
       uiSegments = []
       #print "\n\nInfoDict", self._infoDict
       for segment in segments:
           widgets, uiSegment = self._makeSegmentFrame(master, segment)
           uiSegments.append((uiSegment))
           column=0
           for widget in widgets:
               widget.grid(sticky=Tkinter.N+Tkinter.W, row=row, column=column)
               column = column +1
           row=self._makeLine(master, row, columnspan=columns, width=190)
           row = row+1
       #segBox.pack(fill=Tkinter.X, expand=Tkinter.YES)
       return row, uiSegments

    def _makeSegmentFrame(self, master, segment):
       #frame = Tkinter.Frame(master, relief=Tkinter.GROOVE, borderwidth=1)
       segNum, areas, situation = segment
       situationLabel = situation['label']
       situationName = situation['name']

       widgets, addEntry, sitKey, hazardKeys = self._makeSegmentColumns(segNum, master, areas)

       # Situation
       situationFrame = Tkinter.Frame(master, relief=Tkinter.FLAT,borderwidth=1)
       if len(situationLabel) > 14:
           situationLabel = situationLabel[0:14] + "\n" + situationLabel[14:]
       label = Tkinter.Label(situationFrame, text=situationLabel, width=15)
       label.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO)
       #situationFrame.pack(side=Tkinter.LEFT, expand=Tkinter.NO,
       #    fill=Tkinter.Y, anchor=Tkinter.N)
       widgets.append(situationFrame)

       # Scenarios
       scenarioFrame = Tkinter.Frame(master, relief=Tkinter.FLAT, borderwidth=1, width=30)
       scenarios = situation["scenarios"]
       uiScenario = self._makeRadioOrCheckList(
           scenarioFrame, "", scenarios, buttonSide=Tkinter.TOP, frameSide=Tkinter.LEFT,
           listFrameRelief=Tkinter.FLAT)
       #scenarioFrame.pack(side=Tkinter.LEFT, expand=Tkinter.YES, fill=Tkinter.Y,
       #                   anchor=Tkinter.W)
       widgets.append(scenarioFrame)

       # Sections
       # If Abbreviated, no sections will be shown
       sectionFrame = Tkinter.Frame(master, relief=Tkinter.FLAT, borderwidth=1)
       if situationLabel == "Abbreviated":
           uiSections = None
           text = "                                                                                                               "
           label = Tkinter.Label(sectionFrame, text=text)
           label.pack(side=Tkinter.TOP, expand=Tkinter.NO,fill=Tkinter.Y, anchor=Tkinter.W)
       else:
           sectionList = self._parent._segmentSections()
           # Determine maximum label length
##           maxLen = 0
##           for section in sectionList:
##               label=section.get('label')
##               labelLen = len(label)
##               if labelLen > maxLen: maxLen = labelLen
##           text = str.ljust("   Prev", maxLen+40) + "Import"
##           print "text", maxLen, len(text), text
##           usePrevLabel = Tkinter.Label(sectionFrame, text=text, width=maxLen+2)
           #usePrevLabel = Tkinter.Label(sectionFrame, text="Prev Import", width=11)
           #usePrevLabel.pack(side=Tkinter.TOP, expand=Tkinter.NO,fill=Tkinter.Y, anchor=Tkinter.W)
           # Filter for sections to be displayed
           sections = []
           for section in sectionList:
               inSegments = section.get('inSegments', None)
               if inSegments in [None, 'always']:
                   continue
               # Check excludeFromSituations
               excluded = section.get('excludeFromSituations', [])
               if situationName in excluded:
                   continue
               # Check for whether or not to include
               # If ANY zone meets the "includeFor" criteria for the section,
               #  Then it will be included as an option
               includeFor = section.get("includeFor", None)
               if includeFor is None:
                   include = True
               elif type(includeFor) is types.MethodType:
                   name = section.get('name')
                   include = includeFor(name, areas)
               else:
                   include = False
                   for area in areas:
                       if area in includeFor:
                           include=True
                           break
               if include: sections.append(section)
           uiSections = self._makeCheckList(sectionFrame, "", sections, segmentAreas=areas)
       #sectionFrame.pack(side=Tkinter.LEFT, expand=Tkinter.NO,
       #     fill=Tkinter.Y, anchor=Tkinter.N)
       widgets.append(sectionFrame)

       #frame.pack(fill=Tkinter.X, expand=Tkinter.YES)
       return widgets, (segNum, areas, situation, uiScenario, scenarios, uiSections)

    def _makeButtons(self, master, row, columnspan):
       # create the basic dialog buttons the user sees (Ok, Cancel)
       frame = Tkinter.Frame(master)
       buttonList = self._parent._GUI3_configDict().get("buttonList", [])
       for button, label in buttonList:
           if button == "Ok":
               command = self.okCB
           else: # button == "Cancel":
               command = self.cancelCB
           Tkinter.Button(frame, text=label, command=command, width=10,
                          state=Tkinter.NORMAL).pack(side=Tkinter.LEFT, pady=5, padx=10)
       #frame.pack()
       frame.grid(row=row, columnspan=columnspan)

    def okCB(self):
       # pull the data before they get toasted
       tkObject_dict  = self._tkObject_dict

       segments = []
       for segNum, areas, situation, uiScenario, scenarios, uiSections in tkObject_dict["segments"]:
           extraInfo = {
               "usePrev_HU_S_Headline":situation.get("usePrev_HU_S_Headline",None),
               "userHeadline_HU_S":situation.get("userHeadline_HU_S",None),
               }
           #  Only need the situation name for varDict
           situation= situation["name"]
           scenarioObj, entryObj = uiScenario
           label, scenario = scenarios[scenarioObj.get()]
           sections = []
           if uiSections is not None:
               for name, iCheck, iOrder, prevVar, importVar in uiSections:
                   if iCheck.get():
                       try:
                           order = iOrder.get()
                       except:
                           order = None
                       try:
                           usePrev = prevVar.get()
                       except:
                           usePrev = 0
                       try:
                           useImport = importVar.get()
                       except:
                           useImport = None
                       sections.append((name, order, usePrev,
                                        useImport))
                       #print "usePrev, useImport", name, usePrev, useImport
           segments.append((segNum, areas, situation, scenario, sections, extraInfo))
       self._varDict["segments"] = segments
       LogStream.logVerbose("varDict=",self._varDict)

       # close window and set status "Ok"
       self._status = "Ok"
       self.ok()

