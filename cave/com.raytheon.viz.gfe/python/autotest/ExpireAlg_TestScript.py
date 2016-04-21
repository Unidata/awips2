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
# Expire Algorithm
#
# Author:
# ----------------------------------------------------------------------------

scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.",
    "name": "ExpireAlg_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "One active event starting now and ends sooner than the default product expiration",
    "name": "ExpireAlg_1ActiveShort",
    "productType": "Hazard_NPW_Local",
    "decodeVTEC": 0,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "DU.Y", ["FLZ149"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ149-010300-",
      "/O.NEW.KTBW.DU.Y.0001.100101T0000Z-100101T0300Z/",
      "Pasco-",
      "700 PM EST Thu Dec 31 2009",
      "...BLOWING DUST ADVISORY IN EFFECT UNTIL 10 PM EST THIS EVENING...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Blowing Dust Advisory...which is in effect until 10 PM EST this evening.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "A Blowing Dust Advisory means that blowing dust will restrict visibilities. Travelers are urged to use caution.",
      "$$",
       ],
    },

    {
    "commentary": "One active event starting now and ending after the default product expiration",
    "name": "ExpireAlg_1ActiveLong",
    "productType": "Hazard_NPW_Local",
    "decodeVTEC": 1,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 36, "DU.Y", ["FLZ149"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ149-010800-",
      "/O.NEW.KTBW.DU.Y.0001.100101T0000Z-100102T1200Z/",
      "Pasco-",
      "700 PM EST Thu Dec 31 2009",
      "...BLOWING DUST ADVISORY IN EFFECT UNTIL 7 AM EST SATURDAY...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Blowing Dust Advisory...which is in effect until 7 AM EST Saturday.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "A Blowing Dust Advisory means that blowing dust will restrict visibilities. Travelers are urged to use caution.",
      "$$",
       ],
    },

    {
    "commentary": "One active event that is cancelled",
    "name": "ExpireAlg_1Cancelled",
    "productType": "Hazard_NPW_Local",
    "decodeVTEC": 1,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ149-010100-",
      "/O.CAN.KTBW.DU.Y.0001.000000T0000Z-100102T1200Z/",
      "Pasco-",
      "700 PM EST Thu Dec 31 2009",
      "...BLOWING DUST ADVISORY IS CANCELLED...",
      "The National Weather Service in Tampa Bay Ruskin has cancelled the Blowing Dust Advisory.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
       ],
    },

    {
    "commentary": "Two active events, back to back, with the last one ending prior to the default product expiration time.",
    "name": "ExpireAlg_2ActiveShort",
    "productType": "Hazard_NPW_Local",
    "decodeVTEC": 0,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "DU.Y", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 3, 6, "DS.W", ["FLZ149"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ149-010600-",
      "/O.NEW.KTBW.DS.W.0001.100101T0300Z-100101T0600Z/",
      "/O.NEW.KTBW.DU.Y.0002.100101T0000Z-100101T0300Z/",
      "Pasco-",
      "700 PM EST Thu Dec 31 2009",
      "...BLOWING DUST ADVISORY IN EFFECT UNTIL 10 PM EST THIS EVENING...",
      "...DUST STORM WARNING IN EFFECT FROM 10 PM THIS EVENING TO 1 AM EST FRIDAY...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Dust Storm Warning...which is in effect from 10 PM this evening to 1 AM EST Friday. A Blowing Dust Advisory has also been issued. This Blowing Dust Advisory is in effect until 10 PM EST this evening.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "A Dust Storm Warning means severely limited visibilities are expected with blowing dust. Travel could become extremely dangerous. Persons with respiratory problems should make preparations to stay indoors until the storm passes.",
      "A Blowing Dust Advisory means that blowing dust will restrict visibilities. Travelers are urged to use caution.",
      "$$",
       ],
    },

    {
    "commentary": "Two active events, back to back, with the first event ending prior to the default product expiration time, and the second event ending after the default product expiration time.",
    "name": "ExpireAlg_2ActiveLong",
    "productType": "Hazard_NPW_Local",
    "decodeVTEC": 1,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "DU.Y", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 30, 60, "DS.W", ["FLZ149"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ149-010800-",
      "/O.NEW.KTBW.DS.W.0001.100102T0600Z-100103T1200Z/",
      "/O.NEW.KTBW.DU.Y.0002.100101T0000Z-100101T0300Z/",
      "Pasco-",
      "700 PM EST Thu Dec 31 2009",
      "...BLOWING DUST ADVISORY IN EFFECT UNTIL 10 PM EST THIS EVENING...",
      "...DUST STORM WARNING IN EFFECT FROM 1 AM SATURDAY TO 7 AM EST SUNDAY...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Dust Storm Warning...which is in effect from 1 AM Saturday to 7 AM EST Sunday. A Blowing Dust Advisory has also been issued. This Blowing Dust Advisory is in effect until 10 PM EST this evening.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "A Dust Storm Warning means severely limited visibilities are expected with blowing dust. Travel could become extremely dangerous. Persons with respiratory problems should make preparations to stay indoors until the storm passes.",
      "A Blowing Dust Advisory means that blowing dust will restrict visibilities. Travelers are urged to use caution.",
      "$$",
       ],
    },

    {
    "commentary": "One active events, with it ending prior to the default product expiration time, along with an event that has been cancelled (CAN)",
    "name": "ExpireAlg_1ActiveShort-CAN",
    "productType": "Hazard_NPW_Local",
    "decodeVTEC": 0,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "DU.Y", ["FLZ149"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ149-010300-",
      "/O.CAN.KTBW.DS.W.0001.100102T0600Z-100103T1200Z/",
      "/O.CON.KTBW.DU.Y.0002.000000T0000Z-100101T0300Z/",
      "Pasco-",
      "700 PM EST Thu Dec 31 2009",
      "...BLOWING DUST ADVISORY REMAINS IN EFFECT UNTIL 10 PM EST THIS EVENING...",
      "...DUST STORM WARNING IS CANCELLED...",
#      "The National Weather Service in Tampa Bay Ruskin has cancelled the Dust Storm Warning. A Blowing Dust Advisory remains in effect until 10 PM EST this evening.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "A Blowing Dust Advisory means that blowing dust will restrict visibilities. Travelers are urged to use caution.",
      "$$",
       ],
    },

    {
    "commentary": "Two active events, back to back, that end prior to the default product expiration time, with another event that has been cancelled (CAN)",
    "name": "ExpireAlg_2ActiveShort-CAN",
    "productType": "Hazard_NPW_Local",
    "decodeVTEC": 1,
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "DU.Y", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 4, 5, "DU.Y", ["FLZ149"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ149-010500-",
      "/O.CAN.KTBW.DS.W.0001.100102T0600Z-100103T1200Z/",
      "/O.NEW.KTBW.DU.Y.0003.100101T0400Z-100101T0500Z/",
      "/O.CON.KTBW.DU.Y.0002.000000T0000Z-100101T0300Z/",
      "Pasco-",
      "700 PM EST Thu Dec 31 2009",
      "...BLOWING DUST ADVISORY REMAINS IN EFFECT UNTIL 10 PM EST THIS EVENING...",
      "...BLOWING DUST ADVISORY IN EFFECT FROM 11 PM THIS EVENING TO MIDNIGHT EST TONIGHT...",
      "...DUST STORM WARNING IS CANCELLED...",
#      "The National Weather Service in Tampa Bay Ruskin has issued a Blowing Dust Advisory...which is in effect from 11 PM this evening to midnight EST tonight. THE Dust Storm Warning HAS BEEN CANCELLED. A Blowing Dust Advisory remains in effect until 10 PM EST this evening.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "A Blowing Dust Advisory means that blowing dust will restrict visibilities. Travelers are urged to use caution.",
      "$$",
       ],
    },

    {
    "commentary": "Deleting hazard grids.",
    "name": "ExpireAlg_Cleanup",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },
    ]

       
import TestScript
def testScript(self, dataMgr):
    defaults = {
        "database": "<site>_GRID__Fcst_00000000_0000",
        "publishGrids": 0,
        "decodeVTEC": 1,
        "gridsStartTime": "20100101_0000",
        "drtTime": "20100101_0000",
        "orderStrings": 1,
        "vtecMode": "O",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




