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
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "DU.Y", ["FLZ049"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ049-010300-",
      "/O.NEW.KTBW.DU.Y.0001.100101T0000Z-100101T0300Z/",
      "PASCO-",
      "700 PM EST THU DEC 31 2009",
      "...BLOWING DUST ADVISORY IN EFFECT UNTIL 10 PM EST THIS EVENING...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A BLOWING DUST ADVISORY...WHICH IS IN EFFECT UNTIL 10 PM EST THIS EVENING.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A BLOWING DUST ADVISORY MEANS THAT BLOWING DUST WILL RESTRICT VISIBILITIES. TRAVELERS ARE URGED TO USE CAUTION.",
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
       ("Fcst", "Hazards", "DISCRETE", 0, 36, "DU.Y", ["FLZ049"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ049-010800-",
      "/O.NEW.KTBW.DU.Y.0001.100101T0000Z-100102T1200Z/",
      "PASCO-",
      "700 PM EST THU DEC 31 2009",
      "...BLOWING DUST ADVISORY IN EFFECT UNTIL 7 AM EST SATURDAY...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A BLOWING DUST ADVISORY...WHICH IS IN EFFECT UNTIL 7 AM EST SATURDAY.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A BLOWING DUST ADVISORY MEANS THAT BLOWING DUST WILL RESTRICT VISIBILITIES. TRAVELERS ARE URGED TO USE CAUTION.",
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
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ049-010100-",
      "/O.CAN.KTBW.DU.Y.0001.000000T0000Z-100102T1200Z/",
      "PASCO-",
      "700 PM EST THU DEC 31 2009",
      "...BLOWING DUST ADVISORY IS CANCELLED...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE BLOWING DUST ADVISORY.",
      "|* SEGMENT TEXT GOES HERE *|.",
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
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "DU.Y", ["FLZ049"]),
       ("Fcst", "Hazards", "DISCRETE", 3, 6, "DS.W", ["FLZ049"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ049-010600-",
      "/O.NEW.KTBW.DS.W.0001.100101T0300Z-100101T0600Z/",
      "/O.NEW.KTBW.DU.Y.0002.100101T0000Z-100101T0300Z/",
      "PASCO-",
      "700 PM EST THU DEC 31 2009",
      "...BLOWING DUST ADVISORY IN EFFECT UNTIL 10 PM EST THIS EVENING...",
      "...DUST STORM WARNING IN EFFECT FROM 10 PM THIS EVENING TO 1 AM EST FRIDAY...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A DUST STORM WARNING...WHICH IS IN EFFECT FROM 10 PM THIS EVENING TO 1 AM EST FRIDAY. A BLOWING DUST ADVISORY HAS ALSO BEEN ISSUED. THIS BLOWING DUST ADVISORY IS IN EFFECT UNTIL 10 PM EST THIS EVENING.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A DUST STORM WARNING MEANS SEVERELY LIMITED VISIBILITIES ARE EXPECTED WITH BLOWING DUST. TRAVEL COULD BECOME EXTREMELY DANGEROUS. PERSONS WITH RESPIRATORY PROBLEMS SHOULD MAKE PREPARATIONS TO STAY INDOORS UNTIL THE STORM PASSES.",
      "A BLOWING DUST ADVISORY MEANS THAT BLOWING DUST WILL RESTRICT VISIBILITIES. TRAVELERS ARE URGED TO USE CAUTION.",
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
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "DU.Y", ["FLZ049"]),
       ("Fcst", "Hazards", "DISCRETE", 30, 60, "DS.W", ["FLZ049"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ049-010800-",
      "/O.NEW.KTBW.DS.W.0001.100102T0600Z-100103T1200Z/",
      "/O.NEW.KTBW.DU.Y.0002.100101T0000Z-100101T0300Z/",
      "PASCO-",
      "700 PM EST THU DEC 31 2009",
      "...BLOWING DUST ADVISORY IN EFFECT UNTIL 10 PM EST THIS EVENING...",
      "...DUST STORM WARNING IN EFFECT FROM 1 AM SATURDAY TO 7 AM EST SUNDAY...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A DUST STORM WARNING...WHICH IS IN EFFECT FROM 1 AM SATURDAY TO 7 AM EST SUNDAY. A BLOWING DUST ADVISORY HAS ALSO BEEN ISSUED. THIS BLOWING DUST ADVISORY IS IN EFFECT UNTIL 10 PM EST THIS EVENING.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A DUST STORM WARNING MEANS SEVERELY LIMITED VISIBILITIES ARE EXPECTED WITH BLOWING DUST. TRAVEL COULD BECOME EXTREMELY DANGEROUS. PERSONS WITH RESPIRATORY PROBLEMS SHOULD MAKE PREPARATIONS TO STAY INDOORS UNTIL THE STORM PASSES.",
      "A BLOWING DUST ADVISORY MEANS THAT BLOWING DUST WILL RESTRICT VISIBILITIES. TRAVELERS ARE URGED TO USE CAUTION.",
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
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "DU.Y", ["FLZ049"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ049-010300-",
      "/O.CAN.KTBW.DS.W.0001.100102T0600Z-100103T1200Z/",
      "/O.CON.KTBW.DU.Y.0002.000000T0000Z-100101T0300Z/",
      "PASCO-",
      "700 PM EST THU DEC 31 2009",
      "...BLOWING DUST ADVISORY REMAINS IN EFFECT UNTIL 10 PM EST THIS EVENING...",
      "...DUST STORM WARNING IS CANCELLED...",
#      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE DUST STORM WARNING. A BLOWING DUST ADVISORY REMAINS IN EFFECT UNTIL 10 PM EST THIS EVENING.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "A BLOWING DUST ADVISORY MEANS THAT BLOWING DUST WILL RESTRICT VISIBILITIES. TRAVELERS ARE URGED TO USE CAUTION.",
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
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "DU.Y", ["FLZ049"]),
       ("Fcst", "Hazards", "DISCRETE", 4, 5, "DU.Y", ["FLZ049"]),
       ],
    "checkStrings": [
      "WWUS72 KTBW 010000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "700 PM EST THU DEC 31 2009",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ049-010500-",
      "/O.CAN.KTBW.DS.W.0001.100102T0600Z-100103T1200Z/",
      "/O.NEW.KTBW.DU.Y.0003.100101T0400Z-100101T0500Z/",
      "/O.CON.KTBW.DU.Y.0002.000000T0000Z-100101T0300Z/",
      "PASCO-",
      "700 PM EST THU DEC 31 2009",
      "...BLOWING DUST ADVISORY REMAINS IN EFFECT UNTIL 10 PM EST THIS EVENING...",
      "...BLOWING DUST ADVISORY IN EFFECT FROM 11 PM THIS EVENING TO MIDNIGHT EST TONIGHT...",
      "...DUST STORM WARNING IS CANCELLED...",
#      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A BLOWING DUST ADVISORY...WHICH IS IN EFFECT FROM 11 PM THIS EVENING TO MIDNIGHT EST TONIGHT. THE DUST STORM WARNING HAS BEEN CANCELLED. A BLOWING DUST ADVISORY REMAINS IN EFFECT UNTIL 10 PM EST THIS EVENING.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "A BLOWING DUST ADVISORY MEANS THAT BLOWING DUST WILL RESTRICT VISIBILITIES. TRAVELERS ARE URGED TO USE CAUTION.",
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
        "database": "<site>_GRID__Official_00000000_0000",
        "publishGrids": 1,
        "decodeVTEC": 1,
        "gridsStartTime": "20100101_0000",
        "drtTime": "20100101_0000",
        "orderStrings": 1,
        "vtecMode": "O",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




