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
# Headlines Timing
#
# Author:
# ----------------------------------------------------------------------------

scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.",
    "name": "Hazard_GenHaz_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },
    {
    "commentary": "NEW GenHaz",
    "name": "Hazard_GenHaz_1",
    "drtTime": "20100101_0510",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 3, 5, "DU.Y", ["FLZ049"]),
       ],
    "checkStrings": [
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "1210 AM EST FRI JAN 1 2010",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ049-011000-",
      "/O.NEW.KTBW.DU.Y.0001.100101T0800Z-100101T1000Z/",
      "PASCO-",
      "1210 AM EST FRI JAN 1 2010",
      "...BLOWING DUST ADVISORY IN EFFECT UNTIL 5 AM EST EARLY THIS MORNING...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A BLOWING DUST ADVISORY...WHICH IS IN EFFECT UNTIL 5 AM EST EARLY THIS MORNING. ",
      "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A BLOWING DUST ADVISORY MEANS THAT BLOWING DUST WILL RESTRICT VISIBILITIES. TRAVELERS ARE URGED TO USE CAUTION.",
      "&&",
      "$$",
       ],
    },

    {
    "commentary": "EXT GenHaz",
    "name": "Hazard_GenHaz_2",
    "drtTime": "20100101_0530",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 3, 12, "DS.W", ["FLZ049"]),
       ],
    "checkStrings": [
       "NPWTBW",
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "1230 AM EST FRI JAN 1 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ049-011330-",
       "/O.UPG.KTBW.DU.Y.0001.100101T0800Z-100101T1000Z/",
       "/O.NEW.KTBW.DS.W.0001.100101T0800Z-100101T1700Z/",
       "PASCO-",
       "1230 AM EST FRI JAN 1 2010",
       "...DUST STORM WARNING IN EFFECT UNTIL NOON EST TODAY...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A DUST STORM WARNING...WHICH IS IN EFFECT UNTIL NOON EST TODAY. THE BLOWING DUST ADVISORY IS NO LONGER IN EFFECT. ",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A DUST STORM WARNING MEANS SEVERELY LIMITED VISIBILITIES ARE EXPECTED WITH BLOWING DUST. TRAVEL COULD BECOME EXTREMELY DANGEROUS. PERSONS WITH RESPIRATORY PROBLEMS SHOULD MAKE PREPARATIONS TO STAY INDOORS UNTIL THE STORM PASSES.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "EXA/EXT GenHaz",
    "name": "Hazard_GenHaz_3",
    "drtTime": "20100101_0830",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 3, 15, "DS.W", ["FLZ049", "FLZ057"]),
       ],
    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "330 AM EST FRI JAN 1 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ057-011630-",
       "/O.EXB.KTBW.DS.W.0001.000000T0000Z-100101T2000Z/",
       "HIGHLANDS-",
       "INCLUDING THE CITIES OF...SEBRING...AVON PARK...PLACID LAKES",
       "330 AM EST FRI JAN 1 2010",
       "...DUST STORM WARNING IN EFFECT UNTIL 3 PM EST THIS AFTERNOON...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A DUST STORM WARNING...WHICH IS IN EFFECT UNTIL 3 PM EST THIS AFTERNOON. ",
       "A DUST STORM WARNING MEANS SEVERELY LIMITED VISIBILITIES ARE EXPECTED WITH BLOWING DUST. TRAVEL COULD BECOME EXTREMELY DANGEROUS. PERSONS WITH RESPIRATORY PROBLEMS SHOULD MAKE PREPARATIONS TO STAY INDOORS UNTIL THE STORM PASSES.",
       "$$",
       "FLZ049-011630-",
       "/O.EXT.KTBW.DS.W.0001.000000T0000Z-100101T2000Z/",
       "PASCO-",
       "330 AM EST FRI JAN 1 2010",
       "...DUST STORM WARNING NOW IN EFFECT UNTIL 3 PM EST THIS AFTERNOON...",
       "THE DUST STORM WARNING IS NOW IN EFFECT UNTIL 3 PM EST THIS AFTERNOON. ",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A DUST STORM WARNING MEANS SEVERELY LIMITED VISIBILITIES ARE EXPECTED WITH BLOWING DUST. TRAVEL COULD BECOME EXTREMELY DANGEROUS. PERSONS WITH RESPIRATORY PROBLEMS SHOULD MAKE PREPARATIONS TO STAY INDOORS UNTIL THE STORM PASSES.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "downgrade GenHaz",
    "name": "Hazard_GenHaz_4",
    "drtTime": "20100101_1100",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 3, 15, "DU.Y", ["FLZ049", "FLZ057"]),
       ],
    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "600 AM EST FRI JAN 1 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ049-057-011900-",
       "/O.CAN.KTBW.DS.W.0001.000000T0000Z-100101T2000Z/",
       "/O.NEW.KTBW.DU.Y.0002.100101T1100Z-100101T2000Z/",
       "PASCO-HIGHLANDS-",
       "600 AM EST FRI JAN 1 2010",
       "...BLOWING DUST ADVISORY IN EFFECT UNTIL 3 PM EST THIS AFTERNOON...",
       "...DUST STORM WARNING IS CANCELLED...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A BLOWING DUST ADVISORY...WHICH IS IN EFFECT UNTIL 3 PM EST THIS AFTERNOON. THE DUST STORM WARNING HAS BEEN CANCELLED. ",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A BLOWING DUST ADVISORY MEANS THAT BLOWING DUST WILL RESTRICT VISIBILITIES. TRAVELERS ARE URGED TO USE CAUTION.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "CAN 1 area GenHaz, NEW watch",
    "name": "Hazard_GenHaz_5",
    "drtTime": "20100101_1400",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 3, 15, "DU.Y", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 49, 75, "HW.A", ["FLZ057"]),
       ],
    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "900 AM EST FRI JAN 1 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ049-011500-",
       "/O.CAN.KTBW.DU.Y.0002.000000T0000Z-100101T2000Z/",
       "PASCO-",
       "900 AM EST FRI JAN 1 2010",
       "...BLOWING DUST ADVISORY IS CANCELLED...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE BLOWING DUST ADVISORY. ",
       "$$",
       "FLZ057-012200-",
       "/O.NEW.KTBW.HW.A.0001.100103T0600Z-100104T0800Z/",
       "/O.CON.KTBW.DU.Y.0002.000000T0000Z-100101T2000Z/",
       "HIGHLANDS-",
       "900 AM EST FRI JAN 1 2010",
       "...BLOWING DUST ADVISORY REMAINS IN EFFECT UNTIL 3 PM EST THIS AFTERNOON...",
       "...HIGH WIND WATCH IN EFFECT FROM LATE SATURDAY NIGHT THROUGH LATE SUNDAY NIGHT...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HIGH WIND WATCH...WHICH IS IN EFFECT FROM LATE SATURDAY NIGHT THROUGH LATE SUNDAY NIGHT. A BLOWING DUST ADVISORY REMAINS IN EFFECT UNTIL 3 PM EST THIS AFTERNOON.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "EXP GenHaz before",
    "name": "Hazard_GenHaz_6",
    "drtTime": "20100101_1950",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 3, 15, "DU.Y", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 45, 75, "HW.A", ["FLZ057"]),
       ],
    "decodeVTEC": 0,  #turn vtec decoder off
    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "250 PM EST FRI JAN 1 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ057-020400-",
       "/O.EXP.KTBW.DU.Y.0002.000000T0000Z-100101T2000Z/",
       "/O.EXT.KTBW.HW.A.0001.100103T0200Z-100104T0800Z/",
       "HIGHLANDS-",
       "250 PM EST FRI JAN 1 2010",
       "...HIGH WIND WATCH NOW IN EFFECT FROM SATURDAY EVENING THROUGH LATE SUNDAY NIGHT...",
       "...BLOWING DUST ADVISORY WILL EXPIRE AT 3 PM EST THIS AFTERNOON...",
       "THE BLOWING DUST ADVISORY WILL EXPIRE AT 3 PM EST THIS AFTERNOON. THE HIGH WIND WATCH IS NOW IN EFFECT FROM SATURDAY EVENING THROUGH LATE SUNDAY NIGHT. ",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A HIGH WIND WATCH MEANS THERE IS THE POTENTIAL FOR A HAZARDOUS HIGH WIND EVENT. SUSTAINED WINDS OF AT LEAST 40 MPH...OR GUSTS OF 58 MPH OR STRONGER MAY OCCUR. CONTINUE TO MONITOR THE LATEST FORECASTS.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "EXP GenHaz after, EXT/EXA HW.A",
    "name": "Hazard_GenHaz_7",
    "drtTime": "20100101_2000",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 3, 15, "DU.Y", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 45, 75, "HW.A", ["FLZ057"]),
       ],
    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "300 PM EST FRI JAN 1 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ057-020400-",
       "/O.EXP.KTBW.DU.Y.0002.000000T0000Z-100101T2000Z/",
       "/O.EXT.KTBW.HW.A.0001.100103T0200Z-100104T0800Z/",
       "HIGHLANDS-",
       "300 PM EST FRI JAN 1 2010",
       "...HIGH WIND WATCH NOW IN EFFECT FROM SATURDAY EVENING THROUGH LATE SUNDAY NIGHT...",
       "...BLOWING DUST ADVISORY HAS EXPIRED...",
       "THE BLOWING DUST ADVISORY IS NO LONGER IN EFFECT. THE HIGH WIND WATCH IS NOW IN EFFECT FROM SATURDAY EVENING THROUGH LATE SUNDAY NIGHT.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A HIGH WIND WATCH MEANS THERE IS THE POTENTIAL FOR A HAZARDOUS HIGH WIND EVENT. SUSTAINED WINDS OF AT LEAST 40 MPH...OR GUSTS OF 58 MPH OR STRONGER MAY OCCUR. CONTINUE TO MONITOR THE LATEST FORECASTS.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "Deleting hazard grids.",
    "name": "Hazard_GenHaz_Cleanup1",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },

    {
    "commentary": "Words for 2 NEWS",
    "name": "Hazard_GenHaz_8",
    "drtTime": "20100101_1100",
    "decodeVTEC": 0,  #turn vtec decoder off
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 6, 12, "DU.Y", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 18, 24, "HW.A", ["FLZ057"]),
       ],
    "checkStrings": [
       "FLZ057-011900-",
       "/O.NEW.KTBW.DU.Y.0001.100101T1100Z-100101T1700Z/",
       "/O.NEW.KTBW.HW.A.0001.100101T2300Z-100102T0500Z/",
       "HIGHLANDS-",
       "600 AM EST FRI JAN 1 2010",
       "...BLOWING DUST ADVISORY IN EFFECT UNTIL NOON EST TODAY...",
       "...HIGH WIND WATCH IN EFFECT THIS EVENING...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A BLOWING DUST ADVISORY...WHICH IS IN EFFECT UNTIL NOON EST TODAY. A HIGH WIND WATCH HAS ALSO BEEN ISSUED. THIS HIGH WIND WATCH IS IN EFFECT THIS EVENING. ",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A BLOWING DUST ADVISORY MEANS THAT BLOWING DUST WILL RESTRICT VISIBILITIES. TRAVELERS ARE URGED TO USE CAUTION.",
       "A HIGH WIND WATCH MEANS THERE IS THE POTENTIAL FOR A HAZARDOUS HIGH WIND EVENT. SUSTAINED WINDS OF AT LEAST 40 MPH...OR GUSTS OF 58 MPH OR STRONGER MAY OCCUR. CONTINUE TO MONITOR THE LATEST FORECASTS.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "Words for 3 NEWS",
    "name": "Hazard_GenHaz_9",
    "drtTime": "20100101_1100",
    "decodeVTEC": 0,  #turn vtec decoder off
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 6, 12, "DU.Y", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 18, 24, "HW.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 36, 48, "HW.A", ["FLZ057"]),
       ],
    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "600 AM EST FRI JAN 1 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ057-011900-",
       "/O.NEW.KTBW.DU.Y.0001.100101T1100Z-100101T1700Z/",
       "/O.NEW.KTBW.HW.A.0001.100101T2300Z-100102T0500Z/",
       "/O.NEW.KTBW.HW.A.0002.100102T1700Z-100103T0500Z/",
       "HIGHLANDS-",
       "600 AM EST FRI JAN 1 2010",
       "...BLOWING DUST ADVISORY IN EFFECT UNTIL NOON EST TODAY...",
       "...HIGH WIND WATCH IN EFFECT THIS EVENING...",
       "...HIGH WIND WATCH IN EFFECT FROM SATURDAY AFTERNOON THROUGH SATURDAY EVENING...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A BLOWING DUST ADVISORY...WHICH IS IN EFFECT UNTIL NOON EST TODAY. A HIGH WIND WATCH HAS ALSO BEEN ISSUED. THIS HIGH WIND WATCH IS IN EFFECT THIS EVENING. IN ADDITION...A HIGH WIND WATCH HAS BEEN ISSUED. THIS HIGH WIND WATCH IS IN EFFECT FROM SATURDAY AFTERNOON THROUGH SATURDAY EVENING. ",
       "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A BLOWING DUST ADVISORY MEANS THAT BLOWING DUST WILL RESTRICT VISIBILITIES. TRAVELERS ARE URGED TO USE CAUTION.",
       "A HIGH WIND WATCH MEANS THERE IS THE POTENTIAL FOR A HAZARDOUS HIGH WIND EVENT. SUSTAINED WINDS OF AT LEAST 40 MPH...OR GUSTS OF 58 MPH OR STRONGER MAY OCCUR. CONTINUE TO MONITOR THE LATEST FORECASTS.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "Deleting hazard grids.",
    "name": "Hazard_GenHaz_Cleanup",
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
        "gridsStartTime": "20100101_0500",
        "orderStrings": 1,
        "vtecMode": "O",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




