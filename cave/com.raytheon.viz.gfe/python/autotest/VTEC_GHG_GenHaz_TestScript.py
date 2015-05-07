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
       ("Fcst", "Hazards", "DISCRETE", 3, 5, "DU.Y", ["FLZ149"]),
       ],
    "checkStrings": [
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "1210 AM EST FRI JAN 1 2010",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ149-011000-",
      "/O.NEW.KTBW.DU.Y.0001.100101T0800Z-100101T1000Z/",
      "COASTAL PASCO-",
      "1210 AM EST FRI JAN 1 2010",
      "...Blowing Dust Advisory in effect until 5 AM EST early this morning...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Blowing Dust Advisory...which is in effect until 5 AM EST early this morning.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "Precautionary/preparedness actions...",
      "A Blowing Dust Advisory means that blowing dust will restrict visibilities. Travelers are urged to use caution.",
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
       ("Fcst", "Hazards", "DISCRETE", 3, 12, "DS.W", ["FLZ149"]),
       ],
    "checkStrings": [
       "NPWTBW",
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "1230 AM EST FRI JAN 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ149-011330-",
       "/O.UPG.KTBW.DU.Y.0001.100101T0800Z-100101T1000Z/",
       "/O.NEW.KTBW.DS.W.0001.100101T0800Z-100101T1700Z/",
       "COASTAL PASCO-",
       "1230 AM EST FRI JAN 1 2010",
       "...Dust Storm Warning in effect until noon EST today...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Dust Storm Warning...which is in effect until noon EST today. The Blowing Dust Advisory is no longer in effect.",
       "Precautionary/preparedness actions...",
       "A Dust Storm Warning means severely limited visibilities are expected with blowing dust. Travel could become extremely dangerous. Persons with respiratory problems should make preparations to stay indoors until the storm passes.",
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
       ("Fcst", "Hazards", "DISCRETE", 3, 15, "DS.W", ["FLZ149", "FLZ057"]),
       ],
    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "330 AM EST FRI JAN 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ057-011630-",
       "/O.EXB.KTBW.DS.W.0001.000000T0000Z-100101T2000Z/",
       "HIGHLANDS-",
#        "INCLUDING THE CITIES OF...SEBRING...AVON PARK...PLACID LAKES",
       "330 AM EST FRI JAN 1 2010",
       "...Dust Storm Warning in effect until 3 PM EST this afternoon...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Dust Storm Warning...which is in effect until 3 PM EST this afternoon.",
       "A Dust Storm Warning means severely limited visibilities are expected with blowing dust. Travel could become extremely dangerous. Persons with respiratory problems should make preparations to stay indoors until the storm passes.",
       "$$",
       "FLZ149-011630-",
       "/O.EXT.KTBW.DS.W.0001.000000T0000Z-100101T2000Z/",
       "COASTAL PASCO-",
       "330 AM EST FRI JAN 1 2010",
       "...Dust Storm Warning now in effect until 3 PM EST this afternoon...",
#        "The Dust Storm Warning is now in effect until 3 PM EST this afternoon.",
       "Precautionary/preparedness actions...",
       "A Dust Storm Warning means severely limited visibilities are expected with blowing dust. Travel could become extremely dangerous. Persons with respiratory problems should make preparations to stay indoors until the storm passes.",
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
       ("Fcst", "Hazards", "DISCRETE", 3, 15, "DU.Y", ["FLZ149", "FLZ057"]),
       ],
    "checkStrings": [
       "URGENT - WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "600 AM EST FRI JAN 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ057-149-011900-",
       "/O.CAN.KTBW.DS.W.0001.000000T0000Z-100101T2000Z/",
       "/O.NEW.KTBW.DU.Y.0002.100101T1100Z-100101T2000Z/",
       "HIGHLANDS-COASTAL PASCO-",
       "600 AM EST FRI JAN 1 2010",
       "...Blowing Dust Advisory in effect until 3 PM EST this afternoon...",
       "...Dust Storm Warning is cancelled...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Blowing Dust Advisory...which is in effect until 3 PM EST this afternoon. The Dust Storm Warning has been cancelled.",
       "Precautionary/preparedness actions...",
       "A Blowing Dust Advisory means that blowing dust will restrict visibilities. Travelers are urged to use caution.",
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
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ149-011500-",
       "/O.CAN.KTBW.DU.Y.0002.000000T0000Z-100101T2000Z/",
       "COASTAL PASCO-",
       "900 AM EST FRI JAN 1 2010",
       "...Blowing Dust Advisory is cancelled...",
       "The National Weather Service in Tampa Bay Ruskin has cancelled the Blowing Dust Advisory.",
       "$$",
       "FLZ057-012200-",
       "/O.NEW.KTBW.HW.A.0001.100103T0600Z-100104T0800Z/",
       "/O.CON.KTBW.DU.Y.0002.000000T0000Z-100101T2000Z/",
       "HIGHLANDS-",
       "900 AM EST FRI JAN 1 2010",
       "...Blowing Dust Advisory remains in effect until 3 PM EST this afternoon...",
       "...High Wind Watch in effect from late Saturday night through late Sunday night...",
       "The National Weather Service in Tampa Bay Ruskin has issued a High Wind Watch...which is in effect from late Saturday night through late Sunday night.",
       "Precautionary/preparedness actions...",
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
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ057-020400-",
       "/O.EXP.KTBW.DU.Y.0002.000000T0000Z-100101T2000Z/",
       "/O.EXT.KTBW.HW.A.0001.100103T0200Z-100104T0800Z/",
       "HIGHLANDS-",
       "250 PM EST FRI JAN 1 2010",
       "...High Wind Watch now in effect from Saturday evening through late Sunday night...",
       "...Blowing Dust Advisory will expire at 3 PM EST this afternoon...",
#        "The Blowing Dust Advisory will expire at 3 PM EST this afternoon. The High Wind Watch is now in effect from Saturday evening through late Sunday night.",
       "Precautionary/preparedness actions...",
       "A High Wind Watch means there is the potential for a hazardous high wind event. Sustained winds of at least 40 mph...or gusts of 58 mph or stronger may occur. Continue to monitor the latest forecasts.",
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
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ057-020400-",
       "/O.EXP.KTBW.DU.Y.0002.000000T0000Z-100101T2000Z/",
       "/O.EXT.KTBW.HW.A.0001.100103T0200Z-100104T0800Z/",
       "HIGHLANDS-",
       "300 PM EST FRI JAN 1 2010",
       "...High Wind Watch now in effect from Saturday evening through late Sunday night...",
       "...Blowing Dust Advisory has expired...",
#        "The Blowing Dust Advisory is no longer in effect. The High Wind Watch is now in effect from Saturday evening through late Sunday night.",
       "Precautionary/preparedness actions...",
       "A High Wind Watch means there is the potential for a hazardous high wind event. Sustained winds of at least 40 mph...or gusts of 58 mph or stronger may occur. Continue to monitor the latest forecasts.",
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
       "...Blowing Dust Advisory in effect until noon EST today...",
       "...High Wind Watch in effect this evening...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Blowing Dust Advisory...which is in effect until noon EST today. A High Wind Watch has also been issued. This High Wind Watch is in effect this evening.",
       "Precautionary/preparedness actions...",
       "A Blowing Dust Advisory means that blowing dust will restrict visibilities. Travelers are urged to use caution.",
       "A High Wind Watch means there is the potential for a hazardous high wind event. Sustained winds of at least 40 mph...or gusts of 58 mph or stronger may occur. Continue to monitor the latest forecasts.",
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
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ057-011900-",
       "/O.NEW.KTBW.DU.Y.0001.100101T1100Z-100101T1700Z/",
       "/O.NEW.KTBW.HW.A.0001.100101T2300Z-100102T0500Z/",
       "/O.NEW.KTBW.HW.A.0002.100102T1700Z-100103T0500Z/",
       "HIGHLANDS-",
       "600 AM EST FRI JAN 1 2010",
       "...Blowing Dust Advisory in effect until noon EST today...",
       "...High Wind Watch in effect this evening...",
       "...High Wind Watch in effect from Saturday afternoon through Saturday evening...",
       "The National Weather Service in Tampa Bay Ruskin has issued a Blowing Dust Advisory...which is in effect until noon EST today. A High Wind Watch has also been issued. This High Wind Watch is in effect this evening. In addition...a High Wind Watch has been issued. This High Wind Watch is in effect from Saturday afternoon through Saturday evening.",
#        "|* SEGMENT TEXT GOES HERE *|.",
       "Precautionary/preparedness actions...",
       "A Blowing Dust Advisory means that blowing dust will restrict visibilities. Travelers are urged to use caution.",
       "A High Wind Watch means there is the potential for a hazardous high wind event. Sustained winds of at least 40 mph...or gusts of 58 mph or stronger may occur. Continue to monitor the latest forecasts.",
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
        "database": "<site>_GRID__Fcst_00000000_0000",
        "publishGrids": 0,
        "decodeVTEC": 1,
        "gridsStartTime": "20100101_0500",
        "orderStrings": 1,
        "vtecMode": "O",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




