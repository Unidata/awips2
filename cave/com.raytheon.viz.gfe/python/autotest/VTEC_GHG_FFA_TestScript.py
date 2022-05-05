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

#set up to test area names and part of states
# without locationName defined
areaT1 = """
AreaDictionary['FLZ050']['fullStateName'] = 'Florida'
AreaDictionary['FLZ050']['partOfState'] = 'western'

AreaDictionary['FLZ057']['fullStateName'] = 'Florida'
AreaDictionary['FLZ057']['partOfState'] = 'western'

AreaDictionary['FLZ160']['fullStateName'] = 'Florida'
AreaDictionary['FLZ160']['partOfState'] = 'central'

AreaDictionary['FLZ151']['fullStateName'] = 'Florida'
AreaDictionary['FLZ151']['partOfState'] = 'central'

AreaDictionary['FLZ043']['fullStateName'] = 'Florida'
AreaDictionary['FLZ043']['partOfState'] = 'central'

AreaDictionary['FLZ162']['fullStateName'] = 'Florida'
AreaDictionary['FLZ162']['partOfState'] = 'central'

AreaDictionary['FLZ165']['fullStateName'] = 'Florida'
AreaDictionary['FLZ165']['partOfState'] = 'central'

AreaDictionary['FLZ056']['fullStateName'] = 'Florida'
AreaDictionary['FLZ056']['partOfState'] = 'southern'

AreaDictionary['FLZ052']['fullStateName'] = 'Georgia'
AreaDictionary['FLZ052']['partOfState'] = 'western'

AreaDictionary['FLZ155']['fullStateName'] = 'Georgia'
AreaDictionary['FLZ155']['partOfState'] = 'western'

AreaDictionary['FLZ061']['fullStateName'] = 'Georgia'
AreaDictionary['FLZ061']['partOfState'] = 'southern'

AreaDictionary['FLZ148']['fullStateName'] = 'Georgia'
AreaDictionary['FLZ148']['partOfState'] = 'southern'

AreaDictionary['FLZ142']['fullStateName'] = 'South Carolina'
AreaDictionary['FLZ142']['partOfState'] = 'western'

AreaDictionary['FLZ043']['fullStateName'] = 'South Carolina'
AreaDictionary['FLZ043']['partOfState'] = 'western'

"""
#with location name defined
areaT2= """
AreaDictionary['FLZ050']['fullStateName'] = 'Florida'
AreaDictionary['FLZ050']['partOfState'] = 'western'
AreaDictionary['FLZ050']['locationName'] = 'Clearfield'

AreaDictionary['FLZ057']['fullStateName'] = 'Florida'
AreaDictionary['FLZ057']['partOfState'] = 'western'
AreaDictionary['FLZ057']['locationName'] = 'Clearfield'

AreaDictionary['FLZ160']['fullStateName'] = 'Florida'
AreaDictionary['FLZ160']['partOfState'] = 'central'
AreaDictionary['FLZ160']['locationName'] = 'Aunt Ruby'

AreaDictionary['FLZ151']['fullStateName'] = 'Florida'
AreaDictionary['FLZ151']['partOfState'] = 'central'
AreaDictionary['FLZ151']['locationName'] = 'Aunt Ruby'

AreaDictionary['FLZ043']['fullStateName'] = 'Florida'
AreaDictionary['FLZ043']['partOfState'] = 'central'
AreaDictionary['FLZ043']['locationName'] = 'Adams'

AreaDictionary['FLZ162']['fullStateName'] = 'Florida'
AreaDictionary['FLZ162']['partOfState'] = 'central'
AreaDictionary['FLZ162']['locationName'] = 'Adams'

AreaDictionary['FLZ165']['fullStateName'] = 'Florida'
AreaDictionary['FLZ165']['partOfState'] = 'central'
#AreaDictionary['FLZ165']['locationName'] = 'western'

AreaDictionary['FLZ056']['fullStateName'] = 'Florida'
AreaDictionary['FLZ056']['partOfState'] = 'southern'
AreaDictionary['FLZ056']['locationName'] = 'Tampa'

AreaDictionary['FLZ052']['fullStateName'] = 'Georgia'
AreaDictionary['FLZ052']['partOfState'] = 'western'
AreaDictionary['FLZ052']['locationName'] = 'Tampa'

AreaDictionary['FLZ155']['fullStateName'] = 'Georgia'
AreaDictionary['FLZ155']['partOfState'] = 'western'
AreaDictionary['FLZ155']['locationName'] = 'Atlanta'

AreaDictionary['FLZ061']['fullStateName'] = 'Georgia'
AreaDictionary['FLZ061']['partOfState'] = 'southern'
AreaDictionary['FLZ061']['locationName'] = 'Beach'

AreaDictionary['FLZ148']['fullStateName'] = 'Georgia'
AreaDictionary['FLZ148']['partOfState'] = 'southern'
AreaDictionary['FLZ148']['locationName'] = 'Beach'

AreaDictionary['FLZ142']['fullStateName'] = 'South Carolina'
AreaDictionary['FLZ142']['partOfState'] = 'western'
AreaDictionary['FLZ142']['locationName'] = 'South Park'

AreaDictionary['FLZ043']['fullStateName'] = 'South Carolina'
AreaDictionary['FLZ043']['partOfState'] = 'western'
AreaDictionary['FLZ043']['locationName'] = 'South Park'

"""

#for testing of parishes, counties, and areas
areaT3 = """
AreaDictionary['FLC017']['fullStateName'] = 'Louisiana'
AreaDictionary['FLC017']['partOfState'] = 'western'
AreaDictionary['FLC017']['independentCity'] = 1

AreaDictionary['FLC105']['fullStateName'] = 'Louisiana'
AreaDictionary['FLC105']['partOfState'] = 'western'

AreaDictionary['FLC027']['fullStateName'] = 'Louisiana'
AreaDictionary['FLC027']['partOfState'] = 'western'

AreaDictionary['FLC053']['fullStateName'] = 'Florida'
AreaDictionary['FLC053']['partOfState'] = 'western'

"""
areaT3FIPS0= '#Definition["areaType"] = "FIPS"'
areaT3FIPS1= 'Definition["areaType"] = "FIPS"'




scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.",
    "name": "Hazard_FFA_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },
    {
    "commentary": "NEW FFA",
    "name": "Hazard_FFA_1",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ149"]),
       ],
    "checkStrings": ["URGENT - IMMEDIATE BROADCAST REQUESTED",
                     "Flood Watch",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLZ149-",
                     "/X.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
                     "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "Coastal Pasco-",
                     "1210 AM EST Fri Jan 1 2010",
                     "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a",
                     "* Flood Watch for a portion of west central Florida, including the following area, Coastal Pasco.",
                     "* Until 3 AM EST early this morning",
                     ],
    },
    {
    "commentary": "CON FFA",
    "name": "Hazard_FFA_2",
    "drtTime": "20100101_0530",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'SM '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ149"]),
       ],
    "checkStrings": ["Flood Watch",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLZ149-",
                     "/X.CON.KTBW.FA.A.0001.000000T0000Z-100101T0800Z/",
                     "/00000.0.SM.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLOOD WATCH REMAINS IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
                     "The Flood Watch continues for",
                     "* A portion of west central Florida, including the following area, Coastal Pasco.",
                     "* Until 3 AM EST early this morning",
                     ],
    },
    {
    "commentary": "EXA FFA",
    "name": "Hazard_FFA_3",
    "drtTime": "20100101_0700",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'DM '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ149","FLZ057"]),
       ],
    "checkStrings": ["URGENT - IMMEDIATE BROADCAST REQUESTED",
                     "Flood Watch",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLZ057-",
                     "/X.EXA.KTBW.FA.A.0001.000000T0000Z-100101T0800Z/",
                     "/00000.0.DM.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
                     "The National Weather Service in Tampa Bay Ruskin has expanded the",
                     "* Flood Watch to include a portion of south central Florida, including the following area, Highlands.",
                     "* Until 3 AM EST early this morning",
                     "FLZ149-",
                     "/X.CON.KTBW.FA.A.0001.000000T0000Z-100101T0800Z/",
                     "/00000.0.DM.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLOOD WATCH REMAINS IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
                     "The Flood Watch continues for",
                     "* A portion of west central Florida, including the following area, Coastal Pasco.",
                     "* Until 3 AM EST early this morning",
                     ],
    },
    {
    "commentary": "CAN FFA, NEW FFA",
    "name": "Hazard_FFA_4",
    "drtTime": "20100101_0720",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'IJ '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 8, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 24, 32, "FF.A", ["FLZ057"]),
       ],
    "checkStrings": ["URGENT - IMMEDIATE BROADCAST REQUESTED",
                     "Flood Watch",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLZ057-",
                     "/X.CAN.KTBW.FA.A.0001.000000T0000Z-100101T0800Z/",
                     "/X.NEW.KTBW.FF.A.0001.100101T0720Z-100101T1300Z/",
                     "/X.NEW.KTBW.FF.A.0002.100102T0500Z-100102T1300Z/",
                     "/00000.0.IJ.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLASH FLOOD WATCH IN EFFECT UNTIL 8 AM EST THIS MORNING...",
                     "...FLASH FLOOD WATCH IN EFFECT FROM LATE TONIGHT THROUGH SATURDAY MORNING...",
                     "...FLOOD WATCH IS CANCELLED...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a",
                     "* Flash Flood Watch for a portion of south central Florida, including the following area, Highlands.",
                     "* Until 8 AM EST this morning",
                     "The National Weather Service in Tampa Bay Ruskin has issued a",
                     "* Flash Flood Watch for a portion of south central Florida, including the following area, Highlands.",
                     "* From late tonight through Saturday morning",
                     "The Flood Watch for a portion of south central Florida has been cancelled.",
                     "FLZ149-",
                     "/X.CAN.KTBW.FA.A.0001.000000T0000Z-100101T0800Z/",
                     "/00000.0.IJ.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLOOD WATCH IS CANCELLED...",
                     "The Flood Watch for a portion of west central Florida has been cancelled."
                     ],
    },
    {
    "commentary": "EXP FFA, 2 NEW FFA",
    "name": "Hazard_FFA_5",
    "drtTime": "20100101_1300",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'FS '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 24, 32, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 46, 62, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 45, 46, "FA.A", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 46, 62, "FA.A", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 62, 68, "FA.A", ["FLZ149"]),
       ],
    "checkStrings": ["URGENT - IMMEDIATE BROADCAST REQUESTED",
                     "Flood Watch",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLZ057-",
                     "/X.EXP.KTBW.FF.A.0001.000000T0000Z-100101T1300Z/",
                     "/X.NEW.KTBW.FF.A.0003.100103T0300Z-100103T1900Z/",
                     "/X.CON.KTBW.FF.A.0002.100102T0500Z-100102T1300Z/",
                     "/00000.0.FS.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLASH FLOOD WATCH REMAINS IN EFFECT FROM LATE TONIGHT THROUGH SATURDAY MORNING...",
                     "...FLASH FLOOD WATCH IN EFFECT FROM SATURDAY EVENING THROUGH SUNDAY AFTERNOON...",
                     "...FLASH FLOOD WATCH HAS EXPIRED...",
                     "The Flash Flood Watch continues for",
                     "* A portion of south central Florida, including the following area, Highlands.",
                     "* From late tonight through Saturday morning",
                     "The National Weather Service in Tampa Bay Ruskin has issued a",
                     "* Flash Flood Watch for a portion of south central Florida, including the following area, Highlands.",
                     "* From Saturday evening through Sunday afternoon",
                     "The Flash Flood Watch for a portion of south central Florida has expired.",
                     "FLZ149-",
                     "/X.NEW.KTBW.FA.A.0002.100103T0200Z-100104T0100Z/",
                     "/00000.0.FS.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLOOD WATCH IN EFFECT FROM SATURDAY EVENING THROUGH SUNDAY EVENING...",
                     "The National Weather Service in Tampa Bay Ruskin has issued a",
                     "* Flood Watch for a portion of west central Florida, including the following area, Coastal Pasco.",
                     "* From Saturday evening through Sunday evening",
                     ],
    },
    {
    "commentary": "CON test of multiple events",
    "name": "Hazard_FFA_6",
    "drtTime": "20100102_0300",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'RS '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 24, 32, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 46, 62, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 45, 46, "FA.A", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 46, 62, "FA.A", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 62, 68, "FA.A", ["FLZ149"]),
       ],
    "checkStrings": ["Flood Watch",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLZ057-",
                     "/X.CON.KTBW.FF.A.0002.100102T0500Z-100102T1300Z/",
                     "/X.CON.KTBW.FF.A.0003.100103T0300Z-100103T1900Z/",
                     "/00000.0.RS.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLASH FLOOD WATCH REMAINS IN EFFECT UNTIL 8 AM EST SATURDAY...",
                     "...FLASH FLOOD WATCH REMAINS IN EFFECT FROM SATURDAY EVENING THROUGH SUNDAY AFTERNOON...",
                     "The Flash Flood Watch continues for",
                     "* A portion of south central Florida, including the following area, Highlands.",
                     "* Until 8 AM EST Saturday",
                     "The Flash Flood Watch continues for",
                     "* A portion of south central Florida, including the following area, Highlands.",
                     "* From Saturday evening through Sunday afternoon",
                     "FLZ149-",
                     "/X.CON.KTBW.FA.A.0002.100103T0200Z-100104T0100Z/",
                     "/00000.0.RS.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLOOD WATCH REMAINS IN EFFECT FROM SATURDAY EVENING THROUGH SUNDAY EVENING...",
                     "The Flood Watch continues for",
                     "* A portion of west central Florida, including the following area, Coastal Pasco.",
                     "* From Saturday evening through Sunday evening",
                     ],
    },
    {
    "commentary": "middle of 1st event",
    "name": "Hazard_FFA_7",
    "drtTime": "20100102_0700",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 24, 32, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 46, 62, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 45, 46, "FA.A", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 46, 62, "FA.A", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 62, 68, "FA.A", ["FLZ149"]),
       ],
    "checkStrings": ["Flood Watch",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLZ057-",
                     "/X.CON.KTBW.FF.A.0002.000000T0000Z-100102T1300Z/",
                     "/X.CON.KTBW.FF.A.0003.100103T0300Z-100103T1900Z/",
                     "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLASH FLOOD WATCH REMAINS IN EFFECT UNTIL 8 AM EST THIS MORNING...",
                     "...FLASH FLOOD WATCH REMAINS IN EFFECT FROM THIS EVENING THROUGH SUNDAY AFTERNOON...",
                     "The Flash Flood Watch continues for",
                     "* A portion of south central Florida, including the following area, Highlands.",
                     "* Until 8 AM EST this morning",
                     "The Flash Flood Watch continues for",
                     "* A portion of south central Florida, including the following area, Highlands.",
                     "* From this evening through Sunday afternoon",
                     "FLZ149-",
                     "/X.CON.KTBW.FA.A.0002.100103T0200Z-100104T0100Z/",
                     "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLOOD WATCH REMAINS IN EFFECT FROM THIS EVENING THROUGH SUNDAY EVENING...",
                     "The Flood Watch continues for",
                     "* A portion of west central Florida, including the following area, Coastal Pasco.",
                     "* From this evening through Sunday evening",
                     ],
    },
    {
    "commentary": "joining two events",
    "name": "Hazard_FFA_8",
    "drtTime": "20100102_1200",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'IC '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 24, 45, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 45, 62, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 45, 62, "FA.A", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 62, 68, "FA.A", ["FLZ149"]),

       ],
    "checkStrings": ["URGENT - IMMEDIATE BROADCAST REQUESTED",
                     "Flood Watch",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLZ057-",
                     "/X.CAN.KTBW.FF.A.0002.000000T0000Z-100102T1300Z/",
                     "/X.EXT.KTBW.FF.A.0003.100102T1200Z-100103T1900Z/",
                     "/00000.0.IC.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLASH FLOOD WATCH NOW IN EFFECT THROUGH SUNDAY AFTERNOON...",
                     "The Flash Flood Watch is now in effect for",
                     "* A portion of south central Florida, including the following area, Highlands.",
                     "* Through Sunday afternoon",
                     "FLZ149-",
                     "/X.CON.KTBW.FA.A.0002.100103T0200Z-100104T0100Z/",
                     "/00000.0.IC.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLOOD WATCH REMAINS IN EFFECT FROM THIS EVENING THROUGH SUNDAY EVENING...",
                     "The Flood Watch continues for",
                     "* A portion of west central Florida, including the following area, Coastal Pasco.",
                     "* From this evening through Sunday evening",
                     ],
    },
    {
    "commentary": "into the tail end of the events",
    "name": "Hazard_FFA_9",
    "drtTime": "20100103_1100",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'SM '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 24, 45, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 45, 62, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 45, 62, "FA.A", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 62, 68, "FA.A", ["FLZ149"]),
       ],
    "checkStrings": ["Flood Watch",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLZ057-",
                     "/X.CON.KTBW.FF.A.0003.000000T0000Z-100103T1900Z/",
                     "/00000.0.SM.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLASH FLOOD WATCH REMAINS IN EFFECT UNTIL 2 PM EST THIS AFTERNOON...",
                     "The Flash Flood Watch continues for",
                     "* A portion of south central Florida, including the following area, Highlands.",
                     "* Until 2 PM EST this afternoon",
                     "FLZ149-",
                     "/X.CON.KTBW.FA.A.0002.000000T0000Z-100104T0100Z/",
                     "/00000.0.SM.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLOOD WATCH REMAINS IN EFFECT THROUGH THIS EVENING...",
                     "The Flood Watch continues for",
                     "* A portion of west central Florida, including the following area, Coastal Pasco.",
                     "* Through this evening",
                     ],
    },
    {
    "commentary": "exp 1st event, continue 2nd event",
    "name": "Hazard_FFA_10",
    "drtTime": "20100103_1855",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'DR '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 24, 45, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 45, 62, "FF.A", ["FLZ057"]),
       ("Fcst", "Hazards", "DISCRETE", 45, 62, "FA.A", ["FLZ149"]),
       ("Fcst", "Hazards", "DISCRETE", 62, 68, "FA.A", ["FLZ149"]),
       ],
    "checkStrings": ["Flood Watch",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLZ057-",
                     "/X.EXP.KTBW.FF.A.0003.000000T0000Z-100103T1900Z/",
                     "/00000.0.DR.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLASH FLOOD WATCH WILL EXPIRE AT 2 PM EST THIS AFTERNOON...",
                     "The Flash Flood Watch for a portion of south central Florida will expire at 2 PM EST this afternoon.",
                     "FLZ149-",
                     "/X.CON.KTBW.FA.A.0002.000000T0000Z-100104T0100Z/",
                     "/00000.0.DR.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLOOD WATCH REMAINS IN EFFECT UNTIL 8 PM EST THIS EVENING...",
                     "The Flood Watch continues for",
                     "* A portion of west central Florida, including the following area, Coastal Pasco.",
                     "* Until 8 PM EST this evening",
                     ],
    },
    {
    "commentary": "cancel 2nd event",
    "name": "Hazard_FFA_11",
    "drtTime": "20100104_0000",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'GO '}",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ],
    "checkStrings": ["Flood Watch",
                     "National Weather Service Tampa Bay Ruskin FL",
                     "FLZ149-",
                     "/X.CAN.KTBW.FA.A.0002.000000T0000Z-100104T0100Z/",
                     "/00000.0.GO.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
                     "...FLOOD WATCH IS CANCELLED...",
                     "The Flood Watch for a portion of west central Florida has been cancelled.",
                     ],
    },

    {
    "commentary": "Deleting hazard grids.",
    "name": "Hazard_FFA_12",
    "productType": None,
    "checkStrings": [],
    "clearHazardsTable": 1,
    },

# Begin detailed phrasing of location tests
    {
    "commentary": "one state, single area, w/o location",
    "name": "Hazard_FFA_13a",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [("AreaDictionary", "TextUtility", "add", areaT1, "delete"),],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ050"]),
       ],
    "checkStrings": [
      "WGUS62 KTBW 010510",
      "FFATBW",
      "URGENT - IMMEDIATE BROADCAST REQUESTED",
      "Flood Watch",
      "National Weather Service Tampa Bay Ruskin FL",
      "1210 AM EST Fri Jan 1 2010",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ050-010800-",
      "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
      "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
      "Pinellas-",
      "1210 AM EST Fri Jan 1 2010",
      "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
      "The National Weather Service in Tampa Bay Ruskin has issued a",
      "* Flood Watch for a portion of western Florida, including the following area, Pinellas.",
      "* Until 3 AM EST early this morning",
      "* |* Basis for the watch *|",
      "* |* (optional) potential impacts of flooding *|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Flood Watch means there is a potential for flooding based on current forecasts.",
      "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
      "&&",
      "$$",
       ],
    },

    {
    "commentary": "one state, single area, w location",
    "name": "Hazard_FFA_13b",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [("AreaDictionary", "TextUtility", "add", areaT2, "delete"),],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ050"]),
       ],
    "checkStrings": [
      "WGUS62 KTBW 010510",
      "FFATBW",
      "URGENT - IMMEDIATE BROADCAST REQUESTED",
      "Flood Watch",
      "National Weather Service Tampa Bay Ruskin FL",
      "1210 AM EST Fri Jan 1 2010",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ050-010800-",
      "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
      "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
      "Pinellas-",
      "1210 AM EST Fri Jan 1 2010",
      "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
      "The National Weather Service in Tampa Bay Ruskin has issued a",
      "* Flood Watch for a portion of western Florida, including the following area, Clearfield.",
      "* Until 3 AM EST early this morning",
      "* |* Basis for the watch *|",
      "* |* (optional) potential impacts of flooding *|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Flood Watch means there is a potential for flooding based on current forecasts.",
      "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
      "&&",
      "$$",
       ],
    },

    {
    "commentary": "two states, single area, w/o location",
    "name": "Hazard_FFA_14a",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [("AreaDictionary", "TextUtility", "add", areaT1, "delete"),],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ050","FLZ057",
         "FLZ052","FLZ155"]),
       ],
    "checkStrings": [
       "WGUS62 KTBW 010510",
       "FFATBW",
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "Flood Watch",
       "National Weather Service Tampa Bay Ruskin FL",
       "1210 AM EST Fri Jan 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ050-052-057-155-010800-",
       "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
       "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
       "Pinellas-Polk-Highlands-Coastal Manatee-",
#        "Including the cities of St. Petersburg, Clearwater, Largo, ",
#        "Lakeland, Winter Haven, Bradenton, Bayshore Gardens, ",
#        "Palmetto, Sebring, Avon Park, Placid Lakes",
       "1210 AM EST Fri Jan 1 2010",
       "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
       "The National Weather Service in Tampa Bay Ruskin has issued a",
       "* Flood Watch for portions of western Florida and western Georgia, including the following areas, in western Florida, Highlands and Pinellas. In western Georgia, Coastal Manatee and Polk.",
       "* Until 3 AM EST early this morning",
       "* |* Basis for the watch *|",
       "* |* (optional) potential impacts of flooding *|",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Flood Watch means there is a potential for flooding based on current forecasts.",
       "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "two states, single area, w location",
    "name": "Hazard_FFA_14b",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [("AreaDictionary", "TextUtility", "add", areaT2, "delete"),],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ050","FLZ057",
         "FLZ052","FLZ155"]),
       ],
    "checkStrings": [
       "WGUS62 KTBW 010510",
       "FFATBW",
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "Flood Watch",
       "National Weather Service Tampa Bay Ruskin FL",
       "1210 AM EST Fri Jan 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ050-052-057-155-010800-",
       "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
       "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
       "Pinellas-Polk-Highlands-Coastal Manatee-",
       "1210 AM EST Fri Jan 1 2010",
       "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
       "The National Weather Service in Tampa Bay Ruskin has issued a",
       "* Flood Watch for portions of western Florida and western Georgia, including the following areas, in western Florida, Clearfield. In western Georgia, Atlanta and Tampa.",
       "* Until 3 AM EST early this morning",
       "* |* Basis for the watch *|",
       "* |* (optional) potential impacts of flooding *|",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Flood Watch means there is a potential for flooding based on current forecasts.",
       "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "one state, multiple areas, w/o location",
    "name": "Hazard_FFA_15a",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [("AreaDictionary", "TextUtility", "add", areaT1, "delete"),],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ050","FLZ160",
         "FLZ057","FLZ151","FLZ056"]),
       ],
    "checkStrings": [
      "WGUS62 KTBW 010510",
      "FFATBW",
      "URGENT - IMMEDIATE BROADCAST REQUESTED",
      "Flood Watch",
      "National Weather Service Tampa Bay Ruskin FL",
      "1210 AM EST Fri Jan 1 2010",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ050-056-057-151-160-010800-",
      "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
      "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
      "Pinellas-Hardee-Highlands-Coastal Hillsborough-Coastal Sarasota-",
      "1210 AM EST Fri Jan 1 2010",
      "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
      "The National Weather Service in Tampa Bay Ruskin has issued a",
      "* Flood Watch for portions of central Florida, southern Florida, and western Florida, including the following areas, in central Florida, Coastal Hillsborough and Coastal Sarasota. In southern Florida, Hardee. In western Florida, Highlands and Pinellas.",
      "* Until 3 AM EST early this morning",
      "* |* Basis for the watch *|",
      "* |* (optional) potential impacts of flooding *|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Flood Watch means there is a potential for flooding based on current forecasts.",
      "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
      "&&",
      "$$",
       ],
    },

    {
    "commentary": "one state, multiple areas, w location",
    "name": "Hazard_FFA_15b",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [("AreaDictionary", "TextUtility", "add", areaT2, "delete"),],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ050","FLZ160",
         "FLZ057","FLZ151","FLZ056"]),
       ],
    "checkStrings": [
       "WGUS62 KTBW 010510",
       "FFATBW",
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "Flood Watch",
       "National Weather Service Tampa Bay Ruskin FL",
       "1210 AM EST Fri Jan 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ050-056-057-151-160-010800-",
       "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
       "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
       "Pinellas-Hardee-Highlands-Coastal Hillsborough-Coastal Sarasota-",
       "1210 AM EST Fri Jan 1 2010",
       "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
       "The National Weather Service in Tampa Bay Ruskin has issued a",
       "* Flood Watch for portions of central Florida, southern Florida, and western Florida, including the following areas, in central Florida, Aunt Ruby. In southern Florida, Tampa. In western Florida, Clearfield.",
       "* Until 3 AM EST early this morning",
       "* |* Basis for the watch *|",
       "* |* (optional) potential impacts of flooding *|",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Flood Watch means there is a potential for flooding based on current forecasts.",
       "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "two states, single area 1st, mulitple area 2nd, w/o location",
    "name": "Hazard_FFA_16a",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [("AreaDictionary", "TextUtility", "add", areaT1, "delete"),],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ050","FLZ052",
         "FLZ155","FLZ061"]),
       ],
    "checkStrings": [
       "WGUS62 KTBW 010510",
       "FFATBW",
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "Flood Watch",
       "National Weather Service Tampa Bay Ruskin FL",
       "1210 AM EST Fri Jan 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ050-052-061-155-010800-",
       "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
       "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
       "Pinellas-Polk-DeSoto-Coastal Manatee-",
       "1210 AM EST Fri Jan 1 2010",
       "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
       "The National Weather Service in Tampa Bay Ruskin has issued a",
       "* Flood Watch for portions of western Florida and Georgia, including the following areas, in western Florida, Pinellas. In Georgia, Coastal Manatee, DeSoto, and Polk.",
       "* Until 3 AM EST early this morning",
       "* |* Basis for the watch *|",
       "* |* (optional) potential impacts of flooding *|",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Flood Watch means there is a potential for flooding based on current forecasts.",
       "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "two states, single area 1st, mulitple area 2nd, w location",
    "name": "Hazard_FFA_16b",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [("AreaDictionary", "TextUtility", "add", areaT2, "delete"),],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ050","FLZ052",
         "FLZ155","FLZ061"]),
       ],
    "checkStrings": [
       "WGUS62 KTBW 010510",
       "FFATBW",
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "Flood Watch",
       "National Weather Service Tampa Bay Ruskin FL",
       "1210 AM EST Fri Jan 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ050-052-061-155-010800-",
       "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
       "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
       "Pinellas-Polk-DeSoto-Coastal Manatee-",
       "1210 AM EST Fri Jan 1 2010",
       "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
       "The National Weather Service in Tampa Bay Ruskin has issued a",
       "* Flood Watch for portions of western Florida and Georgia, including the following areas, in western Florida, Clearfield. In Georgia, Atlanta, Beach, and Tampa.",
       "* Until 3 AM EST early this morning",
       "* |* Basis for the watch *|",
       "* |* (optional) potential impacts of flooding *|",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Flood Watch means there is a potential for flooding based on current forecasts.",
       "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "two states, multiple areas, w/o location",
    "name": "Hazard_FFA_17a",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [("AreaDictionary", "TextUtility", "add", areaT1, "delete"),],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ050","FLZ057",
         "FLZ160","FLZ151","FLZ052","FLZ155","FLZ061","FLZ148"]),
       ],
    "checkStrings": [
       "WGUS62 KTBW 010510",
       "FFATBW",
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "Flood Watch",
       "National Weather Service Tampa Bay Ruskin FL",
       "1210 AM EST Fri Jan 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ050-052-057-061-148-151-155-160-010800-",
       "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
       "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
       "Pinellas-Polk-Highlands-DeSoto-Coastal Hernando-",
       "Coastal Hillsborough-Coastal Manatee-Coastal Sarasota-",
       "1210 AM EST Fri Jan 1 2010",
       "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
       "The National Weather Service in Tampa Bay Ruskin has issued a",
       "* Flood Watch for portions of Florida and Georgia, including the following areas, in Florida, Coastal Hillsborough, Coastal Sarasota, Highlands, and Pinellas. In Georgia, Coastal Hernando, Coastal Manatee, DeSoto, and Polk.",
       "* Until 3 AM EST early this morning",
       "* |* Basis for the watch *|",
       "* |* (optional) potential impacts of flooding *|",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Flood Watch means there is a potential for flooding based on current forecasts.",
       "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
       "&&",
       "$$",
       ],
    },

    {
    "commentary": "two states, multiple areas, w location",
    "name": "Hazard_FFA_17b",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [("AreaDictionary", "TextUtility", "add", areaT2, "delete"),],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLZ050","FLZ057",
         "FLZ160","FLZ151","FLZ052","FLZ155","FLZ061","FLZ148"]),
       ],
    "checkStrings": [
       "WGUS62 KTBW 010510",
       "FFATBW",
       "URGENT - IMMEDIATE BROADCAST REQUESTED",
       "Flood Watch",
       "National Weather Service Tampa Bay Ruskin FL",
       "1210 AM EST Fri Jan 1 2010",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ050-052-057-061-148-151-155-160-010800-",
       "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
       "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
       "Pinellas-Polk-Highlands-DeSoto-Coastal Hernando-",
       "Coastal Hillsborough-Coastal Manatee-Coastal Sarasota-",
       "1210 AM EST Fri Jan 1 2010",
       "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
       "The National Weather Service in Tampa Bay Ruskin has issued a",
       "* Flood Watch for portions of Florida and Georgia, including the following areas, in Florida, Aunt Ruby and Clearfield. In Georgia, Atlanta, Beach, and Tampa.",
       "* Until 3 AM EST early this morning",
       "* |* Basis for the watch *|",
       "* |* (optional) potential impacts of flooding *|",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Flood Watch means there is a potential for flooding based on current forecasts.",
       "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
       "&&",
       "$$",
       ],
    },


    {
    "commentary": "parishes 1, independent 1, counties 1",
    "name": "Hazard_FFA_18a",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [
       ("AreaDictionary", "TextUtility", "add", areaT3, "delete"),
       ("Hazard_FFA_Local", "TextProduct", "replace", 
         (areaT3FIPS0, areaT3FIPS1), "delete"),
       ],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLC017","FLC027",
         "FLC053"]),
       ],
    "checkStrings": [
      "WGUS62 KTBW 010510",
      "FFATBW",
      "URGENT - IMMEDIATE BROADCAST REQUESTED",
      "Flood Watch",
      "National Weather Service Tampa Bay Ruskin FL",
      "1210 AM EST Fri Jan 1 2010",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLC017-027-053-010800-",
      "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
      "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
      "Citrus-DeSoto-Hernando-",
      "1210 AM EST Fri Jan 1 2010",
      "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
      "The National Weather Service in Tampa Bay Ruskin has issued a",
      "* Flood Watch for portions of western Florida and western Louisiana, including the following county, independent city, and parish, in western Florida, Hernando. In western Louisiana, Citrus and DeSoto.",
      "* Until 3 AM EST early this morning",
      "* |* Basis for the watch *|",
      "* |* (optional) potential impacts of flooding *|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Flood Watch means there is a potential for flooding based on current forecasts.",
      "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
      "&&",
      "$$",
       ],
    },

    {
    "commentary": "parishes 2, independent 1, counties 1",
    "name": "Hazard_FFA_18b",
    "drtTime": "20100101_0510",
    "productType": "Hazard_FFA_Local",
    "cmdLineVars": "{('Flood Reason', 'floodReason'): 'ER '}",
    "decodeVTEC": 0,
    "vtecMode": "O",
    "fileChanges": [
       ("AreaDictionary", "TextUtility", "add", areaT3, "delete"),
       ("Hazard_FFA_Local", "TextProduct", "replace", 
         (areaT3FIPS0, areaT3FIPS1), "delete"),
       ],
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 3, "FA.A", ["FLC017","FLC027",
         "FLC053","FLC105"]),
       ],
    "checkStrings": [
      "WGUS62 KTBW 010510",
      "FFATBW",
      "URGENT - IMMEDIATE BROADCAST REQUESTED",
      "Flood Watch",
      "National Weather Service Tampa Bay Ruskin FL",
      "1210 AM EST Fri Jan 1 2010",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLC017-027-053-105-010800-",
      "/O.NEW.KTBW.FA.A.0001.100101T0510Z-100101T0800Z/",
      "/00000.0.ER.000000T0000Z.000000T0000Z.000000T0000Z.OO/",
      "Citrus-DeSoto-Hernando-Polk-",
      "1210 AM EST Fri Jan 1 2010",
      "...FLOOD WATCH IN EFFECT UNTIL 3 AM EST EARLY THIS MORNING...",
      "The National Weather Service in Tampa Bay Ruskin has issued a",
      "* Flood Watch for portions of western Florida and western Louisiana, including the following county, independent city, and parishes, in western Florida, Hernando. In western Louisiana, Citrus, DeSoto, and Polk.",
      "* Until 3 AM EST early this morning",
      "* |* Basis for the watch *|",
      "* |* (optional) potential impacts of flooding *|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Flood Watch means there is a potential for flooding based on current forecasts.",
      "You should monitor later forecasts and be alert for possible Flood Warnings. Those living in areas prone to flooding should be prepared to take action should flooding develop.",
      "&&",
      "$$",
       ],
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
        "vtecMode": "X",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




