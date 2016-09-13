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
# VTEC_GHG Complex Hazard Tests 
#
# Author:
# ----------------------------------------------------------------------------

scripts = [
    {
    "commentary": "Clear out all Hazards Table and Grids.",
    "name": "GHG_Complex1_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },
    {
    "commentary": "Creating a WS.A hazard for the entire CWA.",
    "name": "GHG_Complex1_1",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.A", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043", "FLZ148", "FLZ248", "FLZ149", "FLZ249","FLZ050", "FLZ151", "FLZ251", "FLZ052", "FLZ155", "FLZ255", "FLZ056", "FLZ057", "FLZ160", "FLZ260", "FLZ061", "FLZ162", "FLZ262", "FLZ165", "FLZ265"]),
       ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-050-052-056-057-061-139-142-148-149-151-155-160-162-165-",
      "239-242-248-249-251-255-260-262-265-010800-",
      "/O.NEW.KTBW.WS.A.0001.100101T0000Z-100102T1500Z/",
      "Sumter-Pinellas-Polk-Hardee-Highlands-DeSoto-Coastal Levy-",
      "Coastal Citrus-Coastal Hernando-Coastal Pasco-",
      "Coastal Hillsborough-Coastal Manatee-Coastal Sarasota-",
      "Coastal Charlotte-Coastal Lee-Inland Levy-Inland Citrus-",
      "Inland Hernando-Inland Pasco-Inland Hillsborough-Inland Manatee-",
      "Inland Sarasota-Inland Charlotte-Inland Lee-",
      "700 PM EST Thu Dec 31 2009",
      "...WINTER STORM WATCH IN EFFECT THROUGH SATURDAY MORNING...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Watch...which is in effect through Saturday morning.",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Winter Storm Watch means there is a potential for significant snow...sleet...or ice accumulations that may impact travel. Continue to monitor the latest forecasts.",
      "&&",
      "$$",
       ],
    },
    {
    
    "commentary": """
    Upgrading Area1 from WS.A to WS.W
    Upgrading Area2 from WS.A to ZR.Y
    Area1 (FLZ139, FLZ239, FLZ142, FLZ242, FLZ043, FLZ148, FLZ248, FLZ149, FLZ249, FLZ050, FLZ151, FLZ251) 0-39
    Area2 (FLZ052, FLZ155, FLZ255, FLZ056, FLZ057, FLZ160, FLZ260, FLZ061, FLZ162, FLZ262, FLZ165, FLZ265) 0-39
    """,
    
    "name": "GHG_Complex1_2",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.W", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043", "FLZ148", "FLZ248", "FLZ149", "FLZ249", "FLZ050", "FLZ151", "FLZ251"]),
       ("Fcst", "Hazards", "DISCRETE", 0, 39, "ZR.Y", ["FLZ052", "FLZ155", "FLZ255", "FLZ056", "FLZ057", "FLZ160", "FLZ260", "FLZ061", "FLZ162", "FLZ262", "FLZ165", "FLZ265"]),
       ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-050-139-142-148-149-151-239-242-248-249-251-010800-",
      "/O.UPG.KTBW.WS.A.0001.000000T0000Z-100102T1500Z/",
      "/O.NEW.KTBW.WS.W.0001.100101T0000Z-100102T1500Z/",
      "Sumter-Pinellas-Coastal Levy-Coastal Citrus-Coastal Hernando-",
      "Coastal Pasco-Coastal Hillsborough-Inland Levy-Inland Citrus-",
      "Inland Hernando-Inland Pasco-Inland Hillsborough-",
      "700 PM EST Thu Dec 31 2009",
      "...WINTER STORM WARNING IN EFFECT UNTIL 10 AM EST SATURDAY...",
#      "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning...which is in effect until 10 AM EST Saturday. The Winter Storm Watch is no longer in effect. ",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "|*Choose the appropriate CTA below and delete the rest*|",
      "A Winter Storm Warning for heavy snow means severe winter weather conditions are expected or occurring. Significant amounts of snow are forecast that will make travel dangerous. Only travel in an emergency. If you must travel...keep an extra flashlight...food...and water in your vehicle in case of an emergency.",
      "A Winter Storm Warning means significant amounts of snow...sleet...and ice are expected or occurring. Strong winds are also possible. This will make travel very hazardous or impossible.",
      "A Winter Storm Warning for sleet means that a winter storm system is impacting the area with significant amounts of sleet. Travel is likely to be severely impacted.",
      "&&",
      "$$",
      "FLZ052-056-057-061-155-160-162-165-255-260-262-265-010800-",
      "/O.UPG.KTBW.WS.A.0001.000000T0000Z-100102T1500Z/",
      "/O.NEW.KTBW.ZR.Y.0001.100101T0000Z-100102T1500Z/",
      "Polk-Hardee-Highlands-DeSoto-Coastal Manatee-Coastal Sarasota-",
      "Coastal Charlotte-Coastal Lee-Inland Manatee-Inland Sarasota-",
      "Inland Charlotte-Inland Lee-",
      "700 PM EST Thu Dec 31 2009",
      "...FREEZING RAIN ADVISORY IN EFFECT UNTIL 10 AM EST SATURDAY...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Freezing Rain Advisory...which is in effect until 10 AM EST Saturday. The Winter Storm Watch is no longer in effect. ",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Freezing Rain Advisory means that periods of freezing rain or freezing drizzle will cause travel difficulties. Be prepared for slippery roads. Slow down and use caution while driving.",
      "&&",
      "$$",
       ],
    },
    {
    
    "commentary": """
    Expanding the existing WS.W hazard (Area1) into the existing ZR.Y hazard (Area2)
    Area1 (FLZ139, FLZ239, FLZ142, FLZ242, FLZ043, FLZ148, FLZ248, FLZ149, FLZ249, FLZ050, FLZ151, FLZ251, FLZ052, FLZ057) 0-39
    Area2 (FLZ155, FLZ255, FLZ056, FLZ160, FLZ260, FLZ061, FLZ162, FLZ262, FLZO65) 0-39
    """,
    
    "name": "GHG_Complex1_3",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.W", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043", "FLZ148", "FLZ248", "FLZ149", "FLZ249", "FLZ050", "FLZ151", "FLZ251", "FLZ052", "FLZ057"]),
        ("Fcst", "Hazards", "DISCRETE", 0, 39, "ZR.Y", ["FLZ155", "FLZ255", "FLZ056", "FLZ160", "FLZ260", "FLZ061", "FLZ162", "FLZ262", "FLZ165", "FLZ265"]),
        ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ052-057-010800-",
      "/O.UPG.KTBW.ZR.Y.0001.000000T0000Z-100102T1500Z/",
      "/O.EXA.KTBW.WS.W.0001.000000T0000Z-100102T1500Z/",
      "Polk-Highlands-",
      "700 PM EST Thu Dec 31 2009",
      "...WINTER STORM WARNING IN EFFECT UNTIL 10 AM EST SATURDAY...",
#      "The National Weather Service in Tampa Bay Ruskin has issued a Winter Storm Warning...which is in effect until 10 AM EST Saturday. THE Freezing Rain Advisory is no longer in effect. ",
#      "|*|*|* SEGMENT TEXT GOES HERE *|.*|*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#      "A Winter Storm Warning MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "&&",
      "$$",
      "FLZ043-050-139-142-148-149-151-239-242-248-249-251-010800-",
      "/O.CON.KTBW.WS.W.0001.000000T0000Z-100102T1500Z/",
      "Sumter-Pinellas-Coastal Levy-Coastal Citrus-Coastal Hernando-",
      "Coastal Pasco-Coastal Hillsborough-Inland Levy-Inland Citrus-",
      "Inland Hernando-Inland Pasco-Inland Hillsborough-",
      "700 PM EST Thu Dec 31 2009",
      "...WINTER STORM WARNING REMAINS IN EFFECT UNTIL 10 AM EST SATURDAY...",
#       "A Winter Storm Warning remains in effect until 10 AM EST Saturday. ",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#      "A Winter Storm Warning MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "&&",
      "$$",
      "FLZ056-061-155-160-162-165-255-260-262-265-010800-",
      "/O.CON.KTBW.ZR.Y.0001.000000T0000Z-100102T1500Z/",
      "Hardee-DeSoto-Coastal Manatee-Coastal Sarasota-Coastal Charlotte-",
      "Coastal Lee-Inland Manatee-Inland Sarasota-Inland Charlotte-",
      "Inland Lee-",
      "700 PM EST Thu Dec 31 2009",
      "...FREEZING RAIN ADVISORY REMAINS IN EFFECT UNTIL 10 AM EST SATURDAY...",
#       "A Freezing Rain Advisory remains in effect until 10 AM EST Saturday. ",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Freezing Rain Advisory means that periods of freezing rain or freezing drizzle will cause travel difficulties. Be prepared for slippery roads. Slow down and use caution while driving.",
      "&&",
      "$$",
        ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex1_4",
    "productType": "Hazard_WSW_Local",
    "deleteGrids": [],        
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-050-052-057-139-142-148-149-151-239-242-248-249-251-010800-",
      "/O.CON.KTBW.WS.W.0001.000000T0000Z-100102T1500Z/",
      "Sumter-Pinellas-Polk-Highlands-Coastal Levy-Coastal Citrus-",
      "Coastal Hernando-Coastal Pasco-Coastal Hillsborough-Inland Levy-",
      "Inland Citrus-Inland Hernando-Inland Pasco-Inland Hillsborough-",
      "700 PM EST Thu Dec 31 2009",
      "...WINTER STORM WARNING REMAINS IN EFFECT UNTIL 10 AM EST SATURDAY...",
#       "A Winter Storm Warning remains in effect until 10 AM EST Saturday.",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#      "A Winter Storm Warning MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "&&",
      "$$",
      "FLZ056-061-155-160-162-165-255-260-262-265-010800-",
      "/O.CON.KTBW.ZR.Y.0001.000000T0000Z-100102T1500Z/",
      "Hardee-DeSoto-Coastal Manatee-Coastal Sarasota-Coastal Charlotte-",
      "Coastal Lee-Inland Manatee-Inland Sarasota-Inland Charlotte-",
      "Inland Lee-",
      "700 PM EST Thu Dec 31 2009",
      "...FREEZING RAIN ADVISORY REMAINS IN EFFECT UNTIL 10 AM EST SATURDAY...",
#       "A Freezing Rain Advisory remains in effect until 10 AM EST Saturday. ",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Freezing Rain Advisory means that periods of freezing rain or freezing drizzle will cause travel difficulties. Be prepared for slippery roads. Slow down and use caution while driving.",
      "&&",
      "$$",
                     ],
    },
    {
    
    "commentary": """
    Removing Area2 and the ZR.Y hazard
    Area2 (FLZ155, FLZ255, FLZ056, FLZ160, FLZ260, FLZ061, FLZ162, FLZ262, FLZ165, FLZ265)
    """,
    
    "name": "GHG_Complex1_5",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.W", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043", "FLZ148", "FLZ248", "FLZ149", "FLZ249", "FLZ050", "FLZ151", "FLZ251", "FLZ052", "FLZ057"]),
    ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ056-061-155-160-162-165-255-260-262-265-010100-",
      "/O.CAN.KTBW.ZR.Y.0001.000000T0000Z-100102T1500Z/",
      "Hardee-DeSoto-Coastal Manatee-Coastal Sarasota-Coastal Charlotte-",
      "Coastal Lee-Inland Manatee-Inland Sarasota-Inland Charlotte-",
      "Inland Lee-",
      "700 PM EST Thu Dec 31 2009",
      "...FREEZING RAIN ADVISORY IS CANCELLED...",
      "The National Weather Service in Tampa Bay Ruskin has cancelled the Freezing Rain Advisory. ",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ043-050-052-057-139-142-148-149-151-239-242-248-249-251-010800-",
      "/O.CON.KTBW.WS.W.0001.000000T0000Z-100102T1500Z/",
      "Sumter-Pinellas-Polk-Highlands-Coastal Levy-Coastal Citrus-",
      "Coastal Hernando-Coastal Pasco-Coastal Hillsborough-Inland Levy-",
      "Inland Citrus-Inland Hernando-Inland Pasco-Inland Hillsborough-",
      "700 PM EST Thu Dec 31 2009",
      "...WINTER STORM WARNING REMAINS IN EFFECT UNTIL 10 AM EST SATURDAY...",
#       "A Winter Storm Warning remains in effect until 10 AM EST Saturday. ",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#      "A Winter Storm Warning MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "&&",
      "$$",
         ],
    },
    {
    "commentary": "No changes are made to the existing hazards.",
    "name": "GHG_Complex1_6",
    "productType": "Hazard_WSW_Local",
    "deleteGrids": [],
    "decodeVTEC": 0,
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-050-052-057-139-142-148-149-151-239-242-248-249-251-010800-",
      "/O.CON.KTBW.WS.W.0001.000000T0000Z-100102T1500Z/",
      "Sumter-Pinellas-Polk-Highlands-Coastal Levy-Coastal Citrus-",
      "Coastal Hernando-Coastal Pasco-Coastal Hillsborough-Inland Levy-",
      "Inland Citrus-Inland Hernando-Inland Pasco-Inland Hillsborough-",
      "700 PM EST Thu Dec 31 2009",
      "...WINTER STORM WARNING REMAINS IN EFFECT UNTIL 10 AM EST SATURDAY...",
#       "A Winter Storm Warning remains in effect until 10 AM EST Saturday. ",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#      "A Winter Storm Warning MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "&&",
      "$$",
                     ],
    }, 
    {

    "commentary": """
    Reducing the WS.W hazard coverage of Area1
    Area1 (FLZ148, FLZ248, FLZ149, FLZ249, FLZ050) 0-39
    """,
    
    "name": "GHG_Complex1_7",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.W", ["FLZ148", "FLZ248", "FLZ149", "FLZ249", "FLZ050"]),
        ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ043-052-057-139-142-151-239-242-251-010100-",
      "/O.CAN.KTBW.WS.W.0001.000000T0000Z-100102T1500Z/",
      "Sumter-Polk-Highlands-Coastal Levy-Coastal Citrus-",
      "Coastal Hillsborough-Inland Levy-Inland Citrus-",
      "Inland Hillsborough-",
      "700 PM EST Thu Dec 31 2009",
      "...WINTER STORM WARNING IS CANCELLED...",
      "The National Weather Service in Tampa Bay Ruskin has cancelled the Winter Storm Warning. ",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "$$",
      "FLZ050-148-149-248-249-010800-",
      "/O.CON.KTBW.WS.W.0001.000000T0000Z-100102T1500Z/",
      "Pinellas-Coastal Hernando-Coastal Pasco-Inland Hernando-",
      "Inland Pasco-",
      "700 PM EST Thu Dec 31 2009",
      "...WINTER STORM WARNING REMAINS IN EFFECT UNTIL 10 AM EST SATURDAY...",
#       "A Winter Storm Warning remains in effect until 10 AM EST Saturday. ",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
#      "A Winter Storm Warning MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "&&",
      "$$",
                     ],
    },
    {
    
    "commentary": """
    Creating a WC.Y hazard for Area3
    Area3 (FLZ139, FLZ239, FLZ142, FLZ242, FLZ043, FLZ151, FLZ251, FLZ052, FLZ155, FLZ255, FLZ056, FLZ057, FLZ160, FLZ260, FLZ061, FLZ162, FLZ262, FLZ165, FLZ265) 0-39
    """,
    
    "name": "GHG_Complex1_8",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 39, "WS.W", ["FLZ148", "FLZ248", "FLZ149", "FLZ249", "FLZ050"]),
        ("Fcst", "Hazards", "DISCRETE", 0, 39, "WC.Y", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043", "FLZ151", "FLZ251", "FLZ052", "FLZ155", "FLZ255", "FLZ056", "FLZ057", "FLZ160", "FLZ260", "FLZ061", "FLZ162", "FLZ262", "FLZ165", "FLZ265"]),
        ],
    "checkStrings": [
      "WWUS42 KTBW 010000",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "National Weather Service Tampa Bay Ruskin FL",
      "700 PM EST Thu Dec 31 2009",
      "...|*Overview headline (must edit)*|...",
      ".|*Overview (must edit)*|.",
      "FLZ050-148-149-248-249-010800-",
      "/O.CON.KTBW.WS.W.0001.000000T0000Z-100102T1500Z/",
      "Pinellas-Coastal Hernando-Coastal Pasco-Inland Hernando-",
      "Inland Pasco-",
      "700 PM EST Thu Dec 31 2009",
      "...WINTER STORM WARNING REMAINS IN EFFECT UNTIL 10 AM EST SATURDAY...",
#       "A Winter Storm Warning remains in effect until 10 AM EST Saturday. ",
#      "|*|* SEGMENT TEXT GOES HERE *|.*|",
#      "A Winter Storm Warning MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "$$",
      "FLZ043-052-056-057-061-139-142-151-155-160-162-165-239-242-251-",
      "255-260-262-265-010800-",
      "/O.NEW.KTBW.WC.Y.0001.100101T0000Z-100102T1500Z/",
      "Sumter-Polk-Hardee-Highlands-DeSoto-Coastal Levy-Coastal Citrus-",
      "Coastal Hillsborough-Coastal Manatee-Coastal Sarasota-",
      "Coastal Charlotte-Coastal Lee-Inland Levy-Inland Citrus-",
      "Inland Hillsborough-Inland Manatee-Inland Sarasota-",
      "Inland Charlotte-Inland Lee-",
      "700 PM EST Thu Dec 31 2009",
      "...WIND CHILL ADVISORY IN EFFECT UNTIL 10 AM EST SATURDAY...",
      "The National Weather Service in Tampa Bay Ruskin has issued a Wind Chill Advisory...which is in effect until 10 AM EST Saturday. ",
#       "|* SEGMENT TEXT GOES HERE *|.",
      "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
      "A Wind Chill Advisory means that very cold air and strong winds will combine to generate low wind chills. This will result in frost bite and lead to hypothermia if precautions are not taken. If you must venture outdoors...make sure you wear a hat and gloves.",
      "&&",
      "$$",
                     ],
    },
    {
    
    "commentary": """
    Removing Area1 and the WS.W hazard
    Area1 (FLZ148, FLZ248, FLZ149, FLZ249, FLZ050) 0-39
    Reducing the time range of Area3
    Area3 (FLZ139, FLZ239, FLZ142, FLZ242, FLZ043, FLZ151, FLZ251, FLZ052, FLZ155, FLZ255, FLZ056, FLZ057, FLZ160, FLZ260, FLZ061, FLZ162, FLZ262, FLZ165, FLZ265) 0-36
    """,
    
    "name": "GHG_Complex1_9",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
        ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
        ("Fcst", "Hazards", "DISCRETE", 0, 36, "WC.Y", ["FLZ139", "FLZ239", "FLZ142", "FLZ242", "FLZ043", "FLZ151", "FLZ251", "FLZ052", "FLZ155", "FLZ255", "FLZ056", "FLZ057", "FLZ160", "FLZ260", "FLZ061", "FLZ162", "FLZ262", "FLZ165", "FLZ265"]),
        ],
    "checkStrings": [
       "WWUS42 KTBW 010000",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "National Weather Service Tampa Bay Ruskin FL",
       "700 PM EST Thu Dec 31 2009",
       "...|*Overview headline (must edit)*|...",
       ".|*Overview (must edit)*|.",
       "FLZ050-148-149-248-249-010100-",
       "/O.CAN.KTBW.WS.W.0001.000000T0000Z-100102T1500Z/",
       "Pinellas-Coastal Hernando-Coastal Pasco-Inland Hernando-",
       "Inland Pasco-",
       "700 PM EST Thu Dec 31 2009",
       "...WINTER STORM WARNING IS CANCELLED...",
       "The National Weather Service in Tampa Bay Ruskin has cancelled the Winter Storm Warning.",
#         "|* SEGMENT TEXT GOES HERE *|.",
       "$$",
       "FLZ043-052-056-057-061-139-142-151-155-160-162-165-239-242-251-",
       "255-260-262-265-010800-",
       "/O.EXT.KTBW.WC.Y.0001.000000T0000Z-100102T1200Z/",
       "Sumter-Polk-Hardee-Highlands-DeSoto-Coastal Levy-Coastal Citrus-",
       "Coastal Hillsborough-Coastal Manatee-Coastal Sarasota-",
       "Coastal Charlotte-Coastal Lee-Inland Levy-Inland Citrus-",
       "Inland Hillsborough-Inland Manatee-Inland Sarasota-",
       "Inland Charlotte-Inland Lee-",
       "700 PM EST Thu Dec 31 2009",
       "...WIND CHILL ADVISORY NOW IN EFFECT UNTIL 7 AM EST SATURDAY...",
#        "The Wind Chill Advisory is now in effect until 7 AM EST Saturday.",
#         "|* SEGMENT TEXT GOES HERE *|.",
       "PRECAUTIONARY/PREPAREDNESS ACTIONS...",
       "A Wind Chill Advisory means that very cold air and strong winds will combine to generate low wind chills. This will result in frost bite and lead to hypothermia if precautions are not taken. If you must venture outdoors...make sure you wear a hat and gloves.",
       "&&",
       "$$",
                     ],
    },
    {
    "commentary": "Canceling out all hazards.",
    "name": "GHG_Complex1_10",
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
        "orderStrings": 1,
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




