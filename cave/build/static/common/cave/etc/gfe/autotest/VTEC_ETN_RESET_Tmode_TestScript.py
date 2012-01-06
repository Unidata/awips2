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
    "name": "EXPNEW_0",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },

    {
    "commentary": "Generating WS.W Test events",
    "name": "ETNReset_1",
    "drtTime": "20100101_0510",
    "productType": "Hazard_WSW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 12, 24, "WS.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 30, 36, "WS.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 40, 48, "WS.W", ["FLZ050"]),
       ],
    "vtecMode": "T",
    "checkStrings": [
      "WWUS42 KTBW 010510",
      "WSWTBW",
      "TEST...URGENT - WINTER WEATHER MESSAGE...TEST",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "1210 AM EST FRI JAN 1 2010",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ050-011315-",
      "/T.NEW.KTBW.WS.W.0001.100101T1700Z-100102T0500Z/",
      "/T.NEW.KTBW.WS.W.0002.100102T1100Z-100102T1700Z/",
      "/T.NEW.KTBW.WS.W.0003.100102T2100Z-100103T0500Z/",
      "PINELLAS-",
      "1210 AM EST FRI JAN 1 2010",
      "...THIS MESSAGE IS FOR TEST PURPOSES ONLY...",
      "...TEST WINTER STORM WARNING IN EFFECT FROM NOON TODAY TO MIDNIGHT EST TONIGHT TEST...",
      "...TEST WINTER STORM WARNING IN EFFECT FROM 6 AM TO NOON EST SATURDAY TEST...",
      "...TEST WINTER STORM WARNING IN EFFECT FROM 4 PM SATURDAY TO MIDNIGHT EST SATURDAY NIGHT TEST...",
#      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A TEST WINTER STORM WARNING...WHICH IS IN EFFECT FROM NOON TODAY TO MIDNIGHT EST TONIGHT. A TEST WINTER STORM WARNING HAS ALSO BEEN ISSUED. THIS TEST WINTER STORM WARNING IS IN EFFECT FROM 6 AM TO NOON EST SATURDAY. IN ADDITION...A TEST WINTER STORM WARNING HAS BEEN ISSUED. THIS TEST WINTER STORM WARNING IS IN EFFECT FROM 4 PM SATURDAY TO MIDNIGHT EST SATURDAY NIGHT.",
      "|* SEGMENT TEXT GOES HERE *|.",
#      "A WINTER STORM WARNING MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "THIS IS A TEST MESSAGE. DO NOT TAKE ACTION BASED ON THIS TEST MESSAGE.",
      "$$",

                     ],
    },

    {
    "commentary": """Running operational mode. Adds new event.  Formatter 
      ignores previous test mode vtec table entries, except for ETN
      determination.""",
    "name": "ETNReset_2",
    "drtTime": "20100108_1801",
    "productType": "Hazard_WSW_Local",
    "vtecMode": "O",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 12, 24, "WS.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 30, 36, "WS.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 40, 48, "WS.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 8*24, 11*24, "WS.W", ["FLZ050"]),
       ],
    "checkStrings": [
      "WWUS42 KTBW 081801",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "101 PM EST FRI JAN 8 2010",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ050-090215-",
      "/O.NEW.KTBW.WS.W.0004.100109T0500Z-100112T0500Z/",
      "PINELLAS-",
      "101 PM EST FRI JAN 8 2010",
      "...WINTER STORM WARNING IN EFFECT FROM MIDNIGHT TONIGHT TO MIDNIGHT EST MONDAY NIGHT...",
#      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A WINTER STORM WARNING...WHICH IS IN EFFECT FROM MIDNIGHT TONIGHT TO MIDNIGHT EST MONDAY NIGHT.",
      "|* SEGMENT TEXT GOES HERE *|.",
#      "A WINTER STORM WARNING MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "$$",

                     ],
    },

    {
    "commentary": "Operational event issued. ETN increments from last event.",
    "name": "ETNReset_3",
    "drtTime": "20100115_1800",
    "productType": "Hazard_WSW_Local",
    "vtecMode": "O",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 15*24, 16*24, "WS.W", ["FLZ050"]),
       ],
    "checkStrings": [
      "WWUS42 KTBW 151800",
      "WSWTBW",
      "URGENT - WINTER WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "100 PM EST FRI JAN 15 2010",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ050-160200-",
      "/O.NEW.KTBW.WS.W.0005.100116T0500Z-100117T0500Z/",
      "PINELLAS-",
      "100 PM EST FRI JAN 15 2010",
      "...WINTER STORM WARNING IN EFFECT FROM MIDNIGHT TONIGHT TO MIDNIGHT EST SATURDAY NIGHT...",
#      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A WINTER STORM WARNING...WHICH IS IN EFFECT FROM MIDNIGHT TONIGHT TO MIDNIGHT EST SATURDAY NIGHT.",
      "|* SEGMENT TEXT GOES HERE *|.",
#      "A WINTER STORM WARNING MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
      "$$",

                     ],
    },

    {
    "commentary": "Test event issued. ETNs increments.",
    "name": "ETNReset_4",
    "drtTime": "20100115_2100",
    "productType": "Hazard_WSW_Local",
    "vtecMode": "T",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 15*24, 16*24, "WS.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 20*24, 21*24, "WS.W", ["FLZ050"]),
       ],
    "checkStrings": [
       "WWUS42 KTBW 152100",
       "WSWTBW",
       "TEST...URGENT - WINTER WEATHER MESSAGE...TEST",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "400 PM EST FRI JAN 15 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ050-160500-",
       "/T.NEW.KTBW.WS.W.0006.100116T0500Z-100117T0500Z/",
       "/T.NEW.KTBW.WS.W.0007.100121T0500Z-100122T0500Z/",
       "PINELLAS-",
       "400 PM EST FRI JAN 15 2010",
       "...THIS MESSAGE IS FOR TEST PURPOSES ONLY...",
       "...TEST WINTER STORM WARNING IN EFFECT FROM MIDNIGHT TONIGHT TO MIDNIGHT EST SATURDAY NIGHT TEST...",
       "...TEST WINTER STORM WARNING IN EFFECT FROM MIDNIGHT WEDNESDAY NIGHT TO MIDNIGHT EST THURSDAY NIGHT TEST...",
#       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A TEST WINTER STORM WARNING...WHICH IS IN EFFECT FROM MIDNIGHT TONIGHT TO MIDNIGHT EST SATURDAY NIGHT. A TEST WINTER STORM WARNING HAS ALSO BEEN ISSUED. THIS TEST WINTER STORM WARNING IS IN EFFECT FROM MIDNIGHT WEDNESDAY NIGHT TO MIDNIGHT EST THURSDAY NIGHT.",
       "|* SEGMENT TEXT GOES HERE *|.",
#       "A WINTER STORM WARNING MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
       "THIS IS A TEST MESSAGE. DO NOT TAKE ACTION BASED ON THIS TEST MESSAGE.",
       "$$",

                     ],
    },
    {
    "commentary": "Operational Event issued. Formatter ignores test events.",
    "name": "ETNReset_5",
    "drtTime": "20100115_2200",
    "productType": "Hazard_WSW_Local",
    "vtecMode": "O",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 15*24, 16*24, "WS.W", ["FLZ050"]),
       ("Fcst", "Hazards", "DISCRETE", 20*24, 21*24, "WS.W", ["FLZ050"]),
       ],
    "checkStrings": [
       "WWUS42 KTBW 152200",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "500 PM EST FRI JAN 15 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ050-160600-",
       "/O.NEW.KTBW.WS.W.0008.100121T0500Z-100122T0500Z/",
       "/O.CON.KTBW.WS.W.0005.100116T0500Z-100117T0500Z/",
       "PINELLAS-",
       "500 PM EST FRI JAN 15 2010",
       "...WINTER STORM WARNING REMAINS IN EFFECT FROM MIDNIGHT TONIGHT TO MIDNIGHT EST SATURDAY NIGHT...",
       "...WINTER STORM WARNING IN EFFECT FROM MIDNIGHT WEDNESDAY NIGHT TO MIDNIGHT EST THURSDAY NIGHT...",
#       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A WINTER STORM WARNING...WHICH IS IN EFFECT FROM MIDNIGHT WEDNESDAY NIGHT TO MIDNIGHT EST THURSDAY NIGHT. A WINTER STORM WARNING REMAINS IN EFFECT FROM MIDNIGHT TONIGHT TO MIDNIGHT EST SATURDAY NIGHT.",
       "|* SEGMENT TEXT GOES HERE *|.",
#       "A WINTER STORM WARNING MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
       "$$",

                     ],
    },

    {
    "commentary": "Test event issued. ETN increments.",
    "name": "ETNReset_6",
    "drtTime": "20100123_2000",
    "productType": "Hazard_WSW_Local",
    "vtecMode": "T",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 23*24, 25*24, "WS.W", ["FLZ050"]),
       ],
    "checkStrings": [
       "WWUS42 KTBW 232000",
       "WSWTBW",
       "TEST...URGENT - WINTER WEATHER MESSAGE...TEST",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "300 PM EST SAT JAN 23 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ050-240400-",
       "/T.NEW.KTBW.WS.W.0009.100124T0500Z-100126T0500Z/",
       "PINELLAS-",
       "300 PM EST SAT JAN 23 2010",
       "...THIS MESSAGE IS FOR TEST PURPOSES ONLY...",
       "...TEST WINTER STORM WARNING IN EFFECT FROM MIDNIGHT TONIGHT TO MIDNIGHT EST MONDAY NIGHT TEST...",
#      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A TEST WINTER STORM WARNING...WHICH IS IN EFFECT FROM MIDNIGHT TONIGHT TO MIDNIGHT EST MONDAY NIGHT.",
       "|* SEGMENT TEXT GOES HERE *|.",
#       "A WINTER STORM WARNING MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
       "THIS IS A TEST MESSAGE. DO NOT TAKE ACTION BASED ON THIS TEST MESSAGE.",
       "$$",

                     ],
    },

    {
    "commentary": "Operational event issued. ETN increments.",
    "name": "ETNReset_7",
    "drtTime": "20100131_2000",
    "productType": "Hazard_WSW_Local",
    "vtecMode": "O",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 31*24, (31*24)+2, "WS.W", ["FLZ050"]),
       ],
    "checkStrings": [
       "WWUS42 KTBW 312000",
       "WSWTBW",
       "URGENT - WINTER WEATHER MESSAGE",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "300 PM EST SUN JAN 31 2010",
       "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
       ".|*OVERVIEW (MUST EDIT)*|.",
       "FLZ050-010400-",
       "/O.NEW.KTBW.WS.W.0010.100201T0500Z-100201T0700Z/",
       "PINELLAS-",
       "300 PM EST SUN JAN 31 2010",
       "...WINTER STORM WARNING IN EFFECT FROM MIDNIGHT TONIGHT TO 2 AM EST MONDAY...",
#       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A WINTER STORM WARNING...WHICH IS IN EFFECT FROM MIDNIGHT TONIGHT TO 2 AM EST MONDAY.",
       "|* SEGMENT TEXT GOES HERE *|.",
#       "A WINTER STORM WARNING MEANS SIGNIFICANT AMOUNTS OF SNOW...SLEET...AND ICE ARE EXPECTED OR OCCURRING. STRONG WINDS ARE ALSO POSSIBLE. THIS WILL MAKE TRAVEL VERY HAZARDOUS OR IMPOSSIBLE.",
       "$$",
              ],
    },
    {
    "commentary": "Cleanup.",
    "name": "ETNReset_8_cleanup",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    },
    {
    "commentary": """Simulating PQR's SU.Y set of events.  Issue 3 events
      in test mode.  The issue was that the SU.Y ETN was stuck due to 
      the interaction between test and operational events and ETNs.""",
    "name": "ETNReset_9",
    "drtTime": "20100201_0000",
    "gridsStartTime": "20100201_0000",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 6, "SU.Y", ["FLZ049"]),
       ("Fcst", "Hazards", "DISCRETE", 10, 16, "SU.Y", ["FLZ049"]),
       ("Fcst", "Hazards", "DISCRETE", 20, 26, "SU.Y", ["FLZ049"]),
       ],
    "vtecMode": "T",
    "checkStrings": [
       "WHUS42 KTBW 010000",
       "CFWTBW",
       "TEST...COASTAL HAZARD MESSAGE...TEST",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "700 PM EST SUN JAN 31 2010",
       "FLZ049-010800-",
       "/T.NEW.KTBW.SU.Y.0001.100201T0000Z-100201T0600Z/",
       "/T.NEW.KTBW.SU.Y.0002.100201T1000Z-100201T1600Z/",
       "/T.NEW.KTBW.SU.Y.0003.100201T2000Z-100202T0200Z/",
       "PASCO-",
       "700 PM EST SUN JAN 31 2010",
       "...THIS MESSAGE IS FOR TEST PURPOSES ONLY...",
       "...TEST HIGH SURF ADVISORY IN EFFECT UNTIL 1 AM EST MONDAY TEST...",
       "...TEST HIGH SURF ADVISORY IN EFFECT FROM 5 AM TO 11 AM EST MONDAY TEST...",
       "...TEST HIGH SURF ADVISORY IN EFFECT FROM 3 PM TO 9 PM EST MONDAY TEST...",
#       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A TEST HIGH SURF ADVISORY...WHICH IS IN EFFECT UNTIL 1 AM EST MONDAY. A TEST HIGH SURF ADVISORY HAS ALSO BEEN ISSUED. THIS TEST HIGH SURF ADVISORY IS IN EFFECT FROM 5 AM TO 11 AM EST MONDAY. IN ADDITION...A TEST HIGH SURF ADVISORY HAS BEEN ISSUED. THIS TEST HIGH SURF ADVISORY IS IN EFFECT FROM 3 PM TO 9 PM EST MONDAY.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "A HIGH SURF ADVISORY MEANS THAT HIGH SURF WILL AFFECT BEACHES IN THE ADVISORY AREA...PRODUCING RIP CURRENTS AND LOCALIZED BEACH EROSION.",
       "THIS IS A TEST MESSAGE. DO NOT TAKE ACTION BASED ON THIS TEST MESSAGE.",
       "$$",

                     ],
    },
    {
    "commentary": """Simulating PQR's SU.Y set of events - issue SU.Y 0004 NEW
      in test mode.""",
    "name": "ETNReset_10",
    "drtTime": "20100527_1600",
    "gridsStartTime": "20100527_0000",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 16, 34, "SU.Y", ["FLZ049"]),
       ],
    "vtecMode": "T",
    "checkStrings": [
       "WHUS42 KTBW 271600",
       "CFWTBW",
       "TEST...COASTAL HAZARD MESSAGE...TEST",
       "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       "1200 PM EDT THU MAY 27 2010",
       "FLZ049-280000-",
       "/T.NEW.KTBW.SU.Y.0004.100527T1600Z-100528T1000Z/",
       "PASCO-",
       "1200 PM EDT THU MAY 27 2010",
       "...THIS MESSAGE IS FOR TEST PURPOSES ONLY...",
       "...TEST HIGH SURF ADVISORY IN EFFECT UNTIL 6 AM EDT FRIDAY TEST...",
       "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A TEST HIGH SURF ADVISORY...WHICH IS IN EFFECT UNTIL 6 AM EDT FRIDAY.",
       "|* SEGMENT TEXT GOES HERE *|.",
       "A HIGH SURF ADVISORY MEANS THAT HIGH SURF WILL AFFECT BEACHES IN THE ADVISORY AREA...PRODUCING RIP CURRENTS AND LOCALIZED BEACH EROSION.",
       "THIS IS A TEST MESSAGE. DO NOT TAKE ACTION BASED ON THIS TEST MESSAGE.",
       "$$",
                     ],
    },
    {
    "commentary": "CAN the SU.Y 0004 in test mode.",
    "name": "ETNReset_11",
    "drtTime": "20100527_1805",
    "gridsStartTime": "20100527_0000",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ],
    "vtecMode": "T",
    "checkStrings": [
      "WHUS42 KTBW 271805",
      "CFWTBW",
      "TEST...COASTAL HAZARD MESSAGE...TEST",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "205 PM EDT THU MAY 27 2010",
      "FLZ049-271915-",
      "/T.CAN.KTBW.SU.Y.0004.000000T0000Z-100528T1000Z/",
      "PASCO-",
      "205 PM EDT THU MAY 27 2010",
      "...THIS MESSAGE IS FOR TEST PURPOSES ONLY...",
      "...TEST HIGH SURF ADVISORY IS CANCELLED TEST...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE TEST HIGH SURF ADVISORY.",
      "THIS IS A TEST MESSAGE. DO NOT TAKE ACTION BASED ON THIS TEST MESSAGE.",
      "$$",
                     ],
    },
    {
    "name": "ETNReset_12",
    "commentary": """Issue a new SU.Y in operational mode.  Gets assigned the
      next ETN.""",
    "drtTime": "20100914_0000",
    "gridsStartTime": "20100914_0000",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "SU.Y", ["FLZ051"]),
       ],
    "vtecMode": "O",
    "checkStrings": [
        "WHUS42 KTBW 140000",
        "CFWTBW",
        "COASTAL HAZARD MESSAGE",
        "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
        "800 PM EDT MON SEP 13 2010",
        "FLZ051-140800-",
        "/O.NEW.KTBW.SU.Y.0005.100914T0000Z-100915T0000Z/",
        "HILLSBOROUGH-",
        "800 PM EDT MON SEP 13 2010",
        "...HIGH SURF ADVISORY IN EFFECT UNTIL 8 PM EDT TUESDAY...",
        "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HIGH SURF ADVISORY...WHICH IS IN EFFECT UNTIL 8 PM EDT TUESDAY.",
        "|* SEGMENT TEXT GOES HERE *|.",
        "A HIGH SURF ADVISORY MEANS THAT HIGH SURF WILL AFFECT BEACHES IN THE ADVISORY AREA...PRODUCING RIP CURRENTS AND LOCALIZED BEACH EROSION.",
        "$$",
                     ],
    },
    {
    "name": "ETNReset_12a",
    "commentary": "Cancel the SU.Y.",
    "drtTime": "20100914_1200",
    "gridsStartTime": "20100914_0000",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ],
    "vtecMode": "O",
    "checkStrings": [
      "WHUS42 KTBW 141200",
      "CFWTBW",
      "COASTAL HAZARD MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "800 AM EDT TUE SEP 14 2010",
      "FLZ051-141300-",
      "/O.CAN.KTBW.SU.Y.0005.000000T0000Z-100915T0000Z/",
      "HILLSBOROUGH-",
      "800 AM EDT TUE SEP 14 2010",
      "...HIGH SURF ADVISORY IS CANCELLED...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS CANCELLED THE HIGH SURF ADVISORY. ",
      "$$",
                     ],
    },
    {
    "commentary": "force a vtec table squeeze, to remove old events",
    "name": "ETNReset_12b",
    "drtTime": "20100918_0000",
    "gridsStartTime": "20100918_0000",
    "productType": "Hazard_NPW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "HW.W", ["FLZ048","FLZ049","FLZ051"]),
       ],
    "vtecMode": "O",
    "checkStrings": [
      "WWUS72 KTBW 180000",
      "NPWTBW",
      "URGENT - WEATHER MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "800 PM EDT FRI SEP 17 2010",
      "...|*OVERVIEW HEADLINE (MUST EDIT)*|...",
      ".|*OVERVIEW (MUST EDIT)*|.",
      "FLZ048-049-051-180800-",
      "/O.NEW.KTBW.HW.W.0001.100918T0000Z-100919T0000Z/",
      "HERNANDO-PASCO-HILLSBOROUGH-",
      "800 PM EDT FRI SEP 17 2010",
      "...HIGH WIND WARNING IN EFFECT UNTIL 8 PM EDT SATURDAY...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HIGH WIND WARNING...WHICH IS IN EFFECT UNTIL 8 PM EDT SATURDAY.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A HIGH WIND WARNING MEANS A HAZARDOUS HIGH WIND EVENT IS EXPECTED OR OCCURRING. SUSTAINED WIND SPEEDS OF AT LEAST 40 MPH OR GUSTS OF 58 MPH OR MORE CAN LEAD TO PROPERTY DAMAGE.",
      "$$",

                     ],
    },
    {
    "name": "ETNReset_13",
    "commentary": "issue a new SU.Y in operational mode",
    "drtTime": "20100922_0000",
    "gridsStartTime": "20100922_0000",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "SU.Y", ["FLZ048","FLZ049","FLZ051"]),
       ],
    "vtecMode": "O",
    "checkStrings": [
      "WHUS42 KTBW 220000",
      "CFWTBW",
      "COASTAL HAZARD MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "800 PM EDT TUE SEP 21 2010",
      "FLZ048-049-051-220800-",
      "/O.NEW.KTBW.SU.Y.0006.100922T0000Z-100923T0000Z/",
      "HERNANDO-PASCO-HILLSBOROUGH- ",
      "800 PM EDT TUE SEP 21 2010",
      "...HIGH SURF ADVISORY IN EFFECT UNTIL 8 PM EDT WEDNESDAY...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HIGH SURF ADVISORY...WHICH IS IN EFFECT UNTIL 8 PM EDT WEDNESDAY.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A HIGH SURF ADVISORY MEANS THAT HIGH SURF WILL AFFECT BEACHES IN THE ADVISORY AREA...PRODUCING RIP CURRENTS AND LOCALIZED BEACH EROSION.",
      "$$",
                     ],
    },
    {
    "name": "ETNReset_13a",
    "commentary": "expire time for the event",
    "drtTime": "20100923_0000",
    "gridsStartTime": "20100922_0000",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "SU.Y", ["FLZ048","FLZ049","FLZ051"]),
       ],
    "vtecMode": "O",
    "checkStrings": [
      "WHUS42 KTBW 230000",
      "CFWTBW",
      "COASTAL HAZARD MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "800 PM EDT WED SEP 22 2010",
      "FLZ048-049-051-230100-",
      "/O.EXP.KTBW.SU.Y.0006.000000T0000Z-100923T0000Z/",
      "HERNANDO-PASCO-HILLSBOROUGH-",
      "800 PM EDT WED SEP 22 2010",
      "...HIGH SURF ADVISORY HAS EXPIRED...",
      "THE HIGH SURF ADVISORY IS NO LONGER IN EFFECT.",
      "$$",

                     ],
    },
    {
    "name": "ETNReset_14",
    "commentary": "Issue an operational SU.Y event, with new ETN.",
    "drtTime": "20100928_0000",
    "gridsStartTime": "20100928_0000",
    "productType": "Hazard_CFW_Local",
    "createGrids": [
       ("Fcst", "Hazards", "DISCRETE", -100, 100, "<None>", "all"),
       ("Fcst", "Hazards", "DISCRETE", 0, 24, "SU.Y", ["FLZ048","FLZ049","FLZ051"]),
       ],
    "vtecMode": "O",
    "checkStrings": [
      "WHUS42 KTBW 280000",
      "CFWTBW",
      "COASTAL HAZARD MESSAGE",
      "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
      "800 PM EDT MON SEP 27 2010",
      "FLZ048-049-051-280800-",
      "/O.NEW.KTBW.SU.Y.0007.100928T0000Z-100929T0000Z/",
      "HERNANDO-PASCO-HILLSBOROUGH-",
      "800 PM EDT MON SEP 27 2010",
      "...HIGH SURF ADVISORY IN EFFECT UNTIL 8 PM EDT TUESDAY...",
      "THE NATIONAL WEATHER SERVICE IN TAMPA BAY RUSKIN HAS ISSUED A HIGH SURF ADVISORY...WHICH IS IN EFFECT UNTIL 8 PM EDT TUESDAY.",
      "|* SEGMENT TEXT GOES HERE *|.",
      "A HIGH SURF ADVISORY MEANS THAT HIGH SURF WILL AFFECT BEACHES IN THE ADVISORY AREA...PRODUCING RIP CURRENTS AND LOCALIZED BEACH EROSION.",
      "$$",
                     ],
    },
    {
    "commentary": "Cleanup.",
    "name": "ETNReset_15",
    "productType": None,
    "clearHazardsTable": 1,
    "checkStrings": [],
    }
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
        "cmdLineVars": "{('Issued By', 'issuedBy'): None}",
        "deleteGrids": [("Fcst", "Hazards", "SFC", "all", "all")],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)




