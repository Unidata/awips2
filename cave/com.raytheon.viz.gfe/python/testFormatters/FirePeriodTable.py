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
########################################################################
# FirePeriodTable
#
#   Type: table
#   Edit Areas: solicited from user
#   Weather Elements:  You must have these Weather elements defined in
#       your server: Sky, LAL,  RelHum, MaxT,  MinT,  FreeWind,
#                    Haines, TransWind, MixHgt(ft agl)
#   To Run:
#      Set GFE Time Range
#      Products-->Generate Products
#      Choose Edit Areas
#      Select OK
#
########################################################################
## EXAMPLE OUTPUT (Scarce Data)

##    Fire Period Table for Feb 29 00 17:00:00 GMT - Mar 01 00 11:00:00 GMT.

##    Edit Area      Sky (%)  LAL  RelHum (%)  MaxT  MinT  FreeWind(mph)  Haines  TransWind(mph)  MixHgt(ft agl)

##    COAdams          36-23                    46    26
##    COArapahoe       34-24                    46    26
##    COBoulder        31-52                    34    18
##    COClearCreek     16-57                    26    12
##    CODenver         37-40                    43    25
##    CODouglas        24-47                    40    21
##    COElbert         31-22                    46    25



########################################################################
Definition =  {

      "type": "table",
      "displayName": "TEST_Fire Period Table", # for Product Generation Menu

        # Output file for product results
      "outputFile": "./FirePeriodTable.txt",  # default output file

      "constantVariable": "TimePeriod",
      "rowVariable": "EditArea",
      "columnVariable": "WeatherElement",

      "beginningText": "Fire Period Table for %TimePeriod. \n\n",
      "endingText": "",

      # Edit Areas
      "defaultEditAreas" : [("area1","Area 1"),("area2","Area 2")],
      "runTimeEditAreas": "yes",
      "areaType" : "Edit Area", # E.g. City, County, Basin, etc.

      # Time Ranges
      "defaultRanges": ["Today"],
      "runTimeRanges" : "no", # if yes, ask user at run time

      "elementList": [
                ("Sky", "Sky (%)",
                 "minMax",
                 "range2Value",
                 "Scalar", 1, None),
                ("LAL","LAL",
                 "minMax",
                 "range2Value",
                 "Scalar",1,None),
                ("MaxT","MaxT",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                ("MinT","MinT",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                ("FreeWind","FreeWind(mph)",
                 "vectorRange",
                 "range2Value",
                 "Vector", 1, "ktToMph"),
                ("Haines","Haines",
                 "minMax",
                 "range2Value",
                 "Scalar",1,None),
                ("TransWind","TransWind(mph)",
                 "vectorRange",
                 "range2Value",
                 "Vector", 1, "ktToMph"),
                ("MixHgt", "MixHgt(ft agl)",
                 "minMax",
                 "range2Value",
                 "Scalar",10,None),
             ],
       }
