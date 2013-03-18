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
# PeriodByElement
#
#   Type: table
#   Edit Areas: solicited from user
#   To Run:
#      Products-->Generate Products
#      Choose Period (default is 3 hours per column), Edit Area(s),
#        and Time Range(s)
#      Select OK
#
########################################################################
## EXAMPLE OUTPUT

##    Period By Element Table.
##
##    Today
##
##    Area 1 for Today
##
##    Time Period   Sky (%)  Wind (mph)   Temp   Precip (%)
##
##    06Z/17           79      NE 10       48
##    09Z/17                                          0
##    12Z/17           97      SE  5       58
##    15Z/17                                         20
##
##
##    Area 2 for Today
##
##    Time Period   Sky (%)  Wind (mph)   Temp   Precip (%)
##
##    06Z/17           92      NE  5       50
##    09Z/17                                         22
##    12Z/17          100       E  5       57
##    15Z/17                                         43
##
##
##
##
##    Tonight
##
##    Area 1 for Tonight
##
##    Time Period   Sky (%)  Wind (mph)   Temp   Precip (%)
##
##    18Z/17          100      SE 10       59
##    21Z/17                                         33
##    00Z/18          100      SE  5       51
##    03Z/18                                         27
##
##
##    Area 2 for Tonight
##
##    Time Period   Sky (%)  Wind (mph)   Temp   Precip (%)
##
##    18Z/17           99      SE 10       58
##    21Z/17                                         43
##    00Z/18           94       S  5       50
##    03Z/18                                          3
##
##
##
##
##    Tomorrow
##
##    Area 1 for Tomorrow
##
##    Time Period   Sky (%)  Wind (mph)   Temp   Precip (%)
##
##    06Z/18           86       S  5       46
##    09Z/18                                         53
##    12Z/18                                         53
##    15Z/18                                         53
##
##
##    Area 2 for Tomorrow
##
##    Time Period   Sky (%)  Wind (mph)   Temp   Precip (%)
##
##    06Z/18           68      SW  5       45
##    09Z/18                                         29
##    12Z/18                                         29
##    15Z/18                                         29
##
########################################################################
Definition =  {

      "type": "table",
      "displayName": "TEST_Period By Element Table", # for Product Generation Menu

        # Output file for product results
      "outputFile": "./PeriodByElement.txt",  # default output file

      "constantVariable": "EditArea",
      "rowVariable": "TimePeriod",
      "columnVariable": "WeatherElement",

      "beginningText": "Period By Element Table. \n\n",
      "endingText": "\n\n",
      "editAreaLoopBegText": "%EditArea for %TimeRange \n\n",
      "editAreaLoopEndText": "\n\n",
      "timeRangeLoopBegText": "%TimeRange \n\n",
      "timeRangeLoopEndText": "\n\n",
      "outerLoop": "TimeRange",

      # Edit Areas
      "defaultEditAreas" : [("area1","Area 1"),("area2","Area 2")],
      "runTimeEditAreas": "yes",

      # Time Ranges
      "defaultRanges":["Today","Tonight","Tomorrow"],
      "runTimeRanges": "yes",

      "elementList": [
                ("Sky", "Sky (%)",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                ("Wind","Wind (mph)",
                 "vectorRange",
                 "avgValue",
                 "Vector", 5, "ktToMph"),
                ("T","Temp",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                ("PoP", "Precip (%)",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                 ],

      "timePeriod": 3,
      "runTimePeriod": "yes",  # If yes, ask user at run time for period
      "periodLabelFormat" : ("LT", "", "%HZ/%d", ""),

    }
