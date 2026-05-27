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
# ElementByPeriod
#
#   Type: table
#   Edit Areas: solicited from user
#   To Run:
#      Products-->Generate Products
#      Choose Period (default is 3 hours per column) and Edit Area
#      Select OK
#
########################################################################
## EXAMPLE OUTPUT
##
## Weather Element Table for BOU.
##
## Weather Element   12Z/9  15Z/9  18Z/9  21Z/9
##
## Sky (%)             74     72     70     68
## Wind (mph)         W 20   W 20   W 20   W 15
## Temp                33     34     35     35
## Precip (%)          60     60            81
##
########################################################################

Definition =  {

      "type": "table",
      "displayName": "TEST_Element By Period", # for Product Generation Menu

        # Output file for product results
      "outputFile": "./ElementByPeriod.txt",  # default output file

      "constantVariable": "EditArea",
      "rowVariable": "WeatherElement",
      "columnVariable": "TimePeriod",

      "beginningText": "Weather Element Table for %EditArea. \n\n",
      "endingText": "",

      "defaultEditAreas": [("area1","Area 1")],
      "runTimeEditAreas" : "yes", # if yes, ask user at run time
      "defaultRanges": ["Today"],
      "runTimeRanges" : "no", # if yes, ask user at run time

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

    }
