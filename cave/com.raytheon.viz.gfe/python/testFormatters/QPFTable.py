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
# QPFTable
#
#   Type: table
#   Edit Areas: solicited from user
#   To Run:
#      Set GFE Time Range
#      Products-->Generate Products
#      Choose Edit Areas
#      Select OK
#
########################################################################
## EXAMPLE OUTPUT

##    Experimental QPF Table
##
##    Today:
##
##    Edit Area       QPF
##
##    COBoulder       0.31
##    COClearCreek    0.33
##    CODenver        0.29
##    CODouglas       0.28
##    COElbert        0.38



########################################################################
Definition =  {

      "type": "table",
      "displayName": "TEST_QPF Table", # for Product Generation Menu

        # Output file for product results
      "outputFile": "./QPFTable.txt",  # default output file

      "constantVariable": "TimePeriod",
      "rowVariable": "EditArea",
      "columnVariable": "WeatherElement",

      "beginningText": "Experimental QPF Table \n\n",
      "endingText": "",
      "timeRangeLoopBegText": "%TimeRange:\n",

      # Edit Areas
      "defaultEditAreas" : [("area1","Area 1"),("area2","Area 2")],
      "runTimeEditAreas": "yes",
      "areaType" : "Edit Area", # E.g. City, County, Basin, etc.

      # Time Ranges
      "defaultRanges": ["Today"],
      "runTimeRanges" : "no", # if yes, ask user at run time

      "elementList": [
                ("QPF", "QPF",
                 "accumSum",
                 "singleValue",
                 "Scalar", .01, None),
                 ],

    }
