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
# SurfaceTemp
#
#   Type: table
#   Edit Areas: solicited from user
#   To Run:
#      Products-->Generate Products
#      Choose Edit Areas and Period (default is every 3 hours)
#      Select OK
#
########################################################################
## EXAMPLE OUTPUT

##    Experimental Surface Temperature Guidance Product

##    Edit Area      12Z/29  18Z/29  0Z/1  6Z/1  12Z/1  18Z/1  0Z/2

##    COAdams          39      39     39    39     39     39    39
##    COArapahoe       39      39     39    39     39     39    39
##    COBoulder        28      28     28    28     28     28    28
##    COClearCreek     21      21     21    21     21     21    21
##    CODenver         37      37     37    37     37     37    37
##    CODouglas        34      34     34    34     34     34    34
##    COElbert         39      39     39    39     39     39    39



########################################################################
Definition =  {

      "type": "table",
      "displayName": "TEST_Surface Temperature Table", # for Product Generation Menu

        # Output file for product results
      "outputFile": "./SurfaceTemp.txt",  # default output file

      "constantVariable": "WeatherElement",
      "rowVariable": "EditArea",
      "columnVariable": "TimePeriod",

      "beginningText": "Experimental Surface Temperature Guidance Product \n\n",
      "endingText": "",

      # Edit Areas
      "defaultEditAreas" : [("area1","Area 1"),("area2","Area 2")],
      "runTimeEditAreas": "yes",
      "areaType" : "Edit Area", # E.g. City, County, Basin, etc.

      # Time Ranges
      "defaultRanges":["Today"],
      "runTimeRanges": "no",

      "elementList": [
                ("T","Temp",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                 ],


      "timePeriod": 3,
      "runTimePeriod": "yes",  # If yes, ask user at run time for period
    }
