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
# PeriodByArea
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
##    Period By Area Tables
##
##    Temperatures for Today
##
##    Time Period   Area 1  Area 2
##
##    12Z/17          48      50
##    15Z/17
##    18Z/17          58      57
##    21Z/17
##
##
##    Temperatures for Tonight
##
##    Time Period   Area 1  Area 2
##
##    0Z/18           59      58
##    3Z/18
##    6Z/18           51      50
##    9Z/18
##
##
##    Temperatures for Tomorrow
##
##    Time Period   Area 1  Area 2
##
##    12Z/18          46      45
##    15Z/18
##    18Z/18
##    21Z/18
##
##
########################################################################
Definition =  {

      "type": "table",
      "displayName": "TEST_Period By Area Table", # for Product Generation Menu

        # Output file for product results
      "outputFile": "./PeriodByArea.txt",  # default output file

      "constantVariable": "WeatherElement",
      "rowVariable": "TimePeriod",
      "columnVariable": "EditArea",

      'beginningText': 'Period By Area Tables \n\n',
      'endingText': '\n\n',
      "timeRangeLoopBegText" : "Temperatures for %TimeRange \n\n",
      "timeRangeLoopEndText" : "\n\n",

      # Edit Areas
      "defaultEditAreas" : [("area1","Area 1"),("area2","Area 2")],
      "runTimeEditAreas": "yes",

      # Time Ranges
      "defaultRanges":["Today","Tonight","Tomorrow"],
      "runTimeRanges": "yes",

      "areaType" : "Edit Area", # E.g. City, County, Basin, etc.

      "elementList": [
                ("T","Temp",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                 ],

      "timePeriod": 3,
      "runTimePeriod": "yes",  # If yes, ask user at run time for period
    }
