##
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

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

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
