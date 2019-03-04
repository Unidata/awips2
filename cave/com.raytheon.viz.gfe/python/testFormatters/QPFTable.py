##
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
