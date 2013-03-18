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
# RDFcst
#
########################################################################
## EXAMPLE OUTPUT:
##
##    24 Hour Tabular Forecast for Boulder for 12 AM MST Mar 21 TO 12 PM MST Mar 21.
##
##    Weather Element          12 AM    3 AM    6 AM    9 AM
##
##    Temperature                 30      28      29
##    Dew Point                   25      23      25
##    Wind (mph)               NW  4   NW  5   NW  6
##    Sky Cover(%)            MOSTLY  MOSTLY  MOSTLY
##                            CLOUDY  CLOUDY  CLOUDY
##    Rainfall Amount(in.)      0.00    0.00    0.00    0.00
##    Weather                   RAIN    RAIN
##    Snowfall(in.)                0       0       0       0
##
##    This forecast was generated from a gridded database.


# Forecast Definition
RDFcst =  {

    ## General Set-Up

      "type": "table",
      "displayName": "RDFcst", # for Product Generation Menu

        # Output file for product results
      "outputFile": "/home/ifp/release/products/TEXT/RDFcst.txt",  # default output file
      "runTimeOutputFile": "no",  # If yes, ask user at run time

        # Language
      "language": "english",  # default
      "runTimeLanguage": "no",  # If yes, ask user at run time

        # Line Length for resulting Product
      "lineLength": 79,  # default
      "runTimeLineLength": "no",  # If yes, ask user at run time

        # Text to preceed and follow the text product.
        # Remember to add spacing lines backslash n.
        # The variables: %TimePeriod, %EditArea, and %WeatherElement
        # can be included to be filled in with constant variables.
        # For phrase and combo, only %EditArea and %TimePeriod
        # can be filled in.
      "timePeriodMethod": "localTRLabel",    ## localRangeLabel
      "editAreaLoopBegText": "24 Hour Tabular Forecast for %EditArea for %TimePeriod. \n\n",
      "editAreaLoopEndText": "\n",
      "endingText": "\nThis forecast was generated from a gridded database.\n\n\n",

    ## Table Layout
        # A table is a combination of three variables:
        #    edit areas, weather elements, and time periods
        # One of these variables is held constant,
        #    one is assigned to rows and the other to columns.

      "constantVariable": "EditArea",    ## "TimePeriod",
      "rowVariable": "WeatherElement",   ## "EditArea",
      "columnVariable": "TimePeriod",    ## "WeatherElement",
      "columnJustification":"Right",

    ## Edit Areas
        # If the edit area is the constant variable, specify
        # one area and whether to ask user at run time.
        # runTimeEditArea can be a list of areas and/or edit area
        #  groups (groups will be expanded into areas) from which
        #  the user will be able to choose.
      "defaultEditAreas": [
                    ("area1", "Area1"),
                    ("area2", "Area2"),
                    ("area3", "Area3"),
                    ("area4", "Area4")],
      "runTimeEditAreas": "yes",
      "areaType" : "Edit Area", # E.g. City, County, Basin, etc.

      # Time Ranges
      "defaultRanges": ["Today"],
      "runTimeRanges" : "no", # if yes, ask user at run time

    ## Weather Elements
        #   elementList:   List of Weather Element tuples:
        #     Weather Element Name
        #     Weather Element Label
        #        If you want the label to appear on multiple lines,
        #        use vertical bars as separators e.g. Maximum|Temperature
        #     Analysis method -- Method to produce statistics from the data
        #     ReportAs Method -- Method to format the analyzed value(s)
        #     DataType: Scalar or Vector or Weather
        #     Rounding increment e.g. 5 = round final value to
        #             nearest multiple of 5
        #     Conversion method
        #         e.g. "mphToKt" converts from mph to knots
        #
        # If the weather element is the constant variable, only one
        # should be given.

        # Name     , Label    , Analysis Method , ReportAs Method ,
        # DataType , Rounding , Conversion

      "elementList": [
                ("T","Temperature",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                ("Td","Dew Point",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                ("RH","Relative Humidity(%)",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                ("WindChill","Wind Chill(F)",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                ("Wind","Wind (mph)",
                 "vectorRange",
                 "avgValue",
                 "Vector", 1, "ktToMph"),
                ("Sky","Sky Cover(%)",
                 "avg",
                 "cloudCover",
                 "Scalar", 5, None),
                ("QPF","Rainfall Amount(in.)",
                 "avg",
                 "singleValue",
                 "Scalar", .01, None),
                ("Wx","Weather ",
                 "dominantWx",
                 "short_weather_phrase",
                 "Scalar", 1, None),
                ("SnowAmt","Snowfall(in.)",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                ("PoP", "Precip (%)",
                 "avg",
                 "singleValue",
                 "Scalar", 1, None),
                 ],

    ## Time Period (If rows or columns vary with TimePeriod
        #   timePeriod: This is the interval in hours for sampling the data
        #      e.g. every 3 hours.
        #   (Can be floating point e.g. 1.5 hour TimePeriods)
      "timePeriod": 3,
        #    timeSpan: This is the amount of data to sample at each
        #     interval.
        #     If you want the data analyzed (e.g averaged) over the
        #     entire period, the timeSpan should be set to "timePeriod".
        #     If you only want data for the beginning of each timePeriod,
        #     the timeSpan should be set to number of hours over which
        #     to analyze the data e.g. 1 hour
      "timeSpan": 1,
      "runTimePeriod": "no",  # If yes, ask user at run time for period
        # Method to label periods given a time range
        #   periodLabel -- GMT time hourZ/day e.g. 15Z/4
        #   localTimeLabel -- local time e.g. 6 AM
        #   localRangeLabel -- local time range e.g. 6AM-9AM
      "periodLabelMethod": "localTimeLabel",

    ## User-supplied Methods
        #   loopMethod: Method to be called for each row.
        #     Such a method might keep ongoing statistics about table data.
        #      Arguments: (rowLabel, rowEntries, userDict, argDict)
        #              Returns: nothing
        #       "rowEntries" is a list of (colValue, value) tuples
        #          describing the entries in this row.
        #       "userDict" is a dictionary set up for user-defined
        #          callback methods so they can keep ongoing data as
        #          the table is being produced.
        #          It is not modified by the TextFormatter code.
      "loopMethod": None,

        #   endMethod: Method to be called after table is complete.
        #     Arguments: (table, userDict, argDict)
        #     Returns: table (could be modified)
        #        The table can be modified to report summary statistics
        #        gathered in userDict.
        #
      "endMethod": None,
    }
