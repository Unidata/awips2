# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Aviation_SkyFromSatellite - Version 20191212
#
# Author:
# Modified to work in both A1 and A2 environments by Tom LeFebvre - 6/13/14
# Renamed to Aviation_ and removed A1 any code. - 2/29/16 - Tom LeFebvre
# ----------------------------------------------------------------------------

import SmartScript

ToolType = "numeric"
WeatherElementEdited = "Sky"
HideTool = 0

# If desired, Set up variables to be solicited from the user:
VariableList = [
    (
        "Satellite Product:",
        "redVisible",
        "radio",
        [
            "redVisible",
            "blueVisible",
            "Veggie",
            "ir3.9",
            "ir10.33",
            "ir12.29",
            "Fog Product",
        ],
    ),
    ("Threshold for clear:", 0, "numeric"),
    ("Threshold for cloudy:", 5, "numeric"),
]


class Tool(SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, GridTimeRange, varDict):
        "Populates Sky from satellite imagery"

        # Set up Variables from the varDict (see VariableList below)
        product = varDict["Satellite Product:"]
        clear = varDict["Threshold for clear:"]
        cloudy = varDict["Threshold for cloudy:"]

        # Assume we do not have the data we need just yet
        data = None
        Sky = None

        # -----------------------------------------------------------------------
        # See if we have the data we need

        # Fog product
        if product == "Fog Product":

            # Get components the fog product
            data1 = self.getGrids(
                "SAT",
                "ShortwaveWindowIRBand390umE",
                "SFC",
                GridTimeRange,
                noDataError=0,
                mode="Max",
            )
            data2 = self.getGrids(
                "SAT",
                "CleanWindowIRBand1033umE",
                "SFC",
                GridTimeRange,
                noDataError=0,
                mode="Max",
            )

            # Get approximate sunrise and sunset times
            sunrise = self.GM_Day(utc=1)
            sunset = self.GM_Night(utc=1)
            curHour = GridTimeRange.startTime().hour

            # Determine which fog product to produce
            if curHour >= sunrise and curHour < sunset:
                data = data1 - data2
            else:
                data = data2 - data1

        # IR 3.9 micron
        elif product == "ir3.9":

            # Set aside our data
            data = self.getGrids(
                "SAT",
                "ShortwaveWindowIRBand390umE",
                "SFC",
                GridTimeRange,
                noDataError=0,
                mode="Max",
            )

        # IR 11.0 micron
        elif product == "ir10.33":

            # Set aside our data
            data = self.getGrids(
                "SAT",
                "CleanWindowIRBand1033umE",
                "SFC",
                GridTimeRange,
                noDataError=0,
                mode="Max",
            )

        # IR 13.0 micron
        elif product == "ir12.29":

            # Set aside our data
            data = self.getGrids(
                "SAT",
                "DirtyWindowIRBand1229umE",
                "SFC",
                GridTimeRange,
                noDataError=0,
                mode="Max",
            )

        # Visible
        elif product == "redVisible":

            # Set aside our data
            data = self.getGrids(
                "SAT",
                "RedVisibleBand064umE",
                "SFC",
                GridTimeRange,
                noDataError=0,
                mode="Max",
            )

        elif product == "blueVisible":

            # Set aside our data
            data = self.getGrids(
                "SAT",
                "BlueVisibleBand047umE",
                "SFC",
                GridTimeRange,
                noDataError=0,
                mode="Max",
            )

        elif product == "Veggie":

            # Set aside our data
            data = self.getGrids(
                "SAT", "VegetationNIRBand086umE", "SFC", GridTimeRange, noDataError=0
            )

        # Night-time Microphysics R=12.30 -10.35, G=10-35 -3.90, B=10.35

        # -----------------------------------------------------------------------
        # If we have the data we need

        if data is not None:

            # If threshold for cloudy sky is < that for clear sky
            if cloudy < clear:

                # Make masks for cloudy and clear
                # print("cloudy < clear")
                clearMask = data >= clear
                cloudyMask = data <= cloudy

            # Otherwise
            else:

                # Make masks for cloudy and clear
                # print("cloudy >= clear")
                clearMask = data <= clear
                cloudyMask = data >= cloudy

            # Ensure no divide by zero error
            if cloudy == clear:
                cloudy += 0.0001

            # Construct the new Sky grid
            Sky = abs(100 * (data - clear) / (cloudy - clear))
            Sky[clearMask] = 0
            Sky[cloudyMask] = 100

        # Return the new grid
        return Sky

    ############################################################################
    # Define some methods to handle varying sunrise/sunset times
    ############################################################################

    ##
    # Ensures input hour is valid.
    # @param hour: hour to check in 24-hr clock
    # @type hour: integer
    # @return: adjusted hour
    # @rtype: integer
    def GM_hourSanityCheck(self, hour):
        if hour < 0:
            hour += 24
        elif hour > 23:
            hour -= 24

        return hour

    ##
    # Calculates a general sunrise time based on time of year.
    # @param utc: toggle to return time in UTC or local time (default)
    # @type utc: boolean
    # @return: general sunrise hour
    # @rtype: integer
    def GM_Day(self, utc=0):
        """These hours are set for WFO BOX. You should adjust them to better
        reflect the sunrise hours at your office, as needed."""

        # Get current month in local time
        localTime = self._localtime().timetuple()

        # Assume there will be no adjustment to local hour
        adjust = 0

        # If we need to adjust for the UTC hour instead
        if utc:
            dummy, adjust = self._determineTimeShift()
            adjust /= -3600

        # June & July
        if localTime.tm_mon in [6, 7]:
            returnHour = 5 + adjust

        # April, May, August & September
        elif localTime.tm_mon in [4, 5, 8, 9]:
            returnHour = 6 + adjust

        # January, February, March, October, November & December
        else:
            returnHour = 7 + adjust

        # Perform sanity checks on this hour
        return self.GM_hourSanityCheck(returnHour)

    ##
    # Calculates a general sunset time based on time of year.
    # @param utc: toggle to return time in UTC or local time (default)
    # @type utc: boolean
    # @return: general sunset hour
    # @rtype: integer
    def GM_Night(self, utc=0):
        """These hours are set for WFO BOX. You should adjust them to better
        reflect the sunset hours at your office, as needed."""

        # Get current month in local time
        localTime = self._localtime().timetuple()

        # Assume there will be no adjustment to local hour
        adjust = 0

        # If we need to adjust for the UTC hour instead
        if utc:
            dummy, adjust = self._determineTimeShift()
            adjust /= -3600

        # November & December
        if localTime.tm_mon in [11, 12]:
            returnHour = 16 + adjust

        # January & February
        elif localTime.tm_mon in [1, 2]:
            returnHour = 17 + adjust

        # March & October
        elif localTime.tm_mon in [3, 10]:
            returnHour = 18 + adjust

        # April & September
        elif localTime.tm_mon in [4, 9]:
            returnHour = 19 + adjust

        # May, June, July & August
        else:
            returnHour = 20 + adjust

        # Perform sanity checks on this hour
        return self.GM_hourSanityCheck(returnHour)
