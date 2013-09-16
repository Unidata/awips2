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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# SampleAnalysis.py
# Class for producing summary statistics from Sampler data.
# Typically used for Text Product generation.
#
# Author: hansen
# ----------------------------------------------------------------------------
import string, sys, types

import logging
import TimeRange, AbsTime, WeatherSubKey, JUtil
from math import *
from com.raytheon.viz.gfe.sampler import HistValue, HistPair
from com.raytheon.viz.gfe.textformatter import FormatterUtil 

    ##  For complete documentation on the analysis methods available in this class,
    ##  refer to the Text Product User Guide.

    ## Utility Methods:

    ##  The following methods return an integer that indicated the expected
    ##  return type....
    ##  SCALAR
    ##  MAGNITUDE
    ##  DIRECTION
    ##  VECTOR
    ##  WEATHER
    ##
    ##  getModeratedLimits
    ##     returns the min and max percentages allowed for all moderated methods
    ##  getStdDevLimits
    ##     returns the min and max standard deviation limits for all stdDev methods
    ##  getAccumSum
    ##     returns the accumulated sum for the specified time period
    ##  getAccumMinMax
    ##     return the min and max for the specified time period
    ##  getAverage
    ##     returns the absolute average
    ##  getMinMax
    ##     returns the absolute minimum and maximum
    ##  getStdDevAvg
    ##     returns the average after filtering data based on standard deviation
    ##  getStdDevMinMax
    ##     returns the min and max values after filtering based in standard deviation
    ##  getModeratedAvg
    ##     returns the average after filtering based on percentage
    ##  getModeratedMinMax
    ##     returns the min and max after filtering based on percentage
    ##  getVectorAvg
    ##     returns the absolute vector average with no filtering
    ##  getDominantDirection
    ##     returns either the Average or Most Frequent direction based on:
    ##  vectorDirection_algorithm
    ##  getAverageDirection
    ##     returns the average direction over the specified time period
    ##  getMostFrequentDirection
    ##     returns the most frequent direction over the specified time period
    ##  temporalCoverage_flag
    ##     returns 1 if the specified grid sufficiently overlaps the sample time period
    ##  getDominantWx
    ##     returns the dominant weather over the sample period
    ##  getDominantDiscreteValue
    ##     returns the dominant discrete values over the sample period
    ##  getDominant -- handles both getDominantWx and getDominantDiscreteValue
    ##  getSubkey_percentages -- gather all the Weather or Discrete SubKeys and percentages
    ##  dirList
    ##     converts a numerical direction into a string (e.g., N, SE)
    ##  determineGridWeight
    ##     returns the time wieght for a particular grid ( 1.0 = full weight)
    ##  createStats
    ##     reports statistics based on the method specified
    ##  splitRange(timeRange, numPeriods)
    ##     splits a timerange into the specified number of periods and returns a
    ##     timeRange list
    ##  getGridTimeRanges
    ##     returns the list of timeRanges after splitting along sample time periods
    ##  divideRange(timeRange, hours)
    ##     splits a timeRange into sub periods each with the duration specified
    ##  getMode
    ##     returns a range around the median indicating the most frequent values
    ##  getMedian
    ##     returns median value over given timeRange
    ##  getMedianRange
    ##     returns 2-value range chosen around median, uses getRange
    ##  getMedianHistPair
    ##     returns median histPair over given timeRange
    ##  getHistPairMean
    ##     given two HistPairs returns a HistPair representing the mean of the two
    ##  getModeRange
    ##     range chosen around mode, uses getRange
    ##  getModeHistPair
    ##     returns most common HistPair over given timeRange
    ##  getRange
    ##     range chosen around given histPair
    ##     returns: scalar: (min, max)
    ##              vector: (minMag, maxMag, avgDir)
    ##  getDeviation
    ##     returns a deviation around median to include in range
    ##  getBinnedPercent
    ##     returns a list of tuples representing "bins" and corresponding
    ##     percentages of values in each bin
    ##
    ## Conversion methods
    ##  UVToMagDir
    ##  MagDirToUV
    ##  convertAnalysisList
    ## Breakdown of Sampler data:
    ##   HistoSampler   :  Parms, TimeRanges, EditAreas
    ##    Contains SeqOf<ParmHisto>
    ##    ParmHisto:     Parm, TimeRange, EditArea
    ##       Contains SeqOf<HistSample> : (one for each grid overlapping timeRange)
    ##       HistSample is a histogram for a Parm(implicit), Grid, Area
    ##         Contains SeqOf<histPairs>
    ##         HistPair :  Parm(implicit), Grid(implicit), Area(implicit)
    ##                     count, HistValue (value)
    ##               count = how many times that value occurred within the grid
    ##           HistValue value:  scalar, Vector (magnitude, direction), weather

import CommonUtils
import logging

class SampleAnalysis(CommonUtils.CommonUtils):
    def __init__(self):
        CommonUtils.CommonUtils.__init__(self)
        self.log = logging.getLogger("FormatterRunner.SampleAnalysis.SampleAnalysis")
        
    ### CONSTANTS -- Do not override
    def SCALAR(self):
        return 0
    def MAGNITUDE(self):
        return 1
    def DIRECTION(self):
        return 2
    def VECTOR(self):
        return 3
    def WEATHER(self):
        return 4
    def DISCRETE(self):
        return 5

    ### GLOBAL THRESHOLDS AND VARIABLES
    ### To override, override the associated method in your text product class.
    #   To be included in the analysis, a grid must either:
    #   1. Be completely contained in the time range OR
    #   2. Meet BOTH the temporalCoverage_percentage and temporalCoverage_hours
    #      requirements.
    #  The temporalCoverage_percentage is:
        # The percentage of the TIMERANGE covered by the
        #    grid in order to include it in the analysis.
    # The temporalCoverage_hours is:
        # The required hours of overlap of a grid with the TIMERANGE
        #    in order to include it in the analysis.
        #    In addition, if the temporalCoverage_hours is greater than or equal to the
        #    TIMERANGE duration and the grid covers the entire TIMERANGE,
        #    it will be included.
    def temporalCoverage_percentage(self, parmHisto, timeRange, componentName):
        # This is the percentage of the TIMERANGE covered by the
        #    grid in order to include it in the analysis.
        # Percentage of temporal coverage default value (if not found in temporalCoverage_dict)
        # Used by temporalCoverage_flag
        return 20

    def temporalCoverage_dict(self, parmHisto, timeRange, componentName):
        # This is temporalCoverage percentage by weather element
        # Used by temporalCoverage_flag
        return {
                "LAL": 0,
                "MinRH": 0,
                "MaxRH": 0,
                "MinT": 1,
                "MaxT": 1,
                "Haines": 0,
                "PoP" : 0,
                "Hazards" : 0,
                }

    def temporalCoverage_hours(self, parmHisto, timeRange, componentName):
        # This is the required hours of overlap of a grid with the TIMERANGE
        #    in order to include it in the analysis.
        #    In addition, if the temporalCoverage_hours is greater than or equal to the
        #    TIMERANGE duration and the grid covers the entire TIMERANGE,
        #    it will be included.
        # Temporal coverage hours default value
        #     (if not found in temporalCoverage_hours_dict)
        # Used by temporalCoverage_flag
        return 0

    def temporalCoverage_hours_dict(self, parmHisto, timeRange, componentName):
        # This is the temporalCoverage_hours specified per weather element.
        # Used by temporalCoverage_flag
        return {
                #"MinRH": 0,
                #"MaxRH": 0,
                "MinT":  5,
                "MaxT":  5,
                #"Haines":0,
                #"PoP" :  0,
                "pws34": 4,
                "pws64": 4,
                "pwsD34": 4,
                "pwsN34": 4,
                "pwsD64": 4,
                "pwsN64": 4,
                }

    def moderated_dict(self, parmHisto, timeRange, componentName):
        # This dictionary defines the low and high limit at which
        # outliers will be removed when calculating moderated stats.
        # By convention the first value listed is the percentage
        # allowed for low values and second the percentage allowed
        # for high values.
        return {
                "T" : (10, 10),
                "Wind": (0, 20),
                "LAL": (10, 10),
                "MinRH":  (10, 10),
                "MaxRH":  (10, 10),
                "MinT": (10, 10),
                "MaxT": (10, 10),
                "Haines": (10, 10),
                "PoP" : (10, 10),
                }

    def getModeratedLimits(self, parmHisto, timeRange, componentName):
        compositeNameUI = parmHisto.getCompositeNameUI()
        # get the stdDict min and max values
        modMin = self.moderatedDefault(parmHisto, timeRange, componentName)
        modMax = self.moderatedDefault(parmHisto, timeRange, componentName)
        modDict = self.moderated_dict(parmHisto, timeRange, componentName)
        if modDict.has_key(compositeNameUI):
            modMin, modMax = modDict[compositeNameUI]

        return modMin, modMax

    def moderatedDefault(self, parmHisto, timeRange, componentName):
        "Value used by moderated functions if not explicitly defined in moderated_dict"
        return 5

    def maxMode_increment_dict(self, parmHisto, timeRange, componentName):
        return {
            "PoP" : 10,
            }

    def stdDev_dict(self, parmHisto, timeRange, componentName):
        # This dictionary defines the low and high limit at which
        # outliers will be removed when calculating stdDev stats.
        # These tuples represent the (low, high) number of standard
        # deviations.  Any values falling outside this range will
        # not be included in the calculated statistic.
        return {
                "LAL": (1.0, 1.0),
                "MinRH":  (1.0, 1.0),
                "MaxRH":  (1.0, 1.0),
                "MinT": (1.0, 1.0),
                "MaxT": (1.0, 1.0),
                "Haines": (1.0, 1.0),
                "PoP" : (1.0, 1.0),
                "T" : (1.0, 1.0),
                "Wind" : (1.0, 1.0),
                }

    def getStdDevLimits(self, parmHisto, timeRange, componentName):
        compositeNameUI = parmHisto.getCompositeNameUI()
        # get the stdDict min and max values
        stdDevDict = self.stdDev_dict(parmHisto, timeRange, componentName)
        minStdDev = self.stdDevDefault(parmHisto, timeRange, componentName)
        maxStdDev = self.stdDevDefault(parmHisto, timeRange, componentName)
        if stdDevDict.has_key(compositeNameUI):
            minStdDev, maxStdDev = stdDevDict[compositeNameUI]

        return minStdDev, maxStdDev

    def stdDevDefault(self, parmHisto, timeRange, componentName):
        "Value used by all moderated functions if not explicitly defined in stdDev_dict"
        return 1.0

    def vectorDirection_algorithm(self, parmHisto, timeRange, componentName):
        # Algorithm to use for computing vector direction for vector analysis methods.
        # Can be "Average" or "MostFrequent"
        return "Average"

    # Variables for converting Wind Direction from degrees to letters
    def dirList(self):
        dirSpan = 45 # 45 degrees per direction
        base = 22.5 # start with N
        return [
            ('N', 360-base,          361),
            ('N', 0,                 base),
            ('NE',base          ,    base + 1*dirSpan),
            ('E', base + 1*dirSpan,  base + 2*dirSpan),
            ('SE',base + 2*dirSpan,  base + 3*dirSpan),
            ('S', base + 3*dirSpan,  base + 4*dirSpan),
            ('SW',base + 4*dirSpan,  base + 5*dirSpan),
            ('W', base + 5*dirSpan,  base + 6*dirSpan),
            ('NW',base + 6*dirSpan,  base + 7*dirSpan)
            ]


    ########################################
    ## SCALAR

    def avg(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getAverage(self.SCALAR(), parmHisto, timeRange, componentName)"
        result =  self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def minMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        results = self.createStats(parmHisto,timeRange, componentName, args, primaryMethod)
        return results

    def minimum(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractMinMax(result, "Min")

    def maximum(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractMinMax(result, "Max")

    def accumMinMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getAccumMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def accumSum(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getAccumSum(self.SCALAR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def moderatedAccumMinMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModAccumMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def moderatedAccumSum(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModAccumSum(self.SCALAR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def stdDevAccumMinMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getStdDevAccumMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def stdDevAccumSum(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getStdDevAccumSum(self.SCALAR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def median(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getMedian(self.SCALAR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def medianRange(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getMedianRange(self.SCALAR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def mode(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getMode(self.SCALAR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def modeRange(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModeRange(self.SCALAR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def maxMode(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getMaxMode(self.SCALAR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def stdDevAvg(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getStdDevAvg(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def stdDevMin(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getStdDevMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractMinMax(result, "Min")

    def stdDevMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getStdDevMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractMinMax(result, "Max")

    def stdDevMinMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getStdDevMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def stdDevFirstAvg(self, parmHisto, timeRange, componentName, args=None):
        return self.getStdDevAvg(self.SCALAR(), parmHisto, timeRange, componentName, 1)

    def stdDevFirstMinMax(self, parmHisto, timeRange, componentName, args=None):
        return self.getStdDevMinMax(self.SCALAR(), parmHisto, timeRange, componentName, 1)

    def moderatedAvg(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModeratedAvg(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def moderatedMin(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModeratedMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractMinMax(result, "Min")

    def moderatedMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModeratedMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractMinMax(result, "Max")

    def moderatedMinMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModeratedMinMax(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def binnedPercent(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getBinnedPercent(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def moderatedFirstAvg(self, parmHisto, timeRange, componentName, args=None):
        return self.getModeratedAvg(self.SCALAR(), parmHisto, timeRange, componentName, 1)

    def moderatedFirstMinMax(self, parmHisto, timeRange, componentName, args=None):
        return self.getModeratedMinMax(self.SCALAR(), parmHisto, timeRange, componentName, 1)

    def minMaxAvg(self, parmHisto, timeRange, componentName, args=None):
        # Find Min and Max values
        minMax =  self.minMax( parmHisto, timeRange, componentName)
        avg = self.avg(parmHisto, timeRange, componentName)
        if minMax is None or avg is None:
            return None
        else:
            min, max = minMax
            return (min, max, avg)

    def minMaxSum(self, parmHisto, timeRange, componentName, args=None):
        values = parmHisto.minMaxSum()
        if values is None:
            return None
        else:
            minV, maxV, sumV = values
            return minV, maxV, sumV

    def maxAvg(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getMaxAvg(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def stdDevMaxAvg(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getStdDevMaxAvg(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def moderatedMaxAvg(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModeratedMaxAvg(self.SCALAR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def firstAvg(self, parmHisto, timeRange, componentName, args=None):
        return self.getAverage(self.SCALAR(), parmHisto, timeRange, componentName, 1)

    def firstMinMax(self, parmHisto, timeRange, componentName, args=None):
        return self.getMinMax(self.SCALAR(), parmHisto, timeRange, componentName, 1)

    def hourlyTemp(self, parmHisto, timeRange, componentName, args=None):
        "Create hourly temperature stats"
       # Produces a list of hourly temperature values in tuples
       # Each tuple has an average temperature value and
       #     its hour of occurrence
       # Assumptions:
       #   If there is no data for an hour, None is
       #         given instead of a temp value

        if parmHisto.getSampleLen() == 0:
            return None

        start = timeRange.startTime()
        start = AbsTime.absTimeYMD(start.year, start.month,
                                   start.day, start.hour, 0, 0)
        stats = []
        while start < timeRange.endTime():
            # Create Time Range for current hour
            end = start + 3600 # 1 hour in seconds
            hour = start.hour
            tr = TimeRange.TimeRange(start, end)
            #Get the Average T for current hour
            value = self.getAverage(self.SCALAR(), parmHisto, tr, componentName)

            # Append Value and Hour to Stat List
            stats.append((value, hour))
            start = end
        return stats

    ########################################
    ## VECTOR

    def vectorAvg(self, parmHisto, timeRange, componentName, args=None):
        "Create a Vector ((minMag, maxMag), TextDir)  Stats"
        primaryMethod = "self.getAverage(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def vectorMinMax(self, parmHisto, timeRange, componentName, args=None):
        "Create a Vector ((minMag, maxMag), TextDir)  Stats"
        primaryMethod = "self.getMinMax(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def vectorMin(self, parmHisto, timeRange, componentName, args=None):
        "Create a Vector ((minMag, maxMag), TextDir)  Stats"
        primaryMethod = "self.getMinMax(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractVectorMinMax(result, "Min")

    def vectorMax(self, parmHisto, timeRange, componentName, args=None):
        "Create a Vector ((minMag, maxMag), TextDir)  Stats"
        primaryMethod = "self.getMinMax(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractVectorMinMax(result, "Max")

    def vectorMedian(self, parmHisto, timeRange, componentName, args=None):
        "Create a Vector (median, TextDir)  Stats"
        primaryMethod = "self.getMedian(self.VECTOR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def vectorMode(self, parmHisto, timeRange, componentName, args=None):
        "Create a Vector (mode, TextDir)  Stats"
        primaryMethod = "self.getMode(self.VECTOR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def vectorMedianRange(self, parmHisto, timeRange, componentName, args=None):
        "Create a Vector (medianRange, TextDir)  Stats"
        primaryMethod = "self.getMedianRange(self.VECTOR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def vectorModeRange(self, parmHisto, timeRange, componentName, args=None):
        "Create a Vector (modeRange, TextDir)  Stats"
        primaryMethod = "self.getModeRange(self.VECTOR(), parmHisto, timeRange, componentName)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def vectorStdDevAvg(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getStdDevAvg(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod,)
        return result

    def vectorStdDevMinMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getStdDevMinMax(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def vectorStdDevMin(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getStdDevMinMax(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractVectorMinMax(result, "Min")

    def vectorStdDevMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getStdDevMinMax(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractVectorMinMax(result, "Max")

    def vectorModeratedAvg(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModeratedAvg(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod,)
        return result

    def vectorModeratedMinMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModeratedMinMax(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def vectorBinnedPercent(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getBinnedPercent(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return result

    def vectorModeratedMin(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModeratedMinMax(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractVectorMinMax(result, "Min")

    def vectorModeratedMax(self, parmHisto, timeRange, componentName, args=None):
        primaryMethod = "self.getModeratedMinMax(self.VECTOR(), parmHisto, timeRange, componentName)"
        result = self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)
        return self.extractVectorMinMax(result, "Max")

    def vectorMagMinMax(self, parmHisto, timeRange, componentName, args=None):
        "Create a Vector min/max Stats"
        return self.getMinMax(self.MAGNITUDE(), parmHisto, timeRange, componentName)

    def vectorMagMin(self, parmHisto, timeRange, componentName, args=None):
        "Create a Vector min Stats"
        minResult, maxResult = self.getMinMax(self.MAGNITUDE(), parmHisto, timeRange,
                                              componentName)
        return minResult

    def vectorMagMax(self, parmHisto, timeRange, componentName, args=None):
        "Create a Vector max Stats"
        minResult, maxResult = self.getMinMax(self.MAGNITUDE(), parmHisto, timeRange,
                                              componentName)
        return maxResult

    ## This method is being kept for "table" type products
    def vectorRange(self, parmHisto, timeRange, componentName=None):
        "Create a Vector Stats"
        # Split Time Period in half
        # For each half find average values:
        #     mag1 = min Period 1, mag2 = max Period 1,
        #     mag3 = min Period 2, mag4 = max Period 2
        periods = self.splitRange(timeRange)
        period1 = periods[0]
        period2 = periods[1]
        result1 = self.getAverage(self.VECTOR(), parmHisto, period1, componentName)
        result2 = self.getAverage(self.VECTOR(), parmHisto, period2, componentName)

        if result1 is None or result2 is None:
            return None
        mag1, dir1 = result1
        mag2, dir2 = result2
        return (mag1, mag2, dir1, dir2)

    ########################################
    ## WEATHER
    ##
    ## dominantWx
    ##
    ## Thresholds and variables:

    def coverage_weights_dict(self):
        # Weight (between 0 and 1) for the coverage terms
        return {
            "<NoCov>": 0,
            "Iso":    .15,
            "SChc":   .15,
            "Patchy": .15,
            "Areas": .4,
            "Chc":   .4,
            "Sct":   .4,
            "Lkly":  .7,
            "Num":   .7,
            "Brf":   1.0,
            "Frq":   1.0,
            "Ocnl":  1.0,
            "Pds":   1.0,
            "Inter": 1.0,
            "Def":   1.0,
            "Wide":  1.0,
            }

    def wxkey_coverage_weight(self, parmHisto, timeRange, componentName, wxkey):
        # Return a weight (between 0 and 1) for the wxkey coverage term
        cov = wxkey.coverage()
        return self.coverage_weights_dict()[cov]

    def wxkey_coverage_percentage(self, parmHisto, timeRange, componentName, wxkey):
        # Return the required coverage percentage for the given wxkey which will be
        # compared to its "rank" i.e. the percentage of areal coverage over the time period.
        wxType = wxkey.wxType()
        wxCov = wxkey.coverage()
        inten = wxkey.intensity()
        # These rules were from the workshop
        if wxType == "T" and inten == "+":
            return 0
        if wxType in ["ZR", "ZL"]:
            return 0
        # Large Hail
        attrList = wxkey.attributes()
        if "LgA" in attrList:
            return 0
        # Heavy Fog
        if wxType == "F" and inten == "+":
            return 0
        # Low visibility
        if wxType in ["F", "H", "BS", "K", "BD"]:
            vis = wxkey.visibility()
            if  vis  == "1/4SM" or vis == "0SM":
                return 0
        if wxType in ["T", "R", "RW", "S", "SW", "L", "IP"]:
            return 15
        # For the rest: ["F", "H", "BS", "K", "BD", "SA", "LC", "FR", "WG", "VA"]
        return 15

    def checkPercentages(self, parmHisto, timeRange, componentName, wxKey, keyRankDict):
        # If a wxKey does not pass the wxkey_coverage_percentage, this method will be called
        # to give another chance.
        # You can use the keyRankDict:
        #   subkey : (rank, percent coverage)
        # to allow the wxKey to pass based on other values in the grid.
        # For example:  If I have 10% RW 10% SW, neither RW or SW will be reported
        # Using the keyRankDict, I can allow them to pass when I learn 
        #  that 20% of my area is covered with precip.
        # Here's how this might be done:
        #
        #precip = ["SW", "RW", "R", "S"]
        #totalPrecip = 0
        #if wxKey.wxType() in precip:
        #    for subkey in keyRankDict.keys():
        #        if subkey.wxType() in precip:
        #            rank, percent = keyRankDict[subkey]
        #            totalPrecip += percent
        #if totalPrecip > 15:
        #    return 1
        #else:
        #    return 0        
        return 0

    def attribute_coverage_percentage(self, parmHisto, timeRange, componentName, wxType, attr):
        # Return the required coverage percentage for the given attribute.
        # May be based on the wxType and attr if desired.
        if attr == "Dry":
            return 20
        else:
            return 0

    def dominantKeys_threshold(self, parmHisto, timeRange, componentName):
        # This is the maximum number of weather keys desired from the rankedWx method
        return 10

    def cleanOutEmptyValues(self, parmHisto, timeRange, componentName, dataType):
        return 0

    def noWx_percentage(self, parmHisto, timeRange, componentName):
        # If the raw rank (areal and temporal coverage) of NoWx exceeds this value,
        # NoWx will be reported (all other weather keys will be ignored).
        return 100    

    def dominantWx(self, parmHisto, timeRange, componentName, args=None):
        "Return a list of dominant wx subkeys in order by ranking"
        primaryMethod = "self.getDominantWx(parmHisto, timeRange, componentName, withRank=0)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def rankedWx(self, parmHisto, timeRange, componentName, args=None):
        "Return a list of ranked (subkey, ranking) tuples"
        primaryMethod = "self.getDominantWx(parmHisto, timeRange, componentName, withRank=1)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def getDominantWx(self, parmHisto, timeRange, componentName, withRank=0):
         #  Determine the dominant Wx considering the following:
         #     areal coverage over time
         return self.getDominantValues(parmHisto, timeRange, componentName,
                    dataType="WEATHER", withRank=withRank)


    def getDominantValues(self, parmHisto, timeRange, componentName,
                    dataType="WEATHER", withRank=0, withAux=0):
         #  Determine the dominant Wx subkey OR Discrete subkey
         #  considering the following:
         #     areal coverage over time, called the "rank"
         #  Sub-methods:
         #     temporalCoverage_flag
         #     wxKey_coverage_percentage and dominantKeys_threshold  OR
         #     discreteKey_coverage_percentage and dominantDiscreteKeys_threshold
         #  The algorithm:
         #     Temporal coverage by grid: Any individual grid must
         #        cover enough time in the timerange to set the temporalCoverage_flag.
         #     Loop over all samples, which are for all grids and
         #        all weather or discrete keys on each of those grids.
         #        For WEATHER,
         #           We aggregate weather types i.e.
         #           for each weather type (S, RW, R, etc.) we compute
         #           an aggregate subkey such that the resulting rankList
         #           will have just one entry per weather type.
         #              Aggregate Subkey:
         #                coverage: time-weighted average of coverages
         #                intensity: highest ranking OR if ranks are close, dominant
         #                visibility: lowest visibility
         #                attributes: aggregate attributes
         #         Rank: percentage of areal/temporal coverage over time
         #            For WEATHER, we weight this by the coverage term
         #         If a subkey does not meet the "wxkey_coverage_percentage" threshold
         #            or "discreteKey_coverage_percentage", it is removed.
         #       For WEATHER, in all cases, if a subkey has a Primary or Mention attribute,
         #         it automatically "wins" and is used.
         #     Finally, the highest ranking dominantKeys_threshold OR
         #         dominantDiscreteKeys_threshold number of keys are returned.
         #     If withRank == 1: return the ranked list of (subkey, rank) tuples
         #     Else:             return the list of subkeys
         #
        totalHours = 0
        totalPoints = parmHisto.numberOfGridPoints()
        compositeNameUI = parmHisto.getCompositeNameUI()

        #  Loop over all samples, which are for all grids and
        #  all keys on each of those grids.
        #  In this process, we aggregate subkey types e.g.
        #  for each weather type (S, RW, R, etc.) we compute
        #  an aggregate subkey such that the resulting rankList
        #  will have just one entry per weather type.
        #  For discrete, we will have just one entry per
        #  discrete subkey (with or without Aux value).

        #print "\n\nIn getDominantValues: DataType, TimeRange", dataType, timeRange
        #print "STEP 1 -- Aggregate per grid"

        subkeyTypeDict = {}
        # STEP 1:
        #    For each wxType found in the grids,
        #    gather its 'hours' of coverage and 'count' of points covered. 
        for histSample in parmHisto.histoSamples():
            validTime = TimeRange.TimeRange(histSample.validTime())
            if self.temporalCoverage_flag(
                parmHisto, timeRange, componentName, histSample) == 0:
                continue
            #   Get the number of hours inside the timeRange that this
            #   sample comes from (i.e., we can get a sample that lasts
            #   for 3 weeks - but only 1 hour of it is inside the
            #   timeRange - and we only want to rank it by the 1 hour
            #   inside the range)
            #
            hours = validTime.intersection(timeRange).duration()/3600
            if hours < 1:
                continue

            totalHours += hours

            # Gather the subkey Types for this grid in subkeyTypeDict
            #  Each entry ends up being a list of tuples: (subkey, hours, count)
            self.gatherSubkeyTypes(
                parmHisto, timeRange, componentName, histSample, dataType, hours,
                subkeyTypeDict, withAux)

        # STEP 2: For each subkeyType,
        #     --determine an aggregate subkey and rank i.e.
        #          aggregate areal coverage over time percentage
        #     --compare the rank to coverage threshold for the wxType.
        rankList = []
        #print "subkeyTypeDict", subkeyTypeDict
        subkeyTypePointsDict = {}
        noWxRawRank = 0
        keyRankDict = {}  # Holds:  aggregateKey: (rank, rawRank)
        for subkeyType in subkeyTypeDict.keys():
            #print "\nsubkeyType", subkeyType
            subkeyList = subkeyTypeDict[subkeyType]
            if dataType == "WEATHER":
                covDict = {}
                intenDict = {}
                visDict = {}
                attrList = []
                attrDict = {}
                primaryKey = None
                mentionKey = None
            subkeyTypeRank = 0
            # IF WEATHER:
            #    Gather the coverages, intensities, visibilities and attributes
            #    for this weather type
            #    If Primary or Mention, use the subkey as the aggregate key
            # Determine a subkeyType rank
            subkeyTotalPoints = 0
            for subkey, hours, count in subkeyList:
                #print "   subkey, hours, count", subkey, hours, count
                subkeyTotalPoints += count
                if dataType == "WEATHER":
                    attrs = subkey.attributes()
                    attrList = attrList + attrs
                    if "Primary" in attrs:
                        primaryKey = subkey
                        continue
                    if "Mention" in attrs:
                        mentionKey = subkey
                        continue
                    self.addToDict(covDict, subkey.coverage(), hours, count, 1)
                    self.addToDict(intenDict, subkey.intensity(), hours, count)
                    self.addToDict(visDict, subkey.visibility(), hours, count)
                    for attr in attrs:
                        self.addToDict(attrDict, attr, hours, count)
                subkeyTypeRank += hours * count
            subkeyTypePointsDict[subkeyType] = subkeyTotalPoints

            # Determine aggregate key
            #print "   subkeyTypeRank", subkeyTypeRank, subkeyTotalPoints
            #print "   totalHours, totalPoints", totalHours, totalPoints
            subkeyPoints = subkeyTypePointsDict[subkeyType]
            if dataType == "WEATHER":
                aggregateKey = self.getWxAggregateKey(
                    parmHisto, timeRange, componentName,
                    primaryKey, mentionKey, subkeyType, covDict, intenDict, visDict,
                    attrList, attrDict, totalHours, totalPoints, subkeyPoints)
            else:
                aggregateKey = subkeyType

            # Determine rawRank and rank for the aggregateKey
            if dataType == "WEATHER" \
                   and "Primary" in aggregateKey.attributes():
                rank = 200
                rawRank = 200
            else:
                rawRank = int(round(float(subkeyTypeRank)/(totalHours*totalPoints)*100.0))
                if dataType == "WEATHER":
                    # Save the raw rank for NoWx
                    if aggregateKey.wxType() == "<NoWx>":
                        noWxRawRank = rawRank
                    # Multiply by the coverage weight
                    rank = int(rawRank * self.wxkey_coverage_weight(
                        parmHisto, timeRange, componentName, aggregateKey))
                else:
                    rank = rawRank
            #print "   aggregateKey, rank", aggregateKey, rank, rawRank
            keyRankDict[aggregateKey] = (rank, rawRank)
                    
        # Check to see if each aggregateKey meets the required coverage percentage
        for aggregateKey in keyRankDict.keys():
            rank, rawRank = keyRankDict[aggregateKey]
            if dataType == "WEATHER" \
                   and ("Mention" in aggregateKey.attributes() \
                   or "Primary" in aggregateKey.attributes()):
                rankList.append((aggregateKey, rank))
            else:
                if dataType == "WEATHER":
                    # Use rawRank which is the percentage of areal/temporal coverage
                    threshold = self.wxkey_coverage_percentage(
                            parmHisto, timeRange, componentName, aggregateKey)
                    flag = rawRank >= threshold
                else:
                    threshold = self.discreteKey_coverage_percentage(
                            parmHisto, timeRange, componentName, aggregateKey)
                    flag = rawRank >= threshold
                if not flag:
                    # Get another chance to pass
                    flag = self.checkPercentages(
                        parmHisto, timeRange, componentName, aggregateKey, keyRankDict)
                if flag:
                    rankList.append((aggregateKey, rank))
                else:
                    pass
                    #print "didn't make the cut", rank, aggregateKey

        #print "  rankList", rankList
        if len(rankList) == 0:
           return None

        # Check the NoWx Threshold
        if noWxRawRank > self.noWx_percentage(parmHisto, timeRange, componentName):
            # Report NoWx
            newList = []
            for key, rank in rankList:
                if key.wxType() == "<NoWx>":
                    newList.append((key, rank))
            rankList = newList
            
        # Clean out NoWx and None (Discrete)
        if self.cleanOutEmptyValues(parmHisto, timeRange, componentName, dataType):
            newList = []
            for subkey, rank in rankList:
                if dataType == "WEATHER":
                    if subkey.wxType() == "<NoWx>":
                        continue
                else: # DISCRETE
                    if subkey == "<None>":
                        continue
                newList.append((subkey, rank))
            rankList = newList

        # Sort into ranked order
        # Limit the number of keys returned
        if dataType == "WEATHER":
            rankList.sort(self.rankedSortOrder)
            rankList = [
                (WeatherSubKey.weatherSubKey(self._argDict["dataMgr"], subkey.coverage(), subkey.wxType(), subkey.intensity(),
                                    subkey.visibility(),
                                    self.removeSimilarAttrs(subkey.attributes())),
                 rank) for subkey, rank in rankList
                ]
            dominantKeys = self.dominantKeys_threshold(parmHisto, timeRange, componentName)
        else: # DISCRETE
            rankList.sort()
            dominantKeys = self.dominantDiscreteKeys_threshold(parmHisto, timeRange, componentName)

        if len(rankList) > dominantKeys:
            rankList = rankList[0:dominantKeys]
        if self._debug:
            print "\nSampleAnalysis::ranked", dataType, " \n  TimeRange ", timeRange
            print "  Area", parmHisto.area().getId()
        if withRank:
            if self._debug:
                print "   returning with rank %s" % (rankList)
            return rankList
        else:
            newList = []
            for subkey, rank in rankList:
                newList.append(subkey)
            if self._debug:
                print "   returning %s" % (newList)
            return newList

    def gatherSubkeyTypes(self, parmHisto, timeRange, componentName,
                          histSample, dataType, hours, subkeyTypeDict, withAux):
        for histPair in histSample.histogram():
            count = float(histPair.count())
            if dataType == "WEATHER":
                subkey = WeatherSubKey.WeatherSubKey(histPair.value().weather().get(0))
                subkeyType = subkey.wxType()
                if subkeyType == "RW" or subkeyType == "SW":
                    if subkey.intensity() == "--":
                        subkeyType = subkeyType + "--"
            else: # DISCRETE
                subkeyType = histPair.value().discrete().get(0)
                if withAux == 0:
                    subkeyType = histPair.value().discrete().baseData(histPair.value().discrete().getSiteId(), subkeyType)                    
                subkey = subkeyType
            if subkeyTypeDict.has_key(subkeyType):
                subkeyTypeDict[subkeyType].append((subkey, hours, count))
            # Make new entry
            else:
                subkeyTypeDict[subkeyType] = [(subkey, hours, count)]

    def getWxAggregateKey(self, parmHisto, timeRange, componentName,
                    primaryKey, mentionKey, subkeyType, covDict, intenDict, visDict,
                    attrList, attrDict, totalHours, totalPoints, subkeyPoints):
        #   Compute the aggregate key
        #   If Primary was an attribute in any subkey, take it as the aggregate
        #     plus all the other attributes
        #   Otherwise, if Mention was an attribute in any subkey, take it as the
        #     aggregate plus all the other attributes
        #   Otherwise, compute the aggregate subkey from the coverage, intensity,
        #     visibilities for this subkeyType weighted by temporal and areal coverages
        #print "covDict", covDict
        #print "intenDict", intenDict
        #print "visDict", visDict
        #print "attrDict", attrDict
        if subkeyType in ["RW--", "SW--"]:
            subkeyType = subkeyType.replace("--","")
        aggregateKey = None
        if primaryKey is not None:
            aggregateKey = primaryKey
        elif mentionKey is not None:
            aggregateKey = mentionKey

        if aggregateKey is None:
            algorithm = self.aggregateCov_algorithm(parmHisto, timeRange, componentName)
            aggCov = algorithm(parmHisto, timeRange, componentName,
                subkeyType, "coverage", covDict, totalHours, totalPoints, subkeyPoints)
            aggInten = self.getAggregate(parmHisto, timeRange, componentName,
                subkeyType, "intensity", intenDict, totalHours, totalPoints, subkeyPoints)
            aggVis = self.getAggregate(parmHisto, timeRange, componentName,
                subkeyType, "visibility", visDict, totalHours, totalPoints, subkeyPoints)
            aggAttrs = self.getAggregateAttributes(
                parmHisto, timeRange, componentName,
                subkeyType, attrDict, totalHours, totalPoints, subkeyPoints)
        else:
            aggCov = aggregateKey.coverage()
            aggInten = aggregateKey.intensity()
            aggVis = aggregateKey.visibility()
            aggAttrs = aggregateKey.attributes()
        attrList = self.removeDups(attrList)
        aggregateKey = WeatherSubKey.weatherSubKey(self._argDict["dataMgr"], aggCov, subkeyType, aggInten, aggVis, aggAttrs)
        #print "aggregateKey", aggregateKey
        return aggregateKey

    def addToDict(self, dict, key, hours, count, tuple=0):
       # Add to a dictionary whose values are lists

        if dict.has_key(key):

            if tuple:
                (curValue, curMaxCov) = dict[key]

                if count > curMaxCov:
                    curMaxCov = count

                dict[key] = ((curValue + hours*count), curMaxCov)

            else:
                curValue = dict[key]

                dict[key] = curValue + hours * count

        # Make new entry
        else:
            if tuple:
                dict[key] = ((hours * count), count)
            else:
                dict[key] = hours * count

    def aggregateCov_algorithm(self, parmHisto, timeRange, componentName):
        # The algorithm for choosing the coverage term for multiple
        # instances of a weather type.
        # "getAggregateCov" chooses the coverage with the highest rank
        #    (in terms of areal and temporal coverage.)
        # "getExistingWeightedAggregateCov" chooses the coverage with the
        #    highest WEIGHTED rank that exists in the grids.
        # "getWeightedAggregateCov" computes a weighted average coverage
        #    If the resulting coverage is not in the grids,
        #    "creates" an appropriate coverage.
        # "getHighestWeightedAggregateCov" returns the coverage with the
        #    highest weight in "coverage_weights_dict"
        #
        # For example,
        # If you have
        #    Iso T(covers 50% of the zone)
        #    Sct T(covers 25% of the zone)
        #    Num T(covers 25% of the zone)
        # "getAggregateCov" returns "Iso"
        # "getWeightedAggregateCov" returns "Sct"
        # "getExistingWeightedAggregateCov" returns "Num"
        # "getHighestWeightedAggregateCov" returns "Num"
        #
        # If you have
        #    Iso T(covers 60% of the zone)
        #    Num T(covers 40% of the zone)
        # "getAggregateCov" returns "Iso"
        # "getWeightedAggregateCov" returns "Sct"
        # "getExistingWeightedAggregateCov" returns "Num"
        # "getHighestWeightedAggregateCov" returns "Num"
        #
        return self.getAggregateCov
        # return self.getWeightedAggregateCov
        # return self.getExistingWeightedAggregateCov
        # return self.getHighestWeightedAggregateCov

    # Submitted by Matt Belk 8/04
    def getAggregateCov(self, parmHisto, timeRange, componentName,
                        wxType, wxPart, dict, totalHours, totalPoints,
                        subkeyPoints):
        # From the entries in the dictionary,
        # find the aggregate coverage
        # Return coverage in one of two ways:
        #
        # 1)  Coverage covers >= 90% of zone, or
        # 2)  Coverage has the highest subkey rank

        #  If there is only one coverage
        if len(dict) == 1:
            for key in dict.keys():
                return key

        # Get ready to track properties of all coverage terms
        maxRank = 0.0
        aggCov = None
        sameRank = []
        highRank = []

        #  For each coverage
        for key in dict.keys():
            #  If this is a tuple
            if type(dict[key]) == type(()):
                (sum, max) = dict[key]  #  get the point sum and max coverage
            #  Otherwise, get the point sum and assume a max coverage
            else:
                sum = dict[key]
                max = 0

            #  Compute subkey rank
            subkeyRank = float(sum)/(totalHours * totalPoints) * 100.0
            #  If this is the highest subkey rank we have so far
            if subkeyRank > maxRank:
                #  Store this as the highest subkey rank
                maxRank = subkeyRank
                aggCov = key
                sameRank = []
                sameRank.append(key)
            #  Otherwise, if this ties as the highest subkey rank
            elif subkeyRank == maxRank:
                sameRank.append(key)

            #  If the areal coverage of this subkey coverage is >= 90%
            arealThreshold = self.__dict__.get("_aggregateCov_arealThreshold", 90.0)
            if (max * 100.0)/float(totalPoints) >= arealThreshold:
                #  Store this as a candidate for highest coverage key
                highRank.append(key)

        #  Get ready to process sameRank list (if needed)
        maxVal = 0
        #  If there is more than one key in the highRank list
        if len(highRank) > 0:
            #  Use this list to find the aggregate coverage
            testRank = highRank
        #  Otherwise, if there are items in the sameRank list
        elif len(sameRank) > 0:
            testRank = sameRank

        #  Grab the most significant coverage
        for cov in testRank:
            #  Compute the PoP range and a test value for this coverage
            (lowVal, highVal) = self.coveragePoP_value(cov)
            avgVal = (lowVal + highVal)/2.0
            #  If this is the most significant value
            if avgVal >= maxVal:
                #  Use this coverage
                aggCov = cov
                maxVal = avgVal

        #  if the aggregate coverage is still not found
        if aggCov is None:
            aggCov = self.processNoAggCov(dict, wxType)
        self.debug_print('in getAggregateCov ->   returning %s' % (aggCov), 1)
        return aggCov

    def aggregateCov_weights_dict(self):
        # Weight (between 0 and 1) for the coverage terms
        return {
            "<NoCov>": 0,
            "Iso":    .1,
            "SChc":   .1,
            "Patchy": .1,
            "Areas": .4,
            "Chc":   .4,
            "Sct":   .4,
            "Lkly":  .7,
            "Num":   .7,
            "Brf":   .9,
            "Frq":   .9,
            "Ocnl":  .9,
            "Pds":   .9,
            "Inter": .9,
            "Def":   .9,
            "Wide":  .9,
            }

    def aggregateCov_weight(self, parmHisto, timeRange, componentName, cov):
        # Return a weight (between 0 and 1) for the coverage term
        return self.aggregateCov_weights_dict()[cov]

    def getExistingWeightedAggregateCov(self, parmHisto, timeRange, componentName,
                                wxType, wxPart, dict, totalHours, totalPoints,
                                subkeyPoints):
        # From the entries in the dictionary, find the aggregate coverage by
        # using a weighting scheme.
        # If the resulting coverage is not in the grids, use the coverage
        # with the greatest weight.
        if len(dict) == 1:
            for key in dict.keys():
                return key
        aggCov, wtSum = self.getAggCov_and_WtSum(parmHisto, timeRange, componentName,
                                wxType, wxPart, dict, totalHours, totalPoints,
                                subkeyPoints)
        if aggCov is None:
            aggCov = self.processNoAggCov(dict, wxType)
        return aggCov
                
    def getWeightedAggregateCov(self, parmHisto, timeRange, componentName,
                                wxType, wxPart, dict, totalHours, totalPoints,
                                subkeyPoints):
        # From the entries in the dictionary, find the aggregate coverage by
        # using a weighting scheme.
        # If the resulting coverage is not in the grids, "create" an appropriate
        # coverage.
        if len(dict) == 1:
            for key in dict.keys():
                return key
        aggCov, wtSum = self.getAggCov_and_WtSum(parmHisto, timeRange, componentName,
                                wxType, wxPart, dict, totalHours, totalPoints,
                                subkeyPoints)
        # Assign the new coverage
        popValue = self.coveragePoP_table()
        inGrids = 0
        candidates = []
        aggCov = None
        for key in popValue.keys():
            lowVal, highVal = popValue[key]
            #print "key, low, high", key, lowVal, highVal
            # Ranges are inclusive and not contiguous,
            # so we have to adjust
            lowVal = lowVal - 10
            if wtSum > lowVal and wtSum <= highVal:
                # If this coverage was in the grids,
                # choose it and we're done
                #print "dict", dict
                if key in dict.keys():
                    aggCov = key
                    inGrids = 1
                    break
                else:
                    candidates.append(key)

        #print "inGrids", inGrids
        if not inGrids:
            # If the weighted average was not in the grids,
            # we need to choose a coverage or prob term from
            # the candidates

            # Determine coverage or probability based on
            # first dictionary key
            arealCovs = self.arealCoverages()
            for key in dict.keys():
                if key in arealCovs:
                    areal = 1
                else:
                    areal = 0
                break
            for cov in candidates:
                if cov in arealCovs:
                    covAreal = 1
                else:
                    covAreal = 0
                if covAreal == areal:
                    # Make sure this cov can be used with
                    # the wxType
                    availableCoverages = WeatherSubKey.availableCoverages(self._argDict["dataMgr"], wxType)
                    if cov in availableCoverages:
                        aggCov = cov
                        break
        if aggCov is None:
            aggCov = self.processNoAggCov(dict, wxType)
        return aggCov

    def getAggCov_and_WtSum(self, parmHisto, timeRange, componentName,
                                wxType, wxPart, dict, totalHours, totalPoints,
                                subkeyPoints):
        if len(dict) == 1:
            for key in dict.keys():
                return key, 1
        # Compute weighted Sum
        wtSum = 0.0
        maxContrib=0
        aggCov="<NoCov>"
        for key in dict.keys():
            if type(dict[key]) == type(()):
                (sum, max) = dict[key]
            else:
                sum = dict[key]
            subkeyRank = float(sum)/(totalHours * totalPoints) * 100.0
            #print "key", key
            covLowVal, covHighVal = self.coveragePoP_value(key)
            covWt = self.aggregateCov_weight(
                parmHisto, timeRange, componentName,key)
            #print "  covWt, subkeyRank", covWt, subkeyRank
            #print "  contribution", covWt * subkeyRank
            contrib = covWt * subkeyRank
            wtSum += contrib
            if contrib > maxContrib:
                aggCov = key
                maxContrib=contrib
        #print "weighted value", aggCov, wtSum
        return aggCov, wtSum

    def getHighestWeightedAggregateCov(
        self, parmHisto, timeRange, componentName, wxType, wxPart, dict,
        totalHours, totalPoints, subkeyPoints):
        # Return the Coverage with the highest weight in coverage_weights_dict
        # Throw out Coverages that do not meet the wxkey_coverage_percentage
        # EXCEPT if no Coverages meet that threshold, return the Coverage
        # with the highest percentage.
        #
        # NOTE: In cases where the total percentages, e.g. 5% Sct, 7% Num,
        # do not meet the threshold, it's ok for this method to return Num.
        # because the total percentage will be checked in a later step. 
        #
        # Handle case of only one coverage
        if len(dict) == 1:
            for cov in dict.keys():
                return cov
        # Aggregate Coverage
        aggCov = None
        # Coverage Weight for aggregate Coverage
        maxWt = -1
        # Aggregate Cov for those that do not meet threshold
        aggCovReject = None
        # Max Percentage for those Coverages that do not meet threshold
        maxPercentReject = -1

        for cov in dict.keys():
            covWt = self.aggregateCov_weight(
                parmHisto, timeRange, componentName, cov)
            sum, maxCov = dict[cov]
            percentCov = float(sum)/(totalHours*totalPoints)*100.0
            percentCov = int(round(percentCov))
            # Check to see if it meets threshold
            # Make a temporary wxKey to check wxkey_coverage_percentage
            wxKey = WeatherSubKey.weatherSubKey(self._argDict["dataMgr"], cov, wxType, "<NoInten>", "<NoVis>", [])
            #print "wxKey", wxKey, sum, percentCov
            if percentCov >= self.wxkey_coverage_percentage(
                parmHisto, timeRange, componentName, wxKey):
                if covWt > maxWt:
                    aggCov = cov
                    maxWt = covWt
            else:
                if percentCov > maxPercentReject:
                    aggCovReject = cov
                    maxPercentReject = percentCov
            #print "aggCov, wt",aggCov, maxWt
            #print "aggCovReject, %", aggCovReject, maxPercentReject
        if aggCov is None:
            aggCov = aggCovReject
        #print "Returning", aggCov
        return aggCov

    def processNoAggCov(self, dict, wxType):
        msg = "WARNING -- SampleAnalysis cannot aggregate coverages for " + wxType
        log.warning(msg)
        # There was no appropriate coverage for the wxType and given weight
        # So take any coverage that exists in the grid
        aggCov = "<NoCov>"
        for key in dict.keys():
            aggCov = key
            break
        return aggCov

    def getAggregate(self, parmHisto, timeRange, componentName,
                     wxType, wxPart, dict, totalHours, totalPoints, subkeyPoints):
        # From the entries in the dictionary,
        # find the aggregate wxPart (coverage, intensity, visibility)
        # Do it 2 at a time and aggregate the ranks as you go
        if len(dict) == 1:
            for key in dict.keys():
                return key

        firstTime = 1
        for key in dict.keys():
            sum = dict[key]
            subkeyRank = float(sum)/(totalHours * totalPoints) * 100.0
            if wxPart == "coverage":
                subkey = WeatherSubKey.weatherSubKey(self._argDict["dataMgr"], key, wxType, "<NoInten>", "<NoVis>", [])
            elif wxPart == "intensity":
                subkey = WeatherSubKey.weatherSubKey(self._argDict["dataMgr"], "<NoCov>", wxType, key, "<NoVis>", [])
            elif wxPart == "visibility":
                subkey = WeatherSubKey.weatherSubKey(self._argDict["dataMgr"], "<NoCov>", wxType,"<NoInten>" , key, [])
            if firstTime:
                aggRank = subkeyRank
                curKey = subkey
                firstTime = 0
            else:
                curKey = self.makeAggregateSubkey(curKey, aggRank, subkey, subkeyRank)
                aggRank = int((aggRank + subkeyRank)/2.0)
        exec "aggValue = curKey." + wxPart + "()"
        return aggValue

    def getAggregateAttributes(self, parmHisto, timeRange, componentName,
                               wxType, dict, totalHours, totalPoints, subkeyPoints):
        # Take only attributes that meet the threshold
        attrList = []
        for key in dict.keys():
            sum = dict[key]
            attrRank = float(sum)/(totalHours * totalPoints) * 100.0
            threshold = self.attribute_coverage_percentage(
                parmHisto, timeRange, componentName, wxType, key)
            if attrRank > threshold:
                attrList.append(key)
        return attrList

    def makeSubkeyList(self, weatherKey):
        # Make sure subkeyList is a true list
        length = len(weatherKey)
        newList = []
        index = 0
        for subkey in weatherKey:
            newList.append(subkey)
            index = index + 1
            if index >= length:
                break
        return newList

    def weather_percentages(self, parmHisto, timeRange, componentName, args=None):
        # Return a list of tuples:
        #    weather subkey, percentage of coverage
        # All WeatherSubKeys are included
        return self.getSubkey_percentages(parmHisto, timeRange, componentName,
                                          dataType="WEATHER")

    def getSubkey_percentages(self, parmHisto, timeRange, componentName,
                              dataType="WEATHER", withAux=1):
        # Gather all the Weather or Discrete SubKeys and percentages
        numPoints = parmHisto.numberOfGridPoints()
        percentageList = []

        # Each histSample represents a grid
        # To determine percentage for a weather value, we need
        # to aggregate it over grids
        for histSample in parmHisto.histoSamples():
            timeWeight = self.determineGridWeight(histSample, timeRange)
            for histPair in histSample.histogram():
                count = float(histPair.count())
                gridPercentage = int((count/numPoints) * 100.0)
                timeRangePercentage = gridPercentage * timeWeight
                if dataType == "WEATHER":
                    subkey = WeatherSubKey.WeatherSubKey(histPair.value().weather().get(0))
                else: # DISCRETE
                    subkey = histPair.value().discrete().get(0)
                    if withAux == 0:
                        subkey = histPair.value().discrete().baseData(histPair.value().discrete().getSiteId(), subkey)
                # See if subkey is already in list
                found = 0
                for value, percentage in percentageList:
                    # If so, add it's percentage
                    if value == subkey:
                        found = 1
                        index = percentageList.index((value,percentage))
                        newPercentage = percentage + timeRangePercentage
                        percentageList[index] = (subkey, newPercentage)
                if found == 0:
                    percentageList.append((subkey,timeRangePercentage))
        #print "percentage list", dataType, percentageList
        return percentageList

    ########################################
    ## DISCRETE

    def discreteKey_coverage_percentage(self, parmHisto, timeRange, componentName, keyStr):
        # Return the required coverage percentage for the given wxkey which will be
        # compared to its "rank" i.e. the percentage of areal coverage over the time period.
        return 1

    def dominantDiscreteKeys_threshold(self, parmHisto, timeRange, componentName):
        # This is the maximum number of discrete keys desired from the
        # getDominantDiscreteKey method.
        return 10

    def dominantDiscreteValue(self, parmHisto, timeRange, componentName, args=None):
        "Return the most common discrete value over the given timeRange"
        primaryMethod = "self.getDominantDiscreteValue(parmHisto, timeRange, componentName, withAux=0)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def dominantDiscreteValue_withAux(self, parmHisto, timeRange, componentName, args=None):
        "Return the most common discrete value over the given timeRange"
        primaryMethod = "self.getDominantDiscreteValue(parmHisto, timeRange, componentName, withAux=1)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def rankedDiscreteValue(self, parmHisto, timeRange, componentName, args=None):
        "Return the most common discrete value over the given timeRange"
        primaryMethod = "self.getDominantDiscreteValue(parmHisto, timeRange, componentName, withAux=0, withRank=1)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def rankedDiscreteValue_withAux(self, parmHisto, timeRange, componentName, args=None):
        "Return the most common discrete value over the given timeRange"
        primaryMethod = "self.getDominantDiscreteValue(parmHisto, timeRange, componentName, withAux=1, withAux=1)"
        return self.createStats(parmHisto, timeRange, componentName, args, primaryMethod)

    def getDominantDiscreteValue(self, parmHisto, timeRange, componentName, withRank=0, withAux=0):
        # Return a list of dominant discrete subkeys
        # If withRank, return (key, rank) pairs
        # If withAux, include the auxillary field as part of the key
        return self.getDominantValues(parmHisto, timeRange, componentName,
                    dataType="DISCRETE", withRank=withRank, withAux=withAux)

    def discrete_percentages(self, parmHisto, timeRange, componentName, args=None):
        return self.getSubkey_percentages(parmHisto, timeRange, componentName, dataType="DISCRETE",
                                  withAux=0)

    def discrete_percentages_withAux(self, parmHisto, timeRange, componentName, args=None):
        return self.getSubkey_percentages(parmHisto, timeRange, componentName, dataType="DISCRETE",
                                  withAux=1)

    def discreteTimeRangesByKey(self, parmHisto, timeRange, componentName, args=None):
        return self.getDiscreteTimeRangesByKey(
            parmHisto, timeRange, componentName, args=None, withAux=0)

    def discreteTimeRangesByKey_withAux(self, parmHisto, timeRange, componentName, args=None):
        return self.getDiscreteTimeRangesByKey(
            parmHisto, timeRange, componentName, args=None, withAux=1)

    def getDiscreteTimeRangesByKey(self, parmHisto, timeRange, componentName, args=None,
                                withAux=0):
        # This method returns a list of (discreteSubkey, timeRange) pairs ordered in ascending
        # order by timeRange and then by priority of discrete keys as defined in the
        # serverConfig files.

        keyDict = {}
        covDict = {}
        totalHours = timeRange.duration()/3600
        totalPoints = parmHisto.numberOfGridPoints()

        for histSample in parmHisto.histoSamples():
            validTime = TimeRange.TimeRange(histSample.validTime())
            if self.temporalCoverage_flag(
                parmHisto, timeRange, componentName, histSample) == 0:
                continue

            hours = validTime.intersection(timeRange).duration()/3600
            if hours < 1:
                continue

            for histPair in histSample.histogram():
                keyStr = histPair.value().discrete().get(0) # discrete value
                if withAux == 0:
                    keyStr = histPair.value().discrete().baseData(histPair.value().discrete().getSiteId(), keyStr)

                if keyDict.has_key(keyStr):
                    keyDict[keyStr].append(validTime)
                else:
                    keyDict[keyStr] = [validTime]

                # Keep a running total of the temporal and areal percentages
                count = float(histPair.count())
                if covDict.has_key(keyStr):
                    keyRank = covDict[keyStr]
                    newRank = keyRank + hours*count
                    covDict[keyStr] = newRank
                else:   # new entry
                    covDict[keyStr] = hours*count

##        keyList = covDict.keys()
##        for k in keyList:
##            t, a, n = covDict[k]
##            print "key:", k, "time%:", t, "area#:", a, "totalPoints:", n

        keyList = covDict.keys()
        for keyStr in keyList:
            # get the temporal and areal thresholds
            keyRank = covDict[keyStr]
            rank = int(round(float(keyRank)/(totalHours*totalPoints)*100.0))
            if rank < self.discreteKey_coverage_percentage(
                parmHisto, timeRange, componentName, keyStr):
                # remove the dict entry
                del keyDict[keyStr]

        # glue the timeranges that share a common end/start time
        keyList = []
        for k in keyDict.keys():
            trList = keyDict[k]
            trList.sort()
            tr = trList[0]
            for i in range(1, len(trList)):
                if tr.endTime() == trList[i].startTime():
                    # keep extending the time, if TRs are contiguous
                    tr = TimeRange.TimeRange(tr.startTime(), trList[i].endTime())
                else:
                    # no match, append the tuple
                    keyList.append((k, tr))
                    tr = trList[i]

            #  Don't forget the last one
            keyList.append((k, tr))

        #print "discreteTimeRangesByKey keyList", keyList
        return keyList


    ########################################
    ## UTILITIES


    def determineGridWeight(self, histSample, timeRange):
        # Returns the ratio: histSample overlap duration / timeRange duration
        validTime = TimeRange.TimeRange(histSample.validTime())
        if validTime.contains(timeRange):
            gridWeight = 1.0
        # Determine time histSample intersects timeRange
        else:
            intersect = validTime.intersection(timeRange).duration()
            try:
                gridWeight = float(intersect)/timeRange.duration()
            except:
                gridWeight = 0.0
        return gridWeight

    def createStats(self, parmHisto, timeRange, componentName, args, primaryMethod):
        # Call appropriate methods to produce statistics based on args which tell us
        # how to report the statistics with respect to the time range.
        #
        if args is None:
            exec "result = " + primaryMethod
            return result
        period = args[0]
        if period == 0:
            subRanges = self.getGridTimeRanges(parmHisto, timeRange)
        else:
            subRanges = self.divideRange(timeRange, period)
        statsByRange = []
        for subRange in subRanges:
            timeRange = subRange
            exec "result = " + primaryMethod
            # Handle no data
            # If a subRange has no data return None
            if result is None:
                return None
            statsByRange.append((result, subRange))
        return statsByRange

    def temporalCoverage_flag(self, parmHisto, timeRange, componentName,
                              histSample):
        # Return 1 if the histSample time range is completely included in the timeRange
        #   OR the histSample time range sufficiently covers the timeRange
        #   i.e. meets BOTH the temporalCoverage_percentage and temporalCoverage_hours
        #   requirements.

        # Sub-methods:
        #   temporalCoverage_dict
        #   temporalCoverage_percentage
        #   temporalCoverage_hours
        #   temporalCoverage_hours_dict
        #
        
        # njensen: I changed this to act directly on the java time ranges since they aren't
        # part of the return value and it's faster to skip creating python TimeRanges
        javaValidTime = histSample.validTime()
        javaTimeRange = timeRange.toJavaObj()
        compositeNameUI = parmHisto.getCompositeNameUI()        

        # Is the histSample time range completely included in the timeRange?        
        #if timeRange.contains(validTime):
        if javaTimeRange.contains(javaValidTime):
            result = 1
        # Look at intersection of histSample and timeRange
        else:
            covDict = self.temporalCoverage_dict(parmHisto, timeRange, componentName)
            if compositeNameUI in covDict.keys():
                percentage = covDict[compositeNameUI]
            else:
                percentage = self.temporalCoverage_percentage(
                    parmHisto, timeRange, componentName)
            hoursDict = self.temporalCoverage_hours_dict(
                parmHisto, timeRange, componentName)
            if compositeNameUI in hoursDict.keys():
                coverageHours = hoursDict[compositeNameUI]
            else:
                coverageHours = self.temporalCoverage_hours(
                    parmHisto, timeRange, componentName)
            #intersect = javaValidTime.intersection(javaTimeRange).getDuration()
            intersect = FormatterUtil.getTimeRangeIntersectionDuration(javaValidTime, javaTimeRange)
            # The intersection should be at least the percentage of the timeRange
            # AND at least the number of coverageHours
            fullPeriod = javaTimeRange.getDuration()
            try:
                if fullPeriod > 0:
                    percentIn = float(intersect)/fullPeriod
                else:
                    percentIn = 0.0
                if percentIn > 0 and percentIn >= percentage/100.0:
                    result = 1
                else: # saying no - not enough is inside timeRange"
                    result = 0
            except:  # saying no - could not figure percentIn"
                result =  0
            # If temporal coverage percentage requirement met,
            # check temporal coverage hours requirement
            if result == 1:
                intersectHours = intersect/3600
                trHours = fullPeriod/3600
                if intersectHours >= coverageHours:
                    result = 1
                elif coverageHours >= trHours and intersectHours == trHours:
                    result = 1
                else:
                    result = 0
        return result

    def getAccumSum(self, dataType, parmHisto, timeRange, componentName, firstOnly=0):
        "Return the cummulative sum over the given time period"
        minVal, maxVal, sumVal = parmHisto.minMaxSum()
        return sumVal

    def getAccumMinMax(self, dataType, parmHisto, timeRange, componentName, firstOnly=0):
        "Return the cummulative min/max over the given time period"
        minVal, maxVal, sumVal = parmHisto.minMaxSum()
        return minVal, maxVal

    def getModAccumSum(self, dataType, parmHisto, timeRange, componentName, firstOnly=0):
        "Return the moderated cummulative sum over the given time period"
        minLimit, maxLimit = self.getModeratedLimits(parmHisto, timeRange, componentName)
        minVal, maxVal, sumVal = parmHisto.moderatedMinMaxSum(minLimit, maxLimit)
        return sumVal

    def getModAccumMinMax(self, dataType, parmHisto, timeRange, componentName, firstOnly=0):
        "Return the modereted cummulative min/max over the given time period"
        minLimit, maxLimit = self.getModeratedLimits(parmHisto, timeRange, componentName)
        minVal, maxVal, sumVal = parmHisto.moderatedMinMaxSum(minLimit, maxLimit)
        return minVal, maxVal

    def getStdDevAccumSum(self, dataType, parmHisto, timeRange, componentName, firstOnly=0):
        "Return the standard deviation sum over the given time period"
        minLimit, maxLimit = self.getStdDevLimits(parmHisto, timeRange, componentName)
        minVal, maxVal, sumVal = parmHisto.stdDevMinMaxSum(minLimit, maxLimit)
        return sumVal

    def getStdDevAccumMinMax(self, dataType, parmHisto, timeRange, componentName, firstOnly=0):
        "Return the standard deviation min/max over the given time period"
        minLimit, maxLimit = self.getStdDevLimits(parmHisto, timeRange, componentName)
        minVal, maxVal, sumVal = parmHisto.stdDevMinMaxSum(minLimit, maxLimit)
        return minVal, maxVal

    def getAverage(self, dataType, parmHisto, timeRange, componentName, firstOnly = 0):
        "Return the time weighted average values over the given time period"
        totValue = 0.0
        totWeight = 0.0
        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue
            # return None if no histSample pairs
            if histSample.numOfPoints() == 0:
                return None

            avg = histSample.average(True)
                        
            # njensen: faster to do this without wrapping java objects
            validTime = histSample.validTime()
            weight = FormatterUtil.getTimeRangeIntersectionDuration(validTime, timeRange.toJavaObj())

            if dataType == self.SCALAR():
                value = avg.scalar()
            elif dataType == self.VECTOR():
                value = avg.magnitude()

            # sum weighted averages
            totValue = totValue + weight * value
            totWeight = totWeight + weight

            if firstOnly == 1:
                break

        if totWeight > 0.0:
            result = totValue / totWeight
        else:
            return None

        if dataType == self.VECTOR():
            dir = self.getDominantDirection(dataType, parmHisto, timeRange,
                                            componentName)
            return result, dir

        return result

    def getMinMax(self, dataType, parmHisto, timeRange, componentName,
                  firstOnly = 0):
        "Return the minimum and maximum values over the given time period"
        firstTime = 1
        minValue = 0.0
        maxValue = 0.0
        minResult = 0.0
        maxResult = 0.0
        noData = 1
        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue
            # return None if no histSample pairs
            if histSample.numOfPoints() == 0:
                return None
            noData = 0

            min = histSample.absoluteMin()
            max = histSample.absoluteMax()
            if dataType == self.SCALAR():
                minValue = min.scalar()
                maxValue = max.scalar()
            elif dataType == self.VECTOR() or dataType == self.MAGNITUDE():
                minValue = min.magnitude()
                maxValue = max.magnitude()
            if firstTime == 1:
                firstTime = 0
                minResult = minValue
                maxResult = maxValue
            else:
                if minValue < minResult:
                    minResult = minValue
                if maxValue > maxResult:
                    maxResult = maxValue
            if firstOnly == 1:
                break

        if noData == 1:
            return None
        if dataType == self.VECTOR():
            dir = self.getDominantDirection(dataType, parmHisto,
                                            timeRange, componentName)
            return (minResult, maxResult), dir

        return minResult, maxResult

    def getStdDevAvg(self, dataType, parmHisto, timeRange, componentName,
                     firstOnly = 0):
        "Return the time wieghted average values over the given time period"
        # get the stdDev limits from the stdDev dictionary
        minStd, maxStd = self.getStdDevLimits(parmHisto, timeRange, componentName)
        totValue = 0.0
        totWeight = 0.0
        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue
            # return None if no histSample pairs
            if histSample.numOfPoints() == 0:
                return None

            # In AWIPS1, stdDevAvg utilized a default value of True for 
            # separateMagDir argument
            avg = histSample.stdDevAvg(minStd, maxStd, True)
            validTime = TimeRange.TimeRange(histSample.validTime())
            weight = validTime.intersection(timeRange).duration()

            if dataType == self.SCALAR():
                value = avg.scalar()
            elif dataType == self.VECTOR():
                value = avg.magnitude()

            # sum weighted averages
            totValue = totValue + weight * value
            totWeight = totWeight + weight

            if firstOnly == 1:
                break

        if totWeight > 0.0:
            result = totValue / totWeight
        else:
            return None

        if dataType == self.VECTOR():
            dir = self.getDominantDirection(dataType, parmHisto, timeRange,
                                            componentName)
            return result, dir

        return result

    def getStdDevMinMax(self, dataType, parmHisto, timeRange, componentName,
                        firstOnly = 0):
        "Return the minimum and maximum values over the given time period"
        firstTime = 1
        minValue = 0.0
        maxValue = 0.0
        minResult = 0.0
        maxResult = 0.0
        noData = 1
        # get the stdDev limits from the stdDev dictionary
        minStd, maxStd = self.getStdDevLimits(parmHisto, timeRange, componentName)

        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue
            # return None if no histSample pairs
            if histSample.numOfPoints() == 0:
                return None
            noData = 0

            min = histSample.stdDevMin(minStd)
            max = histSample.stdDevMax(maxStd)
            if dataType == self.SCALAR():
                minValue = min.scalar()
                maxValue = max.scalar()
            elif dataType == self.VECTOR():
                minValue = min.magnitude()
                maxValue = max.magnitude()
            if firstTime == 1:
                firstTime = 0
                minResult = minValue
                maxResult = maxValue
            else:
                if minValue < minResult:
                    minResult = minValue
                if maxValue > maxResult:
                    maxResult = maxValue
            if firstOnly == 1:
                break

        if noData == 1:
            return None
        if dataType == self.VECTOR():
            dir = self.getDominantDirection(dataType, parmHisto,
                                            timeRange, componentName)
            return (minResult, maxResult), dir

        return  minResult, maxResult

    def getModeratedAvg(self, dataType, parmHisto, timeRange, componentName,
                        firstOnly = 0):
        "Return the time weighted average values over the given time period"
        # get the stdDev limits from the stdDev dictionary
        minMod, maxMod = self.getModeratedLimits(parmHisto, timeRange, componentName)
        totValue = 0.0
        totWeight = 0.0
        noData = 1
        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue
            # return None if no histSample pairs
            if histSample.numOfPoints() == 0:
                return None
            noData = 0

            avg = histSample.moderatedAverage(minMod, maxMod, True)
            validTime = TimeRange.TimeRange(histSample.validTime())
            weight = validTime.intersection(timeRange).duration()

            if dataType == self.SCALAR():
                value = avg.scalar()
            elif dataType == self.VECTOR():
                value = avg.magnitude()

            # sum weighted averages
            totValue = totValue + weight * value
            totWeight = totWeight + weight

            if firstOnly == 1:
                break

        if noData == 1:
            return None
        if totWeight > 0.0:
            result = totValue / totWeight
        else:
            return None

        if dataType == self.VECTOR():
            dir = self.getDominantDirection(dataType, parmHisto, timeRange,
                                            componentName)
            return result, dir

        return result

    def getModeratedMinMax(self, dataType, parmHisto, timeRange, componentName,
                           firstOnly = 0):
        "Return the minimum and maximum values over the given time period"
        firstTime = 1
        minValue = 0.0
        maxValue = 0.0
        minResult = 0.0
        maxResult = 0.0
        noData = 1
        # get the stdDev limits from the stdDev dictionary
        minMod, maxMod = self.getModeratedLimits(parmHisto, timeRange, componentName)

        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue
            # return None if no histSample pairs
            if histSample.numOfPoints() == 0:
                return None
            noData = 0

            min = histSample.moderatedMin(minMod)
            max = histSample.moderatedMax(maxMod)
            if dataType == self.SCALAR():
                minValue = min.scalar()
                maxValue = max.scalar()
            elif dataType == self.VECTOR():
                minValue = min.magnitude()
                maxValue = max.magnitude()
            if firstTime == 1:
                firstTime = 0
                minResult = minValue
                maxResult = maxValue
            else:
                if minValue < minResult:
                    minResult = minValue
                if maxValue > maxResult:
                    maxResult = maxValue
            if firstOnly == 1:
                break

        if noData == 1:
            return None
        if dataType == self.VECTOR():
            dir = self.getDominantDirection(dataType, parmHisto, timeRange,
                                            componentName)
            return (minResult, maxResult), dir

        return  minResult, maxResult

    def getMaxAvg(self, dataType, parmHisto, timeRange, componentName):
        "Return the maximum average value over the given time period"
        firstTime = 1
        maxValue = 0.0
        maxResult = 0.0
        noData = 1
        
        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue
            # return None if no histSample pairs
            if histSample.numOfPoints() == 0:
                return None
            noData = 0

            maxV = histSample.average()
            if dataType == self.SCALAR():
                maxValue = maxV.scalar()
            elif dataType == self.VECTOR():
                maxValue = maxV.magnitude()
            if firstTime == 1:
                firstTime = 0
                maxResult = maxValue
            else:
                if maxValue > maxResult:
                    maxResult = maxValue

        if noData == 1:
            return None
        if dataType == self.VECTOR():
            dir = self.getDominantDirection(dataType, parmHisto, timeRange,
                                            componentName)
            return (maxResult, dir)

        return maxResult

    def getStdDevMaxAvg(self, dataType, parmHisto, timeRange, componentName):
        "Return the maximum average value filtering by standard deviation"
        firstTime = 1
        maxValue = 0.0
        maxResult = 0.0
        noData = 1
        # get the stdDev limits from the stdDev dictionary
        minStd, maxStd = self.getStdDevLimits(parmHisto, timeRange, componentName)

        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue
            # return None if no histSample pairs
            if histSample.numOfPoints() == 0:
                return None
            noData = 0

            maxV = histSample.stdDevAvg(minStd, maxStd, True)
            if dataType == self.SCALAR():
                maxValue = maxV.scalar()
            elif dataType == self.VECTOR():
                maxValue = maxV.magnitude()
            if firstTime == 1:
                firstTime = 0
                maxResult = maxValue
            else:
                if maxValue > maxResult:
                    maxResult = maxValue

        if noData == 1:
            return None
        if dataType == self.VECTOR():
            dir = self.getDominantDirection(dataType, parmHisto, timeRange,
                                            componentName)
            return (maxResult, dir)

        return maxResult

    def getModeratedMaxAvg(self, dataType, parmHisto, timeRange, componentName):
        "Return the maximum average value filtering by percentage"
        firstTime = 1
        maxValue = 0.0
        maxResult = 0.0
        noData = 1
        # get the moderated limits from the stdDev dictionary
        minMod, maxMod = self.getModeratedLimits(parmHisto, timeRange, componentName)

        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue
            # return None if no histSample pairs
            if histSample.numOfPoints() == 0:
                return None
            noData = 0

            maxV = histSample.moderatedAverage(minMod, maxMod, True)
            if dataType == self.SCALAR():
                maxValue = maxV.scalar()
            elif dataType == self.VECTOR():
                maxValue = maxV.magnitude()
            if firstTime == 1:
                firstTime = 0
                maxResult = maxValue
            else:
                if maxValue > maxResult:
                    maxResult = maxValue

        if noData == 1:
            return None
        if dataType == self.VECTOR():
            dir = self.getDominantDirection(dataType, parmHisto, timeRange,
                                            componentName)
            return (maxResult, dir)

        return maxResult

    def getVectorAvg(self, histPairs):
        # Temporary method to emulate new HistSample.average(0)
        # to be supplied in next release
        uSum = 0.0
        vSum = 0.0
        totCount = 0
        for histPair in histPairs:
            count = histPair.count()
            totCount = totCount + count
            val = histPair.value()
            uw, vw = self.MagDirToUV(val.magnitude(), val.direction())
            uSum = uSum + uw * count
            vSum = vSum + vw * count

        # calculate the average wind vector
        if totCount > 0:
            u = uSum / float(totCount)
            v = vSum / float(totCount)
            mag, dir = self.UVToMagDir(u, v)
            mag = int(mag + 0.5)
            dir = int(dir + 0.5)
            return HistValue(float(mag), float(dir))
        else:
            return HistValue()

    def extractMinMax(self, minMaxList, minOrMax, dataType=None):
        # returns the min or max value in the list depending on minOrMax
        # minMaxList is a list returned from createStats
        # minOrMax can have the values "Min" or "Max" and nothing else

        if dataType == self.VECTOR():
            return self.extractVectorMinMax(minMaxList, minOrMax)

        # sanity checks - must be a list or tuple
        if type(minMaxList) != types.ListType and \
           type(minMaxList) != types.TupleType:
            return None

        # minOrMax must be "Min" or "Max"
        if not (minOrMax == "Min" or minOrMax == "Max"):
            return None

        if type(minMaxList) is types.TupleType:
            if minOrMax == "Min":
                return minMaxList[0]  # return min value
            elif minOrMax == "Max":
                return minMaxList[1]  # return max value
            else:
                print "extractMinMax error - Bad min/max string:", minOrMax
                print "Must be: 'Min' or 'Max'. "
                return None

        # check for empty list
        if len(minMaxList) <= 0:
            return None

        newList = []
        # loop through and find the min and max
        for (vMin, vMax), timeRange in minMaxList:
            if minOrMax == "Min":
                value = vMin
            else:
                value = vMax
            newList.append((value, timeRange))
        return newList

    def extractVectorMinMax(self, minMaxList, minOrMax):
        # returns the min or max value in the list depending on minOrMax
        # minMaxList is a list returned from createStats
        # minOrMax can have the values "Min" or "Max" and nothing else

        # sanity checks - must be a list or tuple
        if type(minMaxList) != types.ListType and \
           type(minMaxList) != types.TupleType:
            return None

        # minOrMax must be "Min" or "Max"
        if not (minOrMax == "Min" or minOrMax == "Max"):
            return None

        if type(minMaxList) is types.TupleType:
            (minMag, maxMag), dir = minMaxList
            if minOrMax == "Min":
                mag = minMag  # return min value
            elif minOrMax == "Max":
                mag = maxMag  # return max value
            else:
                print "extractMinMax error - Bad min/max string:", minOrMax
                print "Must be: 'Min' or 'Max'. "
                return None
            return (mag, dir)

        # check for empty list
        if len(minMaxList) <= 0:
            return None

        newList = []
        # loop through and find the min and max
        for ((vMin, vMax), dir), timeRange in minMaxList:
            if minOrMax == "Min":
                value = vMin
            else:
                value = vMax
            newList.append(((value, dir), timeRange))
        return newList

    def getDominantDirection(self, dataType, parmHisto, timeRange, componentName):
        # returns the dominant direction according to "vectorDirection_algorithm"
        # which can be "Average" or "MostFrequent"

        if not dataType == self.VECTOR():
            return None
        if self.vectorDirection_algorithm(parmHisto, timeRange, componentName) == "Average":
            return self.getAverageDirection(parmHisto, timeRange, componentName)
        else: #Most Frequent
            return self.getMostFrequentDirection(parmHisto, timeRange, componentName)

    def getAverageDirection(self, parmHisto, timeRange, componentName):
        # returns the dominant direction calculated by assuming a mag of 1 always
        uSum = 0.0
        vSum = 0.0
        totCount = 0
        weight = 0
        totWeight = 0
        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName, histSample) == 0:
                continue
            # sum u and v components assigning a magnitude one 1 always
            histPairs = histSample.histogram()
            for histPair in histPairs:
                validTime = TimeRange.TimeRange(histSample.validTime())
                weight = validTime.intersection(timeRange).duration()
                totWeight = totWeight + weight
                count = histPair.count()
                totCount = totCount + count
                uw, vw = self.MagDirToUV(1.0, histPair.value().direction())
                uSum = uSum + (uw * count) * weight
                vSum = vSum + (vw * count) * weight

        # calculate the average wind vector
        if totCount > 0:
            u = uSum / (float(totCount) * totWeight)
            v = vSum / (float(totCount) * totWeight)
            mag, dir = self.UVToMagDir(u, v)
            return dir
        else:
            return None

    def getMostFrequentDirection(self, parmHisto, timeRange, componentName):
        # returns the most frequent direction binned to 8-point numerical direction
        binDict = {}
        totWeight = 0.0
        #print "\nGetting most frequent", timeRange
        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName, histSample) == 0:
                continue
            numOfPoints = histSample.numOfPoints()
            if numOfPoints == 0:
                return None
            histPairs = histSample.histogram()
            for histPair in histPairs:
                validTime = TimeRange.TimeRange(histSample.validTime())
                weight = validTime.intersection(timeRange).duration()
                weight = weight/float(timeRange.duration()) * 100.0
                totWeight += weight
                count = float(histPair.count())
                binnedDir = self.binDir(histPair.value().direction())
                #print "dir, binnedDir", histPair.value().direction(), binnedDir
                percent = count/numOfPoints * weight
                if binDict.has_key(binnedDir):
                    binDict[binnedDir] += percent
                else:
                    binDict[binnedDir] = percent

        if totWeight == 0.0:
            return None

        # Pick the most frequent direction
        maxFreq = 0
        mostFreqDir = None
        for dir in binDict.keys():
            freq = binDict[dir]
            #print "dir, freq", dir, freq
            if freq > maxFreq:
                maxFreq = freq
                mostFreqDir = dir
        #print "returning", mostFreqDir
        return mostFreqDir

    def binDir(self, dir):
        # Return the "bin" direction value for the given direction
        for textDir, low, high in self.dirList():
            if dir >= low and dir < high:
                # Handle N
                if textDir == "N":
                    return 0
                else:
                    return int(low+high)/2.0


    def splitRange(self, timeRange, numPeriods=2):
        "Split the timeRange into the given number of periods and return the resulting list of time ranges"

        periods = []
        duration = (timeRange.endTime()-timeRange.startTime())/numPeriods
        startTime = timeRange.startTime()
        for i in range(numPeriods):
            endTime = startTime + duration
            newRange = TimeRange.TimeRange(startTime, endTime)
            periods.append(newRange)
            startTime = endTime
        return periods

    def getGridTimeRanges(self, parmHisto, timeRange):
        # Return the set of timeRanges that overlap the specified timeRange
        # If a histSample partially overlaps, trim the timeRange to the
        # specified timeRange's startTime() or endTime()
        subRanges = []
        for histSample in parmHisto.histoSamples():
            tr = TimeRange.TimeRange(histSample.validTime())  # get the histSample timeRange
            overlap = timeRange.intersection(tr).duration()  # calc overlap
            if overlap == 0:   # no overlap -> skip to next grid
                continue
            if overlap == tr.duration():  #  the whole grid is included
                subRanges.append(tr)
            elif timeRange.startTime() > tr.startTime():
                newTR = TimeRange.TimeRange(timeRange.startTime(), tr.endTime())
                subRanges.append(newTR)
            elif timeRange.endTime() < tr.endTime():
                newTR = TimeRange.TimeRange(tr.startTime(), timeRange.endTime())
                subRanges.append(newTR)

        return subRanges

    def getMedianHistPair(self, dataType, parmHisto, timeRange, componentName):
        # Return the median HistPair over the timeRange
        # Note: There could be multiple grids (histSamples) in this timeRange
        # over which we are sampling.
        #
        #  we can't figure a median if there are no samples
        #
        if len(parmHisto.histoSamples()) == 0:
            return None
        #
        #  we can only figure the median based on the scalar value,
        #  or, for vectors, the magnitude or direction.  Other types
        #  are invalid, and we have to return None.
        #
        if ((dataType!=self.SCALAR()) and (dataType!=self.MAGNITUDE())
            and (dataType!=self.DIRECTION())):
            return None
        #
        #  Get the samples inside the time range, keeping track
        #  of the values along the way (to sort later).  Since
        #  there may be several grids, each with the same values
        #  that cross the desired timeRange, we need to add to
        #  the saved histogram counts when we encounter such
        #  values.  Make a key with consistent floating point
        #  numbers so that the sorting works right later.
        #

        totalCount=0
        pairDict = {}
        compositeNameUI = parmHisto.getCompositeNameUI()
        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(
                parmHisto, timeRange, componentName, histSample) == 0:
                continue
            # calc the time weight
            validTime = TimeRange.TimeRange(histSample.validTime())
            weight = validTime.intersection(timeRange).duration()
            for histPair in histSample.histogram():
                tempCount=histPair.count()
                tempValue=histPair.value()
                tempKey=0.0
                if dataType == self.SCALAR():
                   tempKey = tempValue.scalar()
                elif dataType == self.MAGNITUDE():
                   tempKey = tempValue.magnitude()
                elif dataType == self.DIRECTION():
                   tempKey = tempValue.direction()
                valuestring="%020.10f" % float(tempKey)
                totalCount = totalCount + tempCount * weight
                if pairDict.has_key(valuestring):
                    pairDict[valuestring].incrementCount(int(tempCount * weight))
                else:
                    # njensen: I added the clone(), because otherwise we are incrementing
                    # the original histpair reference, which corrupts the statistics when
                    # the same method is called against a different subTimeRange from
                    # createStats
                    pairDict[valuestring] = histPair.clone()                    
                    pairDict[valuestring].incrementCount(int((tempCount * weight) - 1))
        #
        #  if no samples landed within the timeRange then we have
        #  to return a median of None
        #
        if totalCount == 0:
            return None
        #
        #  now we know the total number of pairs in the timeRange
        #  so we figure out the middle pair number and then
        #  go through the pairs in numerical order until we get
        #  to that count value
        #
        medianNumber=int(totalCount/2.0)
        odd = 0
        if medianNumber%2 == 1:
            medianNumber == medianNumber + 1
            odd = 1
        count=0
        names=pairDict.keys()
        names.sort()
        for valueStr in names:
            addCount=pairDict[valueStr].count()
            if ((count+addCount)>=medianNumber):
                if odd == 1:
                    return pairDict[valueStr]
                elif ((count+addCount)) == medianNumber:
                    # need to take mean of this value and the next
                    histPair1 = pairDict[valueStr]
                    index1 = names.index(valueStr)
                    if index1 < len(names)-1:
                        valueStr2 = names[index1+1]
                        histPair2 = pairDict[valueStr2]
                        return self.getHistPairMean(dataType, histPair1, histPair2)
                    else:
                        return histPair1
                else:
                    return pairDict[valueStr]
            count+=addCount
        return None

    def getHistPairMean(self, dataType, histPair1, histPair2):
        # Return a HistPair that represents the mean of the two histPair values
        # for the given dataType which can be "SCALAR", "MAGNITUDE", "DIRECTION"
        if dataType == self.SCALAR():
            val1 = histPair1.value().scalar()
            val2 = histPair2.value().scalar()
            avg = float(val1 + val2)/2.0
            value = HistValue(avg)
            return HistPair(value)

        elif dataType == self.MAGNITUDE():
            val1 = histPair1.value().magnitude()
            val2 = histPair2.value().magnitude()
            avg = float(val1 + val2)/2.0
            value = HistValue(avg, 0.0)
            return HistPair(value)

        else: #dataType == self.DIRECTION():
            dir1 = histPair1.value().direction()
            dir2 = histPair2.value().direction()
            u1, v1 = self.MagDirToUV(1.0, dir1)
            u2, v2 = self.MagDirToUV(1.0, dir2)
            u = (u1 + u2)/2
            v = (v1 + v2)/2
            mag, dir = self.UVToMagDir(u,v)
            value = HistValue(mag, dir)
            return HistPair(value)

    def getModeHistPair(self, dataType, parmHisto, timeRange, componentName):
        # Return the most common HistPair over the timeRange
        # Note: There could be multiple grids (histSamples) in this timeRange
        # over which we are sampling.

        if len(parmHisto.histoSamples()) == 0:
            return None
        maxCount = 0
        modePair = None
        compositeNameUI = parmHisto.getCompositeNameUI()

        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue

            # calc the time weight
            validTime = TimeRange.TimeRange(histSample.validTime())
            weight = validTime.intersection(timeRange).duration()
            for histPair in histSample.histogram():
                if (histPair.count() * weight) > maxCount:
                    maxCount = histPair.count() * weight
                    modePair = histPair

        return modePair

    def getMedian(self, dataType, parmHisto, timeRange, componentName):
        # Return a range around the median
        # For vector also return an average direction over that range.

        if len(parmHisto.histoSamples()) == 0:
            return None

        if dataType == self.VECTOR():
            # For VECTOR, base the median on the magnitude
            pairType = self.MAGNITUDE()
        else:
            pairType = dataType

        # Determine the median
        medianPair = self.getMedianHistPair(pairType, parmHisto, timeRange, componentName)
        if medianPair is None:
            return None
        if dataType == self.VECTOR():
            return (medianPair.value().magnitude(), medianPair.value().direction())
        else:
            return medianPair.value().scalar()

    def getMedianRange(self, dataType, parmHisto, timeRange, componentName):
        # Return a range around the median
        # For vector also return an average direction over that range.

        if len(parmHisto.histoSamples()) == 0:
            return None

        if dataType == self.VECTOR():
            # For VECTOR, base the median on the magnitude
            pairType = self.MAGNITUDE()
        else:
            pairType = dataType

        # Determine the median
        medianPair = self.getMedianHistPair(pairType, parmHisto, timeRange, componentName)
        if medianPair is None:
            return None
#        print "\nGetting Median Range"
        return self.getRange(dataType, medianPair, parmHisto, timeRange, componentName, "Median")

    def getMode(self, dataType, parmHisto, timeRange, componentName):
        # Return a range around the median
        # For vector also return an average direction over that range.

        if len(parmHisto.histoSamples()) == 0:
            return None

        if dataType == self.VECTOR():
            # For VECTOR, base the median on the magnitude
            pairType = self.MAGNITUDE()
        else:
            pairType = dataType

        # Determine the median
        modePair = self.getModeHistPair(pairType, parmHisto, timeRange, componentName)
        if modePair is None:
            return None
#        print "\nGetting Median Range"
        if dataType == self.VECTOR():
            return modePair.value().magnitude(), modePair.value().direction() 
        else:
            return modePair.value().scalar()

    def getModeRange(self, dataType, parmHisto, timeRange, componentName):
        # Return a range around the mode.
        # For vector also return an average direction over that range.

        if len(parmHisto.histoSamples()) == 0:
            return None

        if dataType == self.VECTOR():
            pairType = self.MAGNITUDE()
        else:
            pairType = dataType

        # Determine the median
        modePair = self.getModeHistPair(pairType, parmHisto, timeRange, componentName)
        if modePair is None:
            return None
        return self.getRange(dataType, modePair, parmHisto, timeRange, componentName, "Mode")

    def getMaxMode(self, dataType, parmHisto, timeRange, componentName):
        # Return the maximum mode over all grids

        if len(parmHisto.histoSamples()) == 0:
            return None

        compositeNameUI = parmHisto.getCompositeNameUI()
        incDict = self.maxMode_increment_dict(parmHisto, timeRange, componentName)
        if incDict.has_key(compositeNameUI):
            binRes = incDict[compositeNameUI]
        else:
            binRes = 10  #  default value

        maxMode = -99999
        for histSample in parmHisto.histoSamples():
            # Ignore samples that are less than the temporal threshold
            if self.temporalCoverage_flag(
                parmHisto, timeRange, componentName, histSample) == 0:
                continue
            mode = histSample.mostCommonValueBinned(binRes).scalar()
            if mode > maxMode:
                maxMode = mode

        return maxMode

    def getRange(self, dataType, basePair, parmHisto, timeRange,
                 componentName, rangeBase="Median"):
        # Return a range around the basePair.
        # For vector also return an average direction over that range.

        compositeNameUI = parmHisto.getCompositeNameUI()
        # Determine deviation
        deviation = self.getDeviation(dataType, compositeNameUI, basePair)
        # Take pairs that are within deviation of basePair
        #  and determine min, max and number of points covered by range
        if dataType == self.SCALAR():
            scalarflag = 1
            baseValue = basePair.value().scalar()
        else:
            scalarflag = 0
            baseValue = basePair.value().magnitude()

        min = baseValue
        max = baseValue
        pairs = []
        samplesInRange = 0
        totalCount = 0

        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue
            samplesInRange = samplesInRange + 1
            for histPair in histSample.histogram():
                if scalarflag:
                    histValue = histPair.value().scalar()
                else:
                    histValue = histPair.value().magnitude()
                #print "histvalue, baseValue, deviation", \
                #       histValue, baseValue, deviation
                if histValue >= baseValue - deviation and \
                   histValue <= baseValue + deviation:
                    pairs.append(histPair)
                    if histValue < min:
                        min = histValue
                    if histValue > max:
                        max = histValue
                    totalCount = totalCount + histPair.count()

        # If vector, find average direction for pairs in range
        if dataType == self.VECTOR():
            avgValue = self.getVectorAvg(pairs)
            direction = avgValue.direction()
            return  ((min, max), direction)
        else:
            return (min, max)

    def getDeviation(self, dataType, compositeNameUI, histPair):
        # Returns a deviation around the median to include in range
        if dataType == self.VECTOR():
            mag = histPair.value().magnitude()
            if mag < 15:
                return 3
            elif mag < 30:
                return 5
            else:
                return 8
        else:
            return 10

    def UVToMagDir(self, u, v):
        # Converts u, v to magnitude, direction
        RAD_TO_DEG = 57.296083
        speed = sqrt(u * u + v * v)
        dir = atan2(u, v) * RAD_TO_DEG
        while dir < 0.0:
            dir = dir + 360.0
        while dir >= 360.0:
            dir = dir - 360.0
        #print "Speed, dir ", speed, dir
        return (speed, dir)

    def MagDirToUV(self, mag, dir):
        #Converts magnitude, direction to u, v
        DEG_TO_RAD = 0.017453292
        uw = sin(dir * DEG_TO_RAD) * mag
        vw = cos(dir * DEG_TO_RAD) * mag
        return (uw, vw)

    def convertAnalysisList(self, analysisList):
        # Replace text string methods with SampleAnalysis methods
        newList = []
        for entry in analysisList:
            if len(entry) == 2:
                element, method = entry
                if type(method) == types.StringType:
                    exec "method = self."+method
                newList.append((element,method))
            if len(entry) == 3:
                element, method, args = entry
                if type(method) == types.StringType:
                    exec "method = self."+method
                newList.append((element,method, args))
        return newList

    def bin_dict(self, parmHisto, timeRange, componentName):
        # Bins for binnedPercent. Bins are inclusive.
        return {
            "Sky": [(0,89),(90, 100)],
            "PoP": [(0,4), (5,14), (15,24), (25,34), (35,44), (45,54),
                    (55,64), (65,74), (75,84), (85,94), (95,100)],
            "LAL": [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6)],
            }

    def getBinnedPercent(self, dataType, parmHisto, timeRange, componentName, firstOnly = 0):
        "Returns a list of tuples representing bins and corresponding percentages of values in each bin"
        binsDict = self.bin_dict(parmHisto, timeRange, componentName)
        try:
            bins = binsDict[parmHisto.getCompositeNameUI()]
        except:
            return None

        # Returns a list of tuples
        # Each tuple is of the form:
        #   (lowBin_value, highBin_value, percent)
        #  lowBin_value and highBin_value are the inclusive values of the bin
        #  percent is the percentage of data values in that bin

        percents = []
        for bin in bins:
            percents.append(0.0)
        numBins = len(bins)

        totWeight = 0.0
        for histSample in parmHisto.histoSamples():
            if self.temporalCoverage_flag(parmHisto, timeRange, componentName,
                                          histSample) == 0:
                continue
            # return None if no histSample pairs
            numOfPoints = histSample.numOfPoints()
            if numOfPoints == 0:
                return None

            validTime = TimeRange.TimeRange(histSample.validTime())
            weight = validTime.intersection(timeRange).duration()
            weight = weight/float(timeRange.duration()) * 100.0
            totWeight = totWeight + weight

            for histPair in histSample.histogram():
                if dataType == self.SCALAR():
                    value = histPair.value().scalar()
                elif dataType == self.VECTOR():
                    value = histPair.value().magnitude()
                count = float(histPair.count())
                percent = count/numOfPoints * weight

                # Find the bin for this histPair value
                for i in range(numBins):
                    low,high = bins[i]
                    if value >= low and value <= high:
                        # add to percentage for this bin
                        percents[i] += percent

        if totWeight == 0.0:
            return None

        # Glue the bins and the percents together
        newBins = []
        for i in range(numBins):
            low, high = bins[i]
            newBins.append((low, high, percents[i]))
        #print "returning bins", newBins, timeRange
        return newBins
