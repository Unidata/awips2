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
# ConfigVariables.py
# ConfigVariables for Text Products.
#
# Author: hansen
# ----------------------------------------------------------------------------

import TextUtils

class ConfigVariables(TextUtils.TextUtils):
    def __init__(self):
         TextUtils.TextUtils.__init__(self)         
    
    def missingData_notification_list(self, tree, node):
        # Return a list of weather element names for which you wish
        # to be notified when there is missing data
        return []

    #  Descriptors: Phrase, Unit, Time periods
    #  Connecting phrases
    def phrase_descriptor_dict(self, tree, node):
        # Dictionary of descriptors for various weather elements in phrases
        # The value for an element may be a phrase or a method
        # If a method, it will be called with arguments:
        #   tree, node, key, element
        return {
            "HeatIndex": "heat index readings",
            "PoP": "chance of",
            #"PoP": self.areal_or_chance_pop_descriptor,
            #"PoP": self.allAreal_or_chance_pop_descriptor,
            "Period":"period",
            "IceAccum": "ice accumulation",
            "NewIceAccum": "new ice accumulation",
            "SnowLevel": "snow level",
            "Swell": "swell",
            "TotalSnow": "total snow accumulation",
            "NewTotalSnow": "total new snow accumulation",
            "StormTotalSnow": "storm total snow accumulation",
            "Visibility": "visibility",
            "Wind": "winds",
            "Wind20ft": "winds",
            "WindChill": "wind chill readings",
            "WindGust": "gusts up to",
            "Wx": "",
            #
            # Snow accumulation combinations
            "Snow": "snow accumulation",
            "SnowSnow": "snow accumulation",
            "SnowSleet": "snow and sleet accumulation",
            #"SnowSleetIceCrystal": "snow and sleet and ice crystal accumulation",
            "SnowSleetIceCrystal": "snow and sleet accumulation",
            "Sleet": "sleet accumulation",
            #"SleetIceCrystal": "sleet and ice crystal accumulation",
            "SleetIceCrystal": "sleet accumulation",
            #"IceCrystal":  "ice crystal accumulation",
            #"SnowIceCrystal": "snow and ice crystal accumulation",
            "IceCrystal":  "snow accumulation",
            "SnowIceCrystal": "snow accumulation",
            "New": "new",            
            #
            "evening temperatures": "evening temperatures",
            "highs": "highs",
            "lakeWinds": "caution advised on area lakes",
            "lows": "lows",
            "severeWeather": "some thunderstorms may be severe",
            "thunderstorms": "some thunderstorms may produce",
            "heavyRainfall":  "locally heavy rainfall possible",
            "heavyRain" : "rain may be heavy at times",
            "heavyRains" : "locally heavy rain possible",
            "heavySnow" : "snow may be heavy at times",
            "heavyPrecip" : "precipitation may be heavy at times",
            "temperature":"temperature",
            "higher":{
                "MaxT":"warmer",
                "MinT":"warmer",
                "MinRH":"wetter",
                "MaxRH":"wetter",
                "RH":"wetter",
                },
            "lower":{
                "MaxT":"cooler",
                "MinT":"cooler",
                "MinRH":"drier",
                "MaxRH":"drier",
                "RH":"drier",
                },
            # Marine
            "WaveHeight": "waves", # Used for NSH/GLF
            "seas": "combined seas",
            "waves": "wind waves",
            "inland waters": "waves",
            "chop": "bay and inland waters",
            "mixed swell": "mixed swell",
            "dominant period": "dominant period", # to be used in combined seas phrase
            "hurricane force winds to": "hurricane force winds to",
            "storm force winds to": "storm force winds to",
            "gales to": "gales to",
            "up to": "up to",
               # Fire Weather labels
            "SKY/WEATHER.........":"SKY/WEATHER.........",
            "   24 HR TREND......":"   24 HR TREND......",
            "unchanged":"unchanged",
            "missing":"missing",
            "MinT":"lows",
            "MaxT":"highs",
            "MinT_FireWx": "MIN TEMPERATURE.....",
            "MaxT_FireWx": "MAX TEMPERATURE.....",
            "MinRH_FireWx":"MIN HUMIDITY........",
            "MaxRH_FireWx":"MAX HUMIDITY........",
            "HUMIDITY RECOVERY...":"HUMIDITY RECOVERY...",
            "UPSLOPE/DOWNSLOPE...":"UPSLOPE/DOWNSLOPE...",
            "20-FOOT WINDS.......":"20-FOOT WINDS.......",
            "    VALLEYS/LWR SLOPES...":"    VALLEYS/LWR SLOPES...",
            "    RIDGES/UPR SLOPES....":"    RIDGES/UPR SLOPES....",
            "WIND................":"WIND................",
            "LAL.................":"LAL.................",
            "FREE WINDS..........":"10K FT WIND.........",
            "SMOKE DISPERSAL.....":"SMOKE DISPERSAL.....",
            "TRANSPORT WINDS.....":"TRANSPORT WINDS.....",
            "MIXING HEIGHT.......":"MIXING HEIGHT.......",
            "HAINES INDEX........":"HAINES INDEX........",
            "CWR.................":"CHC OF WETTING RAIN.",
            "MARINE LAYER........":"MARINE LAYER........",
            #  Used for Headlines
            "EXPECTED" : "EXPECTED",
            "IN EFFECT" : "IN EFFECT",
            # Used for single values
            "around": "around",
            "through the day": "through the day",
            "through the night": "through the night",
            # Used for Tropical
            "iminHR":"HURRICANE CONDITIONS",
            "iminTS":"TROPICAL STORM CONDITIONS",
            "iminTSposHR":"TROPICAL STORM CONDITIONS WITH HURRICANE CONDITIONS POSSIBLE",
            "posTS":"TROPICAL STORM CONDITIONS POSSIBLE",
            "posTSbcmgposHR":"TROPICAL STORM CONDITIONS POSSIBLE WITH HURRICANE CONDITIONS ALSO POSSIBLE",
            "expTS":"TROPICAL STORM CONDITIONS EXPECTED",
            "posHR":"HURRICANE CONDITIONS POSSIBLE",
            "expHR":"HURRICANE CONDITIONS EXPECTED",
            "expTSposHR":"TROPICAL STORM CONDITIONS EXPECTED WITH HURRICANE CONDITIONS POSSIBLE",
            "posTSorHR":"TROPICAL STORM OR HURRICANE CONDITIONS POSSIBLE" ,           
            }
    
    def phrase_descriptor(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "phrase_descriptor_dict")

    def phrase_connector_dict(self, tree, node):
        # Dictionary of connecting phrases for various
        # weather element phrases
        # The value for an element may be a phrase or a method
        # If a method, it will be called with arguments:
        #   tree, node
        return {
            # Used for Scalar and Wx elements
            "then": {
                 "Sky": " then becoming ",
                 "otherwise": " then ",
                 },
            # Used for Scalar and Vector elements
            "increasing to": {
                  "Sky": " then becoming ",
                  "WaveHeight": " building to ",
                  "otherwise":" increasing to ",
                  },
            "decreasing to": {
                  "Sky": " then becoming ",
                  "WaveHeight": " subsiding to ",
                  "otherwise":" decreasing to ",
                  },            
            "becoming": " becoming ",
            "shifting to the": " shifting to the ",
            
            # Used for Marine Vector weather elements 
            "rising to": " rising to ",
            "easing to": " easing to ",
            "backing": " backing ",
            "veering":" veering ",
            "becoming onshore": " becoming onshore",
            }            
    def phrase_connector(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "phrase_connector_dict")
    
    def useThenConnector_dict(self, tree, node):
        # Conjunctive "THEN" to make 3+ subPhrase phrases
        # flow better. e.g.
        # "N WIND 10 TO 20 KT RISING TO 30 KT EARLY IN THE
        # AFTERNOON...THEN RISING TO GALES TO 40 KT LATE
        # IN THE AFTERNOON." 
        return {
            "otherwise": 1,
            "Wx": 0,
            }
    def useThenConnector(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "useThenConnector_dict")

    def thenConnector_dict(self, tree, node):
        # Conjunctive "THEN" to make 3+ subPhrase phrases
        # flow better. e.g.
        # "N WIND 10 TO 20 KT RISING TO 30 KT EARLY IN THE
        # AFTERNOON...THEN RISING TO GALES TO 40 KT LATE
        # IN THE AFTERNOON." 
        return {
            "otherwise": "...then",
            "Sky": "",
            }
    def thenConnector(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "thenConnector_dict")

    def value_connector_dict(self, tree, node):
        # Dictionary of connectors for ranges of values
        #  E.g.  25 TO 35 mph
        # The value for an element may be a phrase or a method
        # If a method, it will be called with arguments:
        #   tree, node
        return {
            "Period": " to ",
            "Period2": " to ",
            "TransWind": " to ",  
            "Wind20ft": " to ",  
            "FreeWind": " to ",  
            "Swell": " to ",  
            "Swell2": " to ", 
            "Wind": " to ",
            "MaxT": "-",
            "MinT": "-",
            "MaxRH": "-",
            "MinRH": "-",
            "RH": "-",
            "T": "-",
            "WaveHeight" : " to ",   
            "WindWaveHgt" : " to ",  
            "WindChill": " to ",
            "HeatIndex": " to ",
            }
    def value_connector(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value,
                                      "value_connector_dict")
    
    def units_descriptor_dict(self, tree, node):
        # Dictionary of descriptors for various units
        return {
            "units": {
                "ft": "feet",
                "F":"",
                "C":"degrees",
                "K":"kelvins",
                "%":" percent",
                "in":"inches",
                "kts":"knots",
                "s":"seconds",
                "hrs":"hours",
                "m/s":"meters/second",
                "mph":"mph",
                "m":"meters",
                "m^2/s":"meters^2/second",
                "kt-ft":"knots-feet",
                "mm":"millimeters",
                "degrees": "degrees",
                "percent": "percent",
                },
            "unit": {
                "ft":"foot",
                "F":"",
                "C":"degree",
                "K":"kelvin",
                "%":" percent",
                "in":"inch",
                "kts":"knot",
                "s":"second",
                "hrs":"hour",
                "m/s":"meter/second",
                "mph":"mph",
                "m":"meter",
                "m^2/s":"meter^2/second",
                "kt-ft":"knot-foot",
                "mm":"millimeter",
                "degree": "degree",
                "percent": "percent",
                },
            }        

    def units_descriptor(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "units_descriptor_dict")

    def element_inUnits_dict(self, tree, node):
        # Dictionary of descriptors for various units
        return {
            "QPF":"in",
            "Wind":"kts",
            "Wind20ft":"kts",
            "Wx":"wx",
            "SnowAmt":"in",
            "IceAccum":"in",
            "StormTotalSnow": "in",
            "PoP":"%",
            "Sky":"%",
            "FzLevel":"ft",
            "SnowLevel":"ft",
            "RH":"%",
            "HeatIndex":"F",
            "WindChill":"F",
            "T":"F",
            "Td":"F",
            "MinT":"F",
            "MaxT":"F",
            "WindWaveHgt":"ft",
            "WaveHeight":"ft",
            "Swell":"ft",
            "Swell2":"ft",
            "Period":"s",
            "Period2":"s",
            "WindGust":"kts",
            "LAL":"cat",
            "CWR":"%",
            "Haines":"cat",
            "MixHgt":"ft",
            "FreeWind":"kts",
            "TransWind":"kts",
            "Stability":"cat",
            "HrsOfSun":"hrs",
            "MarineLayer":"ft",
            "InvBurnOffTemp":"F",
            "VentRate":"kt-ft",
            "MinRH": "%",
            "MaxRH":"%",
            "RH":"%",
            "WetFlag":"y/n",
            "Ttrend":"F",
            "RHtrend":"%",
            }        

    def element_inUnits(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "element_inUnits_dict")

    def element_outUnits_dict(self, tree, node):
        # Dictionary of descriptors for various units
        return {
            "QPF":"in",
            "Wind":"kts",
            "Wind20ft":"kts",
            "Wx":"wx",
            "SnowAmt":"in",
            "IceAccum":"in",
            "StormTotalSnow": "in",
            "PoP":"%",
            "Sky":"%",
            "FzLevel":"ft",
            "SnowLevel":"ft",
            "RH":"%",
            "HeatIndex":"F",
            "WindChill":"F",
            "T":"F",
            "Td":"F",
            "MinT":"F",
            "MaxT":"F",
            "WindWaveHgt":"ft",
            "WaveHeight":"ft",
            "Swell":"ft",
            "Swell2":"ft",
            "Period":"s",
            "Period2":"s",
            "WindGust":"kts",
            "LAL":"cat",
            "CWR":"%",
            "Haines":"cat",
            "MixHgt":"ft",
            "FreeWind":"kts",
            "TransWind":"kts",
            "Stability":"cat",
            "HrsOfSun":"hrs",
            "MarineLayer":"ft",
            "InvBurnOffTemp":"F",
            "VentRate":"kt-ft",
            "MinRH": "%",
            "MaxRH":"%",
            "RH":"%",
            "WetFlag":"y/n",
            "Ttrend":"F",
            "RHtrend":"%",
            "Visibility": "SM",  # statute miles -- Can also be NM (nautical miles) 
            }        

    def element_outUnits(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "element_outUnits_dict")

    ############################################      
    ### GLOBAL THRESHOLDS AND VARIABLES
    def maxReported_threshold_dict(self, tree, node):
        # Winds will not be reported above this value:
        # For example, if set to 30, all winds above 30 will
        # be reported as:
        #   "Winds up to 30 knots."
        return {
            "Wind": 200, # knots or mph depending on product
            "Wind20ft": 200, # knots or mph depending on product
            }        
    def maxReported_threshold(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "maxReported_threshold_dict")

    def range_nlValue_dict(self, tree, node):
        # If the range of values less than this amount, return as a single value 
        #    
        return {
            "MaxT": 5,
            "MinT": 5,
            "MinRH": 5,
            "MaxRH": 5,
            "RH": 5,
            }
    def range_nlValue(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "range_nlValue_dict")
        
    def maximum_range_nlValue_dict(self, tree, node):
        # Maximum range to be reported within a phrase
        #   e.g. 5 to 10 mph
        # Units depend on the product
        return {}
##        return {
##            "Wind": 10,  
##            "Wind20ft": 10,  
##            "TransWind": 10,  
##            "FreeWind": 10,  
##            "Swell": 5,  
##            "Swell2": 5,
##            "WaveHeight": 2,
##            "WindWaveHgt": 2,
##            "MaxT": 50,
##            "MinT": 50,
##            "HeatIndex": 50,
##            "WindChill": 50,
##            "MaxRH": 50,
##            "MinRH": 50,
##            "RH": 50,
##            }    
    def maximum_range_nlValue(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "maximum_range_nlValue_dict")

    def minimum_range_nlValue_dict(self, tree, node):
        # This threshold is the "smallest" min/max difference allowed between values reported.
        # For example, if threshold is set to 5 for "MaxT", and the min value is 45
        # and the max value is 46, the range will be adjusted to at least a 5 degree
        # range e.g. 43-48.  These are the values that are then submitted for phrasing
        # such as:
        #   HIGHS IN THE MID 40S 
        return {
##            "Wind": 0,
##            "Wind20ft": 0,
##            "TransWind": 0,  
##            "FreeWind": 0,  
##            "Swell": 0,  
##            "Swell2": 0,
##            "WaveHeight": 0,
##            "WindWaveHgt": 0,
##            "MaxT": 0,
##            "MinT": 0,
##            "MaxRH": 0,
##            "MinRH": 0,
##            "RH": 0,
            "WindChill": 10,
            "HeatIndex": 5,
            }
    def minimum_range_nlValue(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "minimum_range_nlValue_dict")

    def range_bias_nlValue_dict(self, tree, node):
        # "Min", "Average", "Max"
        #  Should the range be taken from the "min" "average" or "max"
        #  value of the current range?
        return {"otherwise": "Average"}
    def range_bias_nlValue(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "range_bias_nlValue_dict")

    def maximum_range_bias_nlValue_dict(self, tree, node):
        # "Min", "Average", "Max"
        #  Should the maximum_range be taken from the "min" "average" or "max"
        #  value of the current range?
        return {
            "Wind": "Max",
            "otherwise": "Average",
            }
    def maximum_range_bias_nlValue(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "maximum_range_bias_nlValue_dict")
    
    def minimum_range_bias_nlValue_dict(self, tree, node):
        # "Min", "Average", "Max"
        #  Should the minimum_range be taken from the "min" "average" or "max"
        #  value of the current range?
        return {
            "Wind": "Max",
            "MaxRH": "Max",
            "MinRH": "Max",
            "RH": "Max",
            "WindChill": "Min",
            "HeatIndex": "Max",
            "otherwise": "Average",
            }
    def minimum_range_bias_nlValue(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "minimum_range_bias_nlValue_dict")

    def highValue_threshold_dict(self, tree, node):
        # If high wind conditions report both "becoming" and
        #  "increasing/decreasing"
        # SOUTHEAST WINDS AROUND 70 MPH BECOMING SOUTH
        #   AND INCREASING TO AROUND 105 MPH
        # Otherwise, it will just be reported with "increasing"
        #   or "becoming":
        # SOUTHEAST WINDS AROUND 20 MPH BECOMING SOUTH
        #   AROUND 15 MPH                
        return {
            "Wind": 45, # knots or mph depending on product
            "Wind20ft": 45, # knots or mph depending on product
            "TransWind": 45,   
            "FreeWind": 45,
            "Swell": 20,   
            "Swell2": 20,
            "otherwise": 45,
            }        
    def highValue_threshold(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "highValue_threshold_dict")

    #####
    #  NULL value phrases
    def first_null_phrase_dict(self, tree, node):
        # Phrase to use if values THROUGHOUT the period or
        # in the first period are Null (i.e. below threshold OR NoWx)
        # E.g.  LIGHT WINDS.    or    LIGHT WINDS BECOMING N 5 MPH.
        return {
            "Wind": "light winds", 
            "Wind20ft": "light winds", 
            "TransWind": "light winds", 
            "FreeWind": "light winds", 
            "Swell": "light swells", 
            "Swell2": "",
            "Wx": "",
            "WindGust": "",
            "WaveHeight": "waves 2 feet or less",
            "WindWaveHgt": "waves 2 feet or less",
            "CWR": "",
            }
    def first_null_phrase(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "first_null_phrase_dict")

    def null_phrase_dict(self, tree, node):
        # Phrase to use for null values in subPhrases other than the first
        # Can be an empty string
        #  E.g.  "NORTH WINDS 20 to 25 KNOTS BECOMING LIGHT"
        return {
            "Wind": "light",   
            "Wind20ft": "light",   
            "TransWind": "light",   
            "FreeWind": "light", 
            "Swell": "light",
            "Swell2": "",
            "Wx":"",
            "WindGust": "",
            "WaveHeight": "2 feet or less",
            "WindWaveHgt": "2 feet or less",
            "CWR": "",
            }    
    def null_phrase(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "null_phrase_dict")

    ############################################      
    ### GLOBAL SCALAR THRESHOLDS AND VARIABLES
    def scalar_difference_nlValue_dict(self, tree, node):
        # Scalar difference.  If the difference between scalar values
        # for 2 sub-periods is greater than or equal to this value,
        # the different values will be noted in the phrase.
        return {
            "WindGust": 10, # knots or mph depending on product
            "Period": 5, # seconds
            # Set PoP to high value so we will never have sub-phrases
            "PoP": 200, # percentage
            "WaveHeight": 5, # feet
            "WindWaveHgt": 5, # feet
            "LAL": 1,
            "MaxT": 10,
            "MinT": 10,
            "MaxRH": 10,
            "MinRH": 10,
            "RH": 10,
            "Ttrend": 5,
            "RHtrend": 10,
            "MixHgt": 2000,
            "VentRate": 50000,
            "CWR": 10,
            "WindChill": 10,
            "HeatIndex": 20,
            "SnowLevel": 1000,
            "otherwise": 10,
            }        
    def scalar_difference_nlValue(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "scalar_difference_nlValue_dict")
    
    ### PoP
    def pop_lower_threshold(self, tree, node):
        # Pop values below this amount will not be reported
        return self.popThreshold(tree, node, self._hoursSChcEnds, 15.0, 25.0)

    def popThreshold(self, tree, node, hoursSChcEnds, schcVal, defaultVal):
        hours = self.hoursPastProductStart(tree, node)
        dur = node.getTimeRange().duration()/3600
        val = defaultVal
        if dur <= 12:
            if hours < hoursSChcEnds:
                val = 15.0
        elif hours+dur <= hoursSChcEnds:
            val = 15.0
        return val     

    def pop_upper_threshold(self, tree, node):
        # Pop values above this amount will not be reported
        return 100

    def lowPop_flag(self, tree, node, threshold):
        # Checks pop against the given threshold and returns 1 if
        # it is below that threshold.
        # If there is no data, return None
        # Otherwise, return 0
        popStats = self.matchToWx(tree, node, "PoP")
        if popStats is None:
            return None
        if popStats < threshold:
            return 1
        else:
            return 0

    ############################################      
    ### GLOBAL VECTOR THRESHOLDS AND VARIABLES
    def null_nlValue_dict(self, tree, node):
        # Threshold below which values are considered "null" and
        # reported using te null_phrase (see above)
        return {
            "otherwise": 0,
            "Wind": 5,  
            "Wind20ft": 5,  
            "TransWind": 0,  
            "FreeWind": 0, 
            "Swell": 5,  # ft
            "Swell2": 5,  # ft
            "Visibility": .3, # in nautical miles. Report if less than this value.
            "WaveHeight": 3,  # ft
            "WindWaveHgt": 3,  # ft
            "WindGust": 20,
            "LAL": 0,
            "MaxT": 0,
            "MinT": 0,
            "MaxRH": 0,
            "MinRH": 0,
            "RH": 0,
            "Ttrend": 0,
            "RHtrend": 0,
            "MixHgt": 0,
            "VentRate": 0,
            "CWR": 0,
            "Ttrend": 0,
            "RHtrend": 0,
            "PoP": 0,
            "WindChill": -100,
            "HeatIndex": 108,
            "SnowLevel": 0,
            }
    def null_nlValue(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "null_nlValue_dict")

    def null_alwaysDistinct_flag_dict(self, tree, node):
        # If 1, null values will always be considered distinct from non-null values
        # when combining subphrases.  Thus, with a null value of 5,
        # you may end up with phrases such as:
        #   Winds less than 5 mph becoming east 5 mph.
        # If set to 0, the determination will be made based on the scalar or
        # vector difference as with non-null values.
        #   (See scalar_difference_nlValue_dict and vector_mag_difference_nlValue_dict)
        return {
            "otherwise": 1,
            #"Wind": 0,  
            }
    def null_alwaysDistinct_flag(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "null_alwaysDistinct_flag_dict")
    
    def increment_nlValue_dict(self, tree, node):
        # Increment for rounding values
        # Units depend on the product
        return {
            "otherwise": 1,
            "CWR": 10,
            "PoP": 10, 
            "TransWind": 5,  
            "FreeWind": 5,
            "MarineLayer": 100,
            "MixHgt": 100,
            "SnowLevel": 100,
            "SnowAmt": .1,
            "StormTotalSnow": .1,
            "IceAccum": .1,
            "Visibility": .25, 
            "Wind": 5,
            "Wind20ft": 5,
            "WindGust": 5,
            "VentRate": 100,
            "QPF": .01,
            }
    def increment_nlValue(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "increment_nlValue_dict")

    def rounding_method_dict(self, tree, node):
        # Special rounding methods
        #
        # Uncomment Wind method for marine products
        return {
            #"Wind": self.marineRounding,
            }
    def rounding_method(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value,"rounding_method_dict", execMethods=0)

    def adjust_method_dict(self, tree, node):
        # Special conversion methods
        #
        # Used in Fire Weather product 
        return {
            #"Wind": self._adjustWind,
            }
    def adjust_method(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "adjust_method_dict", execMethods=0)

    def marineRounding(self, value, mode, increment, maxFlag):
        # Rounding for marine winds
        mode = "Nearest"
        if maxFlag:
            if value > 30 and value < 34:
                mode = "RoundDown" 
            elif value > 45 and value < 48:
                mode = "RoundDown"
            else:
                mode = "Nearest"
        return self.round(value, mode, increment)
        
    def vector_mag_difference_nlValue_dict(self, tree, node):
        # Replaces WIND_THRESHOLD
        # Magnitude difference.  If the difference between magnitudes
        # for sub-ranges is greater than or equal to this value,
        # the different magnitudes will be noted in the phrase.
        # Units can vary depending on the element and product
        return  {
            "Wind": 10,   
            "Wind20ft": 10,   
            "TransWind": 10,   
            "FreeWind": 10,   
            "Swell": 5,  # ft
            "Swell2": 5,  # ft
            "otherwise": 5,
            }
    def vector_mag_difference_nlValue(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "vector_mag_difference_nlValue_dict")

    def vector_dir_difference_dict(self, tree, node):
        # Replaces WIND_DIR_DIFFERENCE
        # Direction difference.  If the difference between directions
        # for sub-ranges is greater than or equal to this value,
        # the different directions will be noted in the phrase.
        # Units are degrees
        return {
            "Wind": 60, # degrees
            "Wind20ft": 60, # degrees
            "TransWind": 60,  # mph
            "FreeWind": 60,  # mph
            "Swell":60, # degrees
            "Swell2":60, # degrees
            "otherwise": 60,
            }    
    def vector_dir_difference(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "vector_dir_difference_nlValue_dict")
    
    def vector_dir_difference_nlValue_dict(self, tree, node):
      dict =  self.vector_dir_difference_dict(tree, node)
      # If you want to use a nlValue for the Wind direction
      # override this method and uncomment the entry below for Wind.
      # Adjust the values given to the desired values.
      # The Wind direction threshold will then be chosen according to
      # the MINIMUM nlValue determined from the Wind MAGNITUDE.
      # See 'checkVectorDifference' (PhraseBuilder) for usage.
      #
      #    dict["Wind"] = {
      #       (0, 10): 60,
      #       (10, 200): 40,
      #       'default': 40,
      #      }
      #  When the wind is between 0 and 10 kt,
      #     report when the wind direction change is 60 degrees or more
      #
      return dict

    def vector_dir_difference_nlValue(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "vector_dir_difference_nlValue_dict")

    def embedded_vector_descriptor_flag_dict(self, tree, node):
        # If set to 1, the descriptor will be embedded in the phrase:
        #  such as "NORTH WINDS 20 to 25 KNOTS BECOMING LIGHT AND VARIABLE"
        #  instead of "WINDS NORTH 20 to 25 KNOTS BECOMING LIGHT AND VARIABLE"
        return {
            "Wind": 1,   
            "Wind20ft": 1,   
            "TransWind": 1,  
            "FreeWind": 1,   
            "Swell": 1,
            "Swell2": 1,
            }    
    def embedded_vector_descriptor_flag(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value,
                                      "embedded_vector_descriptor_flag_dict")

    # Sea Breeze information
    def seaBreeze_thresholds(self, tree, node):
        # Offshore and Onshore wind directions to identify sea breezes
        #
        # Offshore wind direction boundaries in first sub-period
        offshore1 = 30
        offshore2 = 150
        # Onshore wind direction boundaries in second sub-period
        onshore1 = 330
        onshore2 = 210
        return offshore1, offshore2, onshore1, onshore2
    
    def seaBreeze_areaLabels(self, tree, node):
        # Return the Offshore and Onshore local effect area labels
        # These areas must be set up as "intersectAreas" in the
        # Component definition for the seaBreezeFlag to function
        return "OffShoreArea", "OnShoreArea"

    # TRENDS
    def trend_threshold_dict(self, tree, node):
        return {
           "MinT": 1,
           "MaxT": 1,
           "MinRH": 1,
           "MaxRH": 1,
           "RH": 1,
        }
    def trend_threshold(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "trend_threshold_dict")

    # COMBINING
    def combine_singleValues_flag_dict(self, tree, node):
        # Dictionary of weather elements to combine using single values
        # rather than ranges.  If you are using single value statistics
        # for a weather element, you will want to set this flag to 1.
        # If there is no entry for an element, min/max combining will
        # be done.
        # The value for an element may be a phrase or a method
        # If a method, it will be called with arguments:
        #   tree, node
        return {}
    def combine_singleValues_flag(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value,
                                      "combine_singleValues_flag_dict")

    # UNTIL PHRASING
    def untilPhrasing_flag_dict(self, tree, node):
        # If set to 1, "until" time descriptor phrasing will be used.
        # E.g. "NORTH WINDS 20 MPH UNTIL 10 AM...THEN 35 MPH"
        return {
            "otherwise": 0,
            #"Wind" : 1,
            }        
    def untilPhrasing_flag(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "untilPhrasing_flag_dict")

    def onTheFly_untilPhrasing_flag_dict(self, tree, node):
        # If set to 1, "until" time descriptor phrasing will be used ONLY if
        # the time range for a sub-phrase does not end on a 3-hour boundary.
        return {
            "otherwise": 0,
            #"Wind" : 1,
            }        
    def onTheFly_untilPhrasing_flag(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "onTheFly_untilPhrasing_flag_dict")

    def untilPhrasing_format_dict(self, tree, node):
        # Format for "until" time descriptors.
        # If "military": UNTIL 1000
        # If "standard": UNTIL 10 AM
        return {
            "otherwise": "military",
            #"Wind": "standard",
            }
    def untilPhrasing_format(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "untilPhrasing_format_dict")

    ### REMOVING EMPTY INDENTED PHRASES
    def removeEmptyPhrase(self, tree, node):
        # If an indented phrase is empty, do not include the entry for it
        return 0    

    ### PERIOD COMBINING

    def periodCombining_elementList(self, tree, node):
        # Weather Elements to determine whether to combine periods
        return ["Sky", "Wind", "Wx", "PoP", "MaxT", "MinT"]
        # Marine
        #return ["WaveHeight", "Wind", "Wx"]
        # Diurnal Sky Wx pattern
        #return ["DiurnalSkyWx"]
    
    def periodCombining_startHour(self, tree, node):
        # Hour after which periods may be combined
        return 36

    # Automatic Collapsing of Sub-phrases for Combined periods
    def collapseSubPhrase_hours_dict(self, tree, node):
        # If the period is longer than these hours, subphrases will automatically
        # be collapsed.
        return {
            "otherwise": 23,
            #"Wx": 12,
            }
    def collapseSubPhrase_hours(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "collapseSubPhrase_hours_dict")
    
    def mergeMethod_dict(self, tree, node):
        # Designates the mergeMethod to use when sub-phrases are automatically collapsed.
        return {
            "otherwise": "MinMax",
            "PoP": "Max",
            "Wx": "Average",  
            }
    def mergeMethod(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "mergeMethod_dict")

    def nextDay24HourLabel_flag(self, tree, node):
        # Return 1 to have the TimeDescriptor module label 24 hour periods starting
        # after 1600 as the next day.
        # This is needed for the Fire Weather Extended product,
        # but not for other products when period combining.
        # NOTE: If you are doing period combining, you should
        # set this flag to zero and set the "splitDay24HourLabel_flag" to 1.
        return 0
    
    def splitDay24HourLabel_flag(self, tree, node):
        # Return 0 to have the TimeDescriptor module label 24 hour periods
        # with simply the weekday name (e.g. SATURDAY)
        # instead of including the day and night periods
        # (e.g. SATURDAY AND SATURDAY NIGHT)
        # NOTE: If you set this flag to 1, make sure the "nextDay24HourLabel_flag"
        # is set to zero.
        # NOTE: This applied only to periods that are exactly 24-hours in length.
        # Periods longer than that will always be split into day and night labels
        # (e.g. SUNDAY THROUGH MONDAY NIGHT)
        return 0

    def mostImportant_dict(self, tree, node):
        # Can be set to "Min" or "Max". Works for Scalar or Vector elements.
        # Only the most important sub-phrase will be reported
        # using the "mostImportant_descriptor" (see below).
        # For example, instead of:
        #   WIND CHILL READINGS 5 BELOW TO 15 BELOW ZERO IN THE EARLY
        #   MORNING INCREASING TO ZERO TO 10 BELOW ZERO IN THE AFTERNOON
        # we will report
        #   LOWEST WIND CHILL READINGS 5 BELOW TO 15 BELOW IN THE EARLY
        #   MORNING
        return {
            "otherwise": None,
            "WindChill": "Min",
            }
    def mostImportant(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "mostImportant_dict")
    
    def mostImportant_descriptor_dict(self, tree, node):
        return {
            "otherwise": None,
            "WindChill": "lowest wind chill readings",
            }
    def mostImportant_descriptor(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "mostImportant_descriptor_dict")

    def matchToWxInfo_dict(self, tree, node):
        # The system will automatically match the following elements to
        # the highest ranking weather subkey coverage.
        # Each entry is a tuple of (increment, algorithm, noPrecipValue) where
        
        #  increment: This is the increment from the low "bin" value
        #    to be added.  For example, PoP has a bin of 55-65, so
        #    its increment is 5 to end up with values as multiples of 10.
        
        #  algorithm: Can be
        #    Max:  The MAXIMUM value that falls within the coverage range
        #          for the highest ranking subkey will be chosen.
        #    Mode: The MOST FREQUENT (over space and time) value that
        #          falls within the coverage range for the highest ranking
        #          subkey will be chosen.
        #    MaxMode: This is the MAXIMUM value over time of the MOST
        #         FREQUENT values over area for each of the grids in the timeRange.
        #         In other words, for each grid, we find the Mode i.e. MOST FREQUENT
        #         value that falls within the coverage range for the highest
        #         ranking subkey.  Then we find the MAXIMUM of these values
        #         over the grids again falling within the coverage values.
        #         NOTE:: If you use MaxMode, you will want to make sure the
        #         PoP "binnedPercent" resolution is high e.g. [3] so that
        #         low PoP's covering extended time periods do not wipe out
        #         higher PoP's covering shorter time periods.
        #    AnalysisMethod: This will simply use whatever analysis method
        #         is specified as the first entry in the product component
        #         for the element. For example, if you have
        #
        #         ("PoP", self.stdDevMaxAvg, [3]),
        #         ("PoP", self.binnedPercent, [3]),
        #
        #         the "stdDevMaxAvg" method will be used.
        #
        #  noPrecipValue: The value that should be returned if there is
        #         no precipitating weather.  Can be:
        #     None
        #     an actual data value
        #     "Max": The maximum value found that has a greater > 0% occurrence.
        #     "AnalysisMethod": As above, will return the result of the product
        #         component analysis method e.g. stdDevMaxAvg or maximum.
        #
        #  percentThreshold: (optional) Percent of areal coverage a value must have in
        #     order to be considered for the element value returned. 
        #     Default is 0.
        #
        #  wxTypes filter: (optional) Match only to weather keys of these wxTypes e.g.
        #     match LAL only to "T"
        #
        #  EXAMPLE 1:  Suppose we have:

        #       Wx  Hours 1-12:  Chc R  (coverage range is 30-60)
        #       PoP Hours 1-3:   40% (over 70% of area), 50% (over 30% of area)
        #           Hours 4-12:  30

        # For the 12-hour PoP,
        #    If set to Max, we will get PoP:      50
        #    If set to Mode, we will get PoP:     30
        #    If set to MaxMode, we will get PoP:  40
        
        # For the Hours 1-3 PoP:
        #    If set to Max, we will get PoP:      50
        #    If set to Mode, we will get PoP:     40
        #    If set to MaxMode, we will get PoP:  40
      
        #  NOTE: IF you add a new element to this list, you MUST include
        #  a coverage table named "coverage<elementName>_value".  Follow
        #  the example for "coveragePoP_value" in CommonUtils.  You can
        #  then access the element value by calling "matchToWx" (WxPhrases).
        #
        return {
            "PoP": (5, "Max", None, 0),
            # If there's no precipitating weather, return LAL 1
            "LAL": (0, "Max", 1, 0, ["T"]),
            }
    def matchToWxInfo(self, tree, node, key, value):
        return self.access_dictionary(tree, node, key, value, "matchToWxInfo_dict")


    def alphabetizeHeaders(self):
        # Set to 1 if you want the zones and cities of area headers to be alphabetized
        return 0


##    LOCAL EFFECTS FOR HEADLINES can be handled using the hazard_hook method which 
##    returns a string to be inserted in the headline (just before the ...) for clarification e.g.
##         "in the mountains", "for lower elevations"

##      ...WINTER STORM WARNING IN EFFECT UNTIL 1 AM EST TUESDAY IN THE MOUNTAINS...
    
##    NOTE:  There is a limited set of words that must begin your returned string e.g.
##       IN, ABOVE, BELOW, NEAR, FOR, AROUND, DUE, ALONG
    
##    1. Override the "Hazards" product component (DiscretePhrases) to sample the Hazards grid
##      (independent of normal Hazards Table processing) and set up "intersectAreas"
##       and/or "additionalAreas". 
##       For example:

##    def Hazards(self): 
##        return {
##            "type": "component",
##            "lineLength": 66,
##            "methodList": [
##                          self.assembleChildWords,   
##                          self.wordWrap,          
##                          ],
##            "analysisList":[
##                ("Hazards", self.dominantDiscreteValue),
##                ],

##            "phraseList":[
##                       self.hazards_phrase,
##                       ],
##            "autoSentence": 0,
##            "intersectAreas": [
##                ("Hazards", ["AboveElev", "BelowElev"]),
##                ],
##        }
    
##    2. Override the "hazard_hook" method and check for local effects. Arguments are:
##        hazardPhen:  hazard phenomenon e.g. WS
##        hazardSig:   hazard significance e.g. W  (watch, warning, advisory)
##        hazardAct:   hazard action code e.g. "NEW", "EXA", "EXB", "EXT", "UPG", "CAN", "CON", "EXP"
##        hazardStart and hazardEnd: start and end times for the hazard
    
##    For example:
        
##    def hazard_hook(self, tree, node, hazardPhen, hazardSig, hazardAct,
##                    hazardStart, hazardEnd):
##        hazTR = self.makeTimeRange(hazardStart, hazardEnd)
##        if hazardSig != "":
##             phenSig = hazardPhen + "." + hazardSig
##        else:
##             phenSig = hazardPhen

##        # Check for hazards AboveElev and BelowElev
##        aboveName = self.getIntersectName(node.getAreaLabel(), "AboveElev")
##        aboveHazards = tree.stats.get("Hazards", hazTR, aboveName)
##        #print "Above", aboveHazards
##        foundAbove = 0
##        if aboveHazards is not None:
##            for hazards, tr in aboveHazards:
##                if phenSig in hazards:
##                    foundAbove = 1
##                    break
##        belowName = self.getIntersectName(node.getAreaLabel(), "BelowElev")
##        belowHazards = tree.stats.get("Hazards", hazTR, belowName)
##        #print "Below", belowHazards
##        foundBelow = 0
##        if belowHazards is not None:
##            for hazards, tr in belowHazards:
##                if phenSig in hazards:
##                    foundBelow = 1
##                    break

##        # Determine if local effect wording is appropriate
##        if foundAbove and not foundBelow:
##            return "in the mountains"
##        if foundBelow and not foundAbove:
##            return "for lower elevations"
##        return ""
    
    def hazard_hook(self, tree, node, hazardPhen, hazardSig, hazardAct, hazardStart, hazardEnd):
        return ""


    ########### LOCAL EFFECTS ######################
    #### Component-Level Local Effect thresholds
    def repeatingEmbedded_localEffect_threshold(self, tree, component):
        # Number of embedded local effect phrases allowed in a component
        # before they are gathered together into a conjunctive local
        # effect clause.  For example, with the threshold set to 2:
        #
        # Instead of:
        #     Cloudy windward and partly cloudy leeward.
        #     Rain likely windward and scattered showers leeward.
        #     Chance of precipitation 50 percent windward and 30
        #     percent leeward.
        #
        # We will produce:
        #     Windward...Cloudy...Rain likely...Chance of precipitation 50 percent.
        #     Leeward...Partly cloudy...Scattered showers...Chance of precipitation
        #     30 percent.
        #
        # NOTE:  If we have even one conjunctive local effect, however, all will be left
        #     conjunctive.  For example, instead of:
        #
        #     Cloudy windward and partly cloudy leeward.
        #     Windward...Rain likely in the morning.
        #     Leeward...Scattered showers in the afternoon.
        #
        #  We will produce:
        #     Windward...Cloudy...Rain likely in the morning.
        #     Leeward...Partly cloudy...Scattered showers in the afternoon.
        #
        return 2

    def repeatingPhrase_localEffect_threshold(self, tree, component):
        # NOT YET IMPLEMENTED
        #
        # Number of repeating local effect phrases allowed inside a
        # set of conjunctive local effects for each of the
        # repeatingPhrase_categories (see below).
        #
        # For example, with the default of 1 and the categories below,
        #
        # Instead of:
        #    Chance of thunderstorms in the morning.
        #    Windward...Cloudy...Rain likely...Chance of precipitation 70 percent.
        #    Leeward...Partly cloudy...Scattered showers...Chance of precipitation 30
        #    percent. Highs in the 40s.  Winds 20 mph.
        #
        # We will produce:
        #    Windward...Cloudy....Rain likely...Chance of thunderstorms in the morning...
        #    Chance of precipitation 70 percent.
        #    Leeward...Partly cloudy...Scattered showers...Chance of thunderstorms in
        #    the morning...Chance of precipitation 30 percent. Highs in the 40s.
        #    Winds 20 mph.
        #
        # Note that if we had:
        #    Windward...Cloudy....Rain likely...Chance of precipitation 70 percent.
        #    Leeward...Partly cloudy...Scattered showers......Chance of precipitation
        #    30 percent. Highs in the 40s. Winds 20 mph.
        #
        # The phrasing would remain unchanged since there are 2 phrases (Temps and Winds)
        # in the "ALL OTHER PHRASES" category that would have to be repeated within the
        # conjunctive local effects.
        return 1
 
    def repeatingPhrase_localEffect_categories(self, tree, component):
        return [
            ["skyPopWx_phrase", "sky_phrase", "weather_phrase", "popMax_phrase"],
            ["ALL OTHER PHRASES"],
            ]

    def lePhraseNameGroups(self, tree, component):
        # Groups of phrase names that can be combined into embedded local effect phrases.
        # If the phrase is not listed here, it is assumed that only phrases with the
        # same name can be combined with it into an embedded local effect phrase.
        # For example:
        #   With the group:  ("skyPopWx_phrase", "weather_phrase"), we will allow:
        #
        #   A 20 percent chance of rain windward and areas of fog leeward.
        #
        #   Since "skyPopWx_phrase" and "wind_phrase" do not appear as group, we will
        #   not allow:
        #
        #    A 20 percent chance of rain windward and north winds 20 mph leeward.
        #
        #
        return [
            ("skyPopWx_phrase", "weather_phrase", "sky_phrase", "popMax_phrase",
             "fireSky_phrase")
            ]
