"""########################################################################
# Experimental TAF  -  TAF_XXX_Overrides.TextUtility
#
#-------------------------------------------------------------------------
# Description:
#  This file is used for WFO specific overrides of the Experimental TAF
#  formatter.
#-------------------------------------------------------------------------
# Copying:
#  This software is in the public domain, furnished "as is", without technical
#  support, and with no warranty, express or implied, as to its usefulness for
#  any purpose.
#-------------------------------------------------------------------------
# Version: 2017.07.25-0  (07/25/2017)
#-------------------------------------------------------------------------
# Authors:  GSD Digital Aviation Services Group
#
# Support Email: nws.digital.aviation.services@noaa.gov
#-------------------------------------------------------------------------
# Customization Points:
#
# REQUIRED CONFIGURATION ITEMS
#
#  Review the significance rating rules in the SignificanceRatingRules class in
#  TAF.TextProduct to ensure are they are appropriate for your airport(s)
#  and place any needed modifications in here. They are used by some shortening
#  algorithms when determining what and how to shorten the TAF. They are also
#  used for TEMPO/PROB30 groups when merging groups and deciding which group(s)
#  to keep. For more information about how the significance ratings are used
#  for shortening, see the comments in _orderedShorteningAlgorithms in the
#  ShorteningAlgorithms class in TAF.TextProduct.
########################################################################"""

import string, time, re, os, types, copy
import TextRules, SampleAnalysis
import TAF

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum contents of this file are the following class definition
# and the __init__ method with only "pass" line in it.

class TAF_<site>_Overrides:
    def __init__(self):
        pass

# End MAKE NO CHANGES HERE
#**********************************************************************
    # Make sure to indent methods inside the class statement.
    #----- WFO <site> TAF Overrides -----

    # It is helpful to put a debug statement at the beginning of each
    # method to help with trouble-shooting.
    #def _method(self):
        #self.debug_print("Debug: _method in TAF_<site>_Overrides")


    # Place any TextProduct overrides here
    

    class GenericGroup(TAF.TextProduct.GenericGroup):
        def __init__(self, textProduct, statList, sharedDict, airportDict):
            TAF.TextProduct.GenericGroup.__init__(self,
                                                        textProduct,
                                                        statList,
                                                        sharedDict,
                                                        airportDict)

        # Place any GenericGroup overrides here
         

    class FmGroup(TAF.TextProduct.FmGroup):
        def __init__(self, textProduct, statList, sharedDict, airportDict,
                     label, period):
            TAF.TextProduct.FmGroup.__init__(self,
                                                   textProduct,
                                                   statList,
                                                   sharedDict,
                                                   airportDict,
                                                   label,
                                                   period)

        # Place any FmGroup overrides here
         

    class ConditionalGroup(TAF.TextProduct.ConditionalGroup):
        def __init__(self, textProduct, statList, sharedDict, airportDict,
                     hourOffset):
            TAF.TextProduct.ConditionalGroup.__init__(self,
                                                            textProduct,
                                                            statList,
                                                            sharedDict,
                                                            airportDict,
                                                            hourOffset)

        # Place any ConditionalGroup overrides here
         

    # Holds information extracted from a weather ugly string
    class WeatherUglyStringInfo(TAF.TextProduct.WeatherUglyStringInfo):
        """
        An example weather ugly string looks like: "Sct:T:+:1/4SM:SmA,HvyRn"
        which would indicate heavy scattered thunderstorms (with attributes of
        small hail and heavy rain) that cause a reduced visibility of 1/4
        statute miles.
 
        It's important to note that the type and attributes use GFE codes
        initially but are converted to TAF codes.
        """

        def __init__(self, textProduct, weatherUglyString, numericalVisibility,
                     groupType):
            TAF.TextProduct.WeatherUglyStringInfo.__init__(self,
                                                                 textProduct,
                                                                 weatherUglyString,
                                                                 numericalVisibility,
                                                                 groupType)

        # Place any WeatherUglyStringInfo overrides here
         

    class GroupWeatherInfo(TAF.TextProduct.GroupWeatherInfo):
        def __init__(self, textProduct, groupType, numericalVisibility):
            TAF.TextProduct.GroupWeatherInfo.__init__(self,
                                                            textProduct,
                                                            groupType,
                                                            numericalVisibility)

        # Place any GroupWeatherInfo overrides here
         

    class SignificanceRatingRules(TAF.TextProduct.SignificanceRatingRules):
        def __init__(self, textProduct, group):
            TAF.TextProduct.SignificanceRatingRules.__init__(self,
                                                                   textProduct,
                                                                   group)

        # Place any overrides to the significance rule methods here
         

    class ShorteningAlgorithms(TAF.TextProduct.ShorteningAlgorithms):
        def __init__(self, textProduct, airportIcaoId):
            TAF.TextProduct.ShorteningAlgorithms.__init__(self,
                                                                textProduct,
                                                                airportIcaoId)

        # Place any overrides to the shortening algorithms here
         
