##
#
# LocalEffect_Overides.py: GFE TextUtility that provides a different way to produce local effects
#     in text products. This code will determine the most "positive" and most "negative" local
#     effects from the "prevailing" condition across an area. Multiple local effects can be reported
#     in the case of a tie.
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer    Description
# ------------- -------- ---------   --------------------------------------------
# Mar 15, 2020  DCS21339 M. Belk     Initial addition of NFTF LocalEffect_Overides module
#                                    to baseline
#
##

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##
"""
This module provides a different way to produce local effects in text products. Rather than
reporting just the first local effect found, this code will determine the most "positive" and
most "negative" local effects from the "prevailing" condition across an area. Multiple local
effects can be reported in the case of a tie for the extreme values.
"""

# ---------------------------------------------------------------------
from __future__ import print_function
import numbers

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum contents of this file are the above Definition = {} line
# plus following class definition and the __init__ method with only
# the "pass" line in it.


class LocalEffect_Overrides:
    def __init__(self):
        pass

# End MAKE NO CHANGES HERE
#**********************************************************************

    # It is helpful to put a debug statement at the beginning of each
    # method to help with troubleshooting.
    # def _method(self):
    #     self.debug_print("Debug: _method in LocalEffect_Overrides")

    #---------------------------------------------------------------------------
    #  Define methods to define local-effect edit areas
    #---------------------------------------------------------------------------

    def _sampleEffectList(self):
        """LocalEffect_Overrides addition of _sampleEffectList.

        Added to specify all possible local-effect edit areas via a list.
        This list is passed to the sampler at the same time.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _sampleEffectList")

        # return ["BOS_Metro", "BOS_NWdistSuburbs", "BOS_NorthShore",
        #         "PVD_Metro", "PVD_NWSuburbs", "Radiators"]

        return []


    def _localEffectDescriptionDict(self):
        """LocalEffect_Overrides addition of _localEffectDescriptionDict.

        Added to specify names for all possible local-effect areas. Keys are
        edit area names defined in the _sampleEffectList. Values are the
        description which should appear in the forecast text.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _localEffectDescriptionDict")

        # curDict = {
        #     "BOS_Metro": "in the Boston metro area",
        #     "BOS_NWdistSuburbs": "across the suburbs north and west of Boston",
        #     "BOS_NorthShore": "across the North Shore",
        #     "PVD_Metro": "in the Providence metro area",
        #     "PVD_NWSuburbs": "across northwest RI and far northeast CT",
        #     "Radiators": "in the normally colder locations",
        # }

        # Define names for each of these local effect areas
        curDict = {}

        # Return completed dictionary
        return curDict


    def _localEffectsFilterByBaseAreaDict(self):
        """LocalEffect_Overrides addition of _localEffectsFilterByBaseAreaDict.

        Added to limit local-effects to be searched for a base edit area.
        Keys are the base edit area identifiers with values being a list of
        local-effect edit area names. Only the local-effect edit areas so
        listed can be reported for that base edit area. If a specific list
        is not defined for a base edit area, all possbile sampled local-
        effects will be searched instead.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _localEffectsFilterByBaseAreaDict")

        # # Blue Hill, MA
        # curDict["BOS"] = ["BOS_Metro", "BOS_NWdistSuburbs", "BOS_NorthShore", "Radiators"]
        #
        # # Johnston, RI
        # curDict["PVD"] = ["PVD_Metro", "PVD_NWSuburbs", "Radiators"]
        #
        # # Cape Cod
        # curDict["MAZ022"] = ["Outer_Cape"]

        # Now make lists of edit areas to use for each transmitter
        curDict = {}

        # Return completed dictionary
        return curDict


    def _localEffectsFilterByFieldDict(self):
        """LocalEffect_Overrides addition of _localEffectFilterByFieldDict.

        Added to limit local-effects to be searched by forecast field.
        Keys are edit area names and the value is a list of fields permitted
        to use this edit area. By default, all edit areas can be used by all
        forecast fields.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _localEffectsFilterByFieldDict")

        # Radiators - only makes sense to use on radiational cooling nights
        # curDict["Radiators"] = ["MinT"]

        # Now make lists of edit areas to use for each forecast element
        curDict = {}

        # Return completed dictionary
        return curDict


    def _localEffectConflictDict(self):
        """LocalEffect_Overrides addition of _localEffectConflictDict.

        Added to define local-effects which should not be permitted to be
        reported simultaneously. For example, if reporting a local-effect at
        elevations above 1000 ft, it would not make much sense to also report
        a local-effect for elevations above 500 ft. Keys are local-effect
        edit area names and the value is a list of local-effect edit area
        names to be removed if also present.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _localEffectConflictDict")

        # Topo local-effects
        # curDict["Topo_1000_plus"] = ["Topo_500_plus"]

        # Now make lists of edit areas to use for each forecast element
        curDict = {}

        # Return completed dictionary
        return curDict


    def _intersectEffectList(self):
        """LocalEffect_Overrides addition of _intersectEffectList.

        Added to specify all local-effect edit areas which should be
        intersected with the primary edit area, possibly reducing the spatial
        coverage of the local-effect area. In general, this should
        match the list used in the _sampleEffectList method.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _intersectEffectList")

        # return ["BOS_Metro", "BOS_NWdistSuburbs", "BOS_NorthShore", "Radiators"]

        return []


    #---------------------------------------------------------------------------
    #  Define method to define local-effect thresholds by field
    #---------------------------------------------------------------------------

    def _localEffectThresholdDict(self, key=None):
        """LocalEffect_Overrides addition of _localEffectThresholdDict.

        Added to define thresholds for local-effects by field name.  This
        threshold can be either a numeric value or a method name.  If the
        requested field key does not have a defined threshold, a value is
        returned to make it nearly impossible to get a local-effect for
        that field.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _localEffectThresholdDict")

        curDict = {
            "MaxT": 3.5,
            "MinT": 3.5,
            "SnowAmt": 2.0,
            "StormTotalSnow": 2.0,
            "PoP": 40.0,
            "Sky": 25.0,
            "Wind": 15.0,
            "Wx": self._wxLocalEffects,
        }

        if key in curDict:
            return curDict[key]
        else:
            return 1000  # make it nearly impossible to succeed


    #---------------------------------------------------------------------------
    #  Define method to limit local-effects by primary sample area
    #---------------------------------------------------------------------------

    def _getBaseAreaFilteredEffectList(self, tree):
        """LocalEffect_Overrides addition of _getBaseAreaFilteredEffectList.

        Added to limit local-effects which can be used with a specific base
        edit area. If no list is defined for a particular base edit area,
        then all local-effects sampled will be evaluated. The edit area name
        of the primary edit area should be a key of the
        _localEffectsByBaseAreaDict dictionary.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _getBaseAreaFilteredEffectList")

        #-----------------------------------------------------------------------
        # Force use of all sampled local-effect areas by default

        baseArea = None

        #-----------------------------------------------------------------------
        # Here is an example of specifying multiple base edit areas, taken
        # from the WFO BOX ZFP SAF.

        # # If this is the Blue Hill, MA transmitter
        # if self.currentAreaContains(tree, ["SAFZFP_BOS"]):
        #     baseArea = "BOS"
        #
        # # If this is the Johnston, RI transmitter
        # elif self.currentAreaContains(tree, ["SAFZFP_PVD"]):
        #     baseArea = "PVD"
        #
        # # If this is the Somers, CT transmitter
        # elif self.currentAreaContains(tree, ["SAFZFP_BDL"]):
        #     baseArea = "BDL"
        #
        # # If this is the Paxton, MA transmitter
        # elif self.currentAreaContains(tree, ["SAFZFP_ORH"]):
        #     baseArea = "ORH"
        #
        # # If this is the Peterborough, NH transmitter
        # elif self.currentAreaContains(tree, ["SAFZFP_PACK"]):
        #     baseArea = "PAC"
        #
        # # If this is the Camp Edwards, MA transmitter
        # elif self.currentAreaContains(tree, ["SAFZFP_HYA"]):
        #     baseArea = "HYA"

        #-----------------------------------------------------------------------
        # If we have a defined base edit area, and a defined local-effect list
        # for it

        if baseArea and baseArea in self._localEffectsFilterByBaseAreaDict():

            # Limit the list of local-effects to examine
            return self._localEffectsFilterByBaseAreaDict()[baseArea]

        # Otherwise, use the entire sampled local-effect list
        else:

            return self._sampleEffectList()


    #---------------------------------------------------------------------------
    #  Define method to limit local-effects by field
    #---------------------------------------------------------------------------

    def _getFieldFilteredEffectList(self, effectList=[], fieldName=None):
        """LocalEffect_Overrides addition of _getFieldFilteredEffectList.

        Added to limit local-effects by forecast field.  This method will
        remove local-effect areas from further consideration if they are not
        permitted for specific forecast fields.  The details are handled in
        the _localEffectsFilterByFieldDict method.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _getFieldFilteredEffectList")

        # Get ready to make a new list
        newList = []

        # Examine each local-effect edit area we have so far
        for effect in effectList:

            # Display some debug info if flag is set
            self.debug_print("current local effect = '%s'" % (effect))

            # If this edit area may be restricted
            if effect in self._localEffectsFilterByFieldDict():

                # Display some debug info if flag is set
                self.debug_print(
                    "\tCan only be reported for -> %s"
                    % (repr(self._localEffectsFilterByFieldDict()[effect])),
                    1,
                )

                # If this forecast element should not use this area
                if fieldName not in self._localEffectsFilterByFieldDict()[effect]:

                    # Display a debug message if flag is set
                    self.debug_print("%s removed for -> %s" % (effect, fieldName), 1)

                    # Move on to next local effect area
                    continue

            # Add this local-effect are to the new list
            newList.append(effect)

        # Return the filtered list
        return newList


    #---------------------------------------------------------------------------
    #  Define method to retrieve local-effect statistics for any field
    #---------------------------------------------------------------------------

    def _getFieldValue(self, tree, fieldName, timeRange, areaLabel):
        """LocalEffect_Overrides addition of _getFieldValue.

        Added to retrieve the field value for the specified area name. This
        method assumes the statistics in the ER ZFP and CWF are being used.
        If this is not the case, this method may need to be modified.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _getFieldValue")

        # Do not report valid data by default
        stats = None
        value = None

        # If this is a MaxT or MinT local-effect
        if fieldName in ["MaxT", "MinT"]:

            # Get stats for comparison
            stats = tree.stats.get(fieldName, timeRange, areaLabel)

        # Otherwise, if this is snow accumulation local-effect
        elif fieldName in ["SnowAmt", "StormTotalSnow"]:

            # Get stats for comparison
            stats = tree.stats.get(fieldName, timeRange, areaLabel)

            # print("&" * 80)
            # print(fieldName, type(stats), stats)

            # If the stats are in a tuple, compute the average accumulation
            if stats and isinstance(stats[0][0], tuple):
                (minSnow, maxSnow) = stats[0][0]
                stats = (minSnow + maxSnow) / 2.0

        # Otherwise if this a Wind, Sky or Wx local-effect
        elif fieldName in ["Wind", "Sky", "Wx"]:

            # Get stats for comparison
            stats = tree.stats.get(fieldName, timeRange, areaLabel, mergeMethod="Average")

        # Otherwise if this a PoP local-effect
        elif fieldName in ["PoP"]:

            # Get stats for comparison
            stats = tree.stats.get(
                fieldName, timeRange, areaLabel, statLabel="stdDevMaxAvg", mergeMethod="Max"
            )

        # If this is a wave height local-effect
        elif fieldName in ["WaveHeight"]:

            # Get stats for comparison
            stats = tree.stats.get(
                fieldName, timeRange, areaLabel, statLabel="stdDevMinMax", mergeMethod="Max"
            )

        # if self._debug:
        #     print("    %s stats = " % (fieldName), stats)

        # If base stats are missing, no point in continuing
        if not stats:
            return None

        # Only keep the wind speed of the wind stats
        if fieldName == "Wind":
            print(fieldName, type(stats), stats)
            value = float(stats[0])

        # Set aside the average value of the Sky, max value of PoP or
        # WaveHeight, or the average snow accumulation
        elif fieldName in ["Sky", "PoP", "WaveHeight", "SnowAmt", "StormTotalSnow"]:
            # print("*" * 80)
            # print(fieldName, type(stats), stats)

            if not stats or stats in ["null"]:
                value = 0.0
            else:
                value = float(stats)

        # Set aside the Wx subkeys only, we don"t need the rankings
        elif fieldName in ["Wx"]:
            value = self.getSubkeys(stats)

        # Otherwise, get the value for this element
        else:
            (value, tr) = stats[0]
            value = float(value)

        # if self._debug:
        #     print("    %s value = " % (fieldName), value)

        # Return the base stats we found
        return value


    #---------------------------------------------------------------------------
    #  Define method to find most significant local-effcts regardless of field
    #---------------------------------------------------------------------------

    def _findMostSignificantLE(self, tree, node, curArea, leList):
        """LocalEffect_Overrides addition of _findMostSignificantLE.

        Added to search the specified local-effect areas, and report only
        those areas with the most significant deviations from the primary
        edit area. Unlike the baseline which returns the first valid local-
        effect found, it is possible to get more than one area returned if
        there are equal deviations of the same sign, or significant
        deviations of opposite signs.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _findMostSignificantLE", 1)

        # Define some variables to hold most extreme values
        mostPosExtremeList = []
        mostPosExtreme = None
        mostNegExtremeList = []
        mostNegExtreme = None

        # Define a list to hold the final local-effect list
        finalList = []

        # Get name of current element and time range of this node
        elementName = node.get("elementName")
        timeRange = node.getTimeRange()

        # Get threshold for this element name
        threshold = self._localEffectThresholdDict(elementName)

        # If this a "Sky" node - set the threshold for the skyPopWx_phrase
        if elementName == "Sky":
            self._skyLocalEffectThreshold = threshold

        # If this a "PoP" node - get the threshold for reportable PoPs
        if elementName == "PoP":
            popThresh = self.pop_lower_threshold(tree, node)

        # print("\n\n" + "_" * 80)
        # print(timeRange, elementName)

        # Get the base value for this element
        baseValue = self._getFieldValue(tree, elementName, timeRange, node.getAreaLabel())

        # Display the base value for all comparisons
        self.debug_print("%s baseValue = '%s'" % (node.getAreaLabel(), baseValue), 1)

        # If we don't have a base value for comparison
        if not baseValue:

            # Don't report any local effects
            return []

        # Display current local effect list - if debug flag is set
        self.debug_print("#" * 80, 1)
        self.debug_print("%s" % (repr(leList)), 1)

        # For each local effect in the list
        for leName in leList:

            # print("*" * 80)
            # print("'%s'  &  '%s'" % (node.getAreaLabel(), leName))

            # Get the sample area name of this local-effect
            if leName in self._intersectEffectList():
                leSampleName = self.getIntersectName(node.getAreaLabel(), leName)

            # Otherwise, this must be an additional area
            else:
                leSampleName = leName

            # Try to get the stats using an intersect name
            leValue = self._getFieldValue(tree, elementName, timeRange, leSampleName)

            # If the local effect value is missing
            if not leValue:

                # No point in considering this area further
                continue

            # If this element is not Wx
            if elementName not in ["Wx"]:

                # print("leValue", type(leValue), leValue)
                # print("baseValue", type(baseValue), baseValue)

                # Compute differences between this local area and the main (numbers only
                if isinstance(leValue, numbers.Number) and isinstance(baseValue, numbers.Number):
                    diff = leValue - baseValue

                # Otherwise, assume no difference
                else:
                    diff = 0.0

                absDiff = abs(diff)

                # Display local-effect difference - if debug flag is set
                self.debug_print("%s\tdiff = %s" % (leSampleName, diff), 1)

                # If we met or exceed the local-effect threshold, or this is
                # a PoP node and local-effect PoP is reportable but the base
                # PoP is not reportable, or this is a snow accumulation local-
                # effect with amounts and the base has no accumulation.
                if (
                    absDiff >= threshold
                    or (elementName == "PoP" and (leValue >= popThresh and baseValue < popThresh))
                    or (
                        elementName in ["SnowAmt", "StormTotalSnow"]
                        and (leValue > 0.0 and baseValue == 0)
                    )
                ):

                    # If the difference is positive
                    if diff > 0:

                        # If we don't have a positive extreme yet, or current
                        # extreme is even bigger
                        if not mostPosExtreme or diff > mostPosExtreme:

                            # Reset the value and list using current extreme
                            mostPosExtreme = diff
                            mostPosExtremeList = [leName]

                        # If this difference is the same as the current largest
                        elif diff == mostPosExtreme:

                            # Add it
                            mostPosExtremeList.append(leName)

                    # if difference is negative
                    elif diff < 0:

                        # If we don't have a negative extreme yet, or current
                        # extreme is even bigger
                        if not mostNegExtreme or diff < mostNegExtreme:

                            # Reset the value and list using current extreme
                            mostNegExtreme = diff
                            mostNegExtremeList = [leName]

                        # If this difference is the same as the current largest
                        elif diff == mostPosExtreme:

                            # Add it
                            mostNegExtremeList.append(leName)

            # Otherwise, handle Wx local-effects
            else:

                # Only include those areas which do not exactly match the
                # "base" Wx for starters
                if leValue != baseValue:

                    # Add the current area to the list
                    mostPosExtremeList.append(leName)

        # Assemble the local effect lists for a final QC
        effectList = mostPosExtremeList + mostNegExtremeList

        # Display the final local effect list - if debug flag is set
        self.debug_print("\n%s effectList = %s\n" % (elementName, effectList), 1)

        # For each local effect we found
        for effectName in effectList:

            # If this edit area has a defined name
            if effectName in self._localEffectDescriptionDict():

                # See if we should intersect this area or not
                if effectName in self._intersectEffectList():
                    intersect = 1
                else:
                    intersect = 0

                # Define current local effect where it intersects current area
                leArea = self.LocalEffectArea(
                    effectName,
                    self._localEffectDescriptionDict()[effectName],
                    intersectFlag=intersect,
                )

                # Add this local effect to the list
                finalList.append(leArea)

        # Determine where to include the current transmitter area in the
        # local-effects list
        finalList = self._localEffectOrder(
            elementName, curArea, baseValue, mostPosExtreme, mostNegExtreme, finalList
        )

        # Return the local-effects we found
        return finalList


    #---------------------------------------------------------------------------
    #  Define method to sort local-effects for reporting
    #---------------------------------------------------------------------------

    def _localEffectOrder(
        self, fieldName, curArea, baseValue, mostPosExtreme, mostNegExtreme, finalList
    ):
        """LocalEffect_Overrides addition of _localEffectOrder.

        Added to sort local-effects with respect to base edit area and
        forecast field within a given phrase.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _localEffectOrder")

        # If this is a PoP node
        if fieldName == "PoP":

            # Place the highest PoP local-effect area first
            if mostPosExtreme and mostPosExtreme > baseValue:
                return finalList + [curArea]
            elif mostNegExtreme and mostNegExtreme < baseValue:
                return [curArea] + finalList

        # If we made it this far, place the base edit area first
        return [curArea] + finalList


    #---------------------------------------------------------------------------
    #  Define method to remove potential local-effects conflicts
    #---------------------------------------------------------------------------

    def _removeLEConflicts(self, leList):
        """LocalEffect_Overrides addition of _removeLEConflicts.

        Added to remove potential local-effect conflicts.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _removeLEConflicts")

        # Get the dictionary of potential local-effect conflicts
        curDict = self._localEffectConflictDict()

        # See if there are any local effects we need to check for conflicts
        checkList = [i for i in leList if i in curDict]

        # If there are no items to check - keep all the local-effects we have
        if not checkList:
            return leList

        # Remove any conflicting local effects
        for effect in checkList:
            filterList = [i for i in leList if i not in curDict[effect]]

        # print("remove leList = ", leList)

        # Return the filtered local-effect list
        return filterList


    #---------------------------------------------------------------------------
    #  Define method to manage reporting of local-effcts regardless of field
    #---------------------------------------------------------------------------

    def _reportLocalEffects(self, tree, node):
        """LocalEffect_Overrides addition of _reportLocalEffects.

        Generic method used to report local-effects.  Local-effects can be
        limited by a base edit area as well as by forecast field.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _reportLocalEffects")

        # Define the primary sampling edit area
        curArea = self.LocalEffectArea("__Current__", "", intersectFlag=0)

        # Get name of current element and time range of this node
        elementName = node.get("elementName")
        timeRange = node.getTimeRange()

        # Get the local-effect threshold for this element
        threshold = self._localEffectThresholdDict(elementName)
        # print("%s threshold = %s" % (elementName, threshold))

        #------------------------------------------------------------------------
        # Limit possible local-effects by transmitter

        effectAreaList = self._getBaseAreaFilteredEffectList(tree)

        # print('-'*80)
        # print("BaseArea")
        # print(effectAreaList)

        #------------------------------------------------------------------------
        # Limit possible local-effects by element

        effectAreaList = self._getFieldFilteredEffectList(effectAreaList, elementName)

        # print('-'*80)
        # print("Element")
        # print(effectAreaList)

        #------------------------------------------------------------------------
        # Find the local effects with the most extreme deviation on either
        # side of the "dominant" forecast

        effectAreaList = self._findMostSignificantLE(tree, node, curArea, effectAreaList)

        #------------------------------------------------------------------------
        # Ensure there are no local-effect conflicts which may make for a
        # confusing forecast

        effectAreaList = self._removeLEConflicts(effectAreaList)

        # If the final local effect list is empty
        if not effectAreaList:

            # Do not report any local-effects
            return []

        # If we made is this far, return the local-effects we found
        return [self.LocalEffect(effectAreaList, threshold, ", except ")]



    ############################################################################
    #  Define method additions intended to simplify implementing local-effects
    ############################################################################

    def _wxLocalEffects(self, tree, node, localEffect, leArea1Label, leArea2Label):
        """LocalEffect_Overrides addition of _wxLocalEffects.

        Added to determine if there are any weather local-effects using the
        stand-alone weather_phrase.
        """
        self.debug_print("\tLocalEffect_Overrides addition of _wxLocalEffects")

        # Get the time range of this node
        timeRange = node.getTimeRange()

        # Get the Wx data for each local-effect area being compared
        wxStats1 = tree.stats.get("Wx", timeRange, leArea1Label, mergeMethod="Average")
        wxStats2 = tree.stats.get("Wx", timeRange, leArea2Label, mergeMethod="Average")

        # See if these Wx data are similar enough to be combined
        wxDiff = self.checkWeatherSimilarity(
            tree, node, wxStats1, wxStats2, al1=leArea1Label, al2=leArea2Label
        )

        # Display some debug information - if the debug flag is set
        self.debug_print("wxDiff = %d" % (wxDiff), 1)

        # If these Wx data are similar enough
        if not wxDiff:
            return 1  # Indicate these areas should be combined
        else:
            return 0  # Indicate these areas should NOT be combined

