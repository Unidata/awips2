##
##
########################################################################
# MultipleElementTable_Aux_Local
#   This routine is to be used in conjunction with the FWF and AreaFcst 
#   products
#
#   Type: smart
#   Local product:
#     MultipleElementTable_Aux_Local(type: smart)
#   To customize this product for your site:
#      Set up MultipleElementTable_Aux_Local (see template below)
#      to override variables, definitions, thresholds, and methods
##
##########################################################################

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

import MultipleElementTable
import string, time, re, os, types, copy

class TextProduct(MultipleElementTable.TextProduct):
    Definition = copy.deepcopy(MultipleElementTable.TextProduct.Definition)
    Definition["displayName"] = "None"
    Definition["regionList"] = []

    def __init__(self):
        MultipleElementTable.TextProduct.__init__(self)

    def _getVariables(self, argDict):
        # Determine whether Morning or Afternoon product type
        varDict = argDict["varDict"]
        for key in varDict.keys():
            if type(key) is types.TupleType:
                label, variable = key
                exec "self._" + variable + "= varDict[key]"

        # Set up any other product-specific variables from the Definition
        self._definition = argDict["forecastDef"]
        for key in self._definition.keys():
            exec "self._" + key + "= self._definition[key]"

        self._elementList = argDict["elementList"]
        self._singleValueFormat = argDict["singleValueFormat"]
        self._includeTitle = argDict["includeTitle"]
        self._currentRegion = None

        # The analysisList tells which weather elements and statistics
        # are desired for the product.
        self._analysisList = self._getAnalysisList()

    # Headers and Footers to override

    def _preProcessArea(self, fcst, editArea, areaLabel, argDict):
        # This is the header for an edit area
        #print "Generating Forecast for", areaLabel
        header = "\n" + string.ljust(areaLabel, 10)
        return fcst + header
    
    def _postProcessProduct(self, fcst, argDict):
        return fcst
