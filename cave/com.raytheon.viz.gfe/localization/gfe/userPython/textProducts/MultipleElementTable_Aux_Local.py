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
import copy

class TextProduct(MultipleElementTable.TextProduct):
    Definition = copy.deepcopy(MultipleElementTable.TextProduct.Definition)
    Definition["displayName"] = "None"
    Definition["regionList"] = []

    def __init__(self):
        MultipleElementTable.TextProduct.__init__(self)

    def _getVariables(self, argDict):
        # Determine whether Morning or Afternoon product type
        varDict = argDict["varDict"]
        for (key, value) in varDict.items():
            if type(key) is tuple:
                label, variable = key
                setattr(self, f"_{variable}", value)

        # Set up any other product-specific variables from the Definition
        self._definition = argDict["forecastDef"]
        for (key, value) in self._definition.items():
            setattr(self, f"_{key}", value)

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
        header = "\n" + areaLabel.ljust(10)
        return fcst + header
    
    def _postProcessProduct(self, fcst, argDict):
        return fcst
