##
##

##
# This is a base file that is not intended to be overridden.
##

########################################################################
# Hazard_NPW_Local.py
#
##
##########################################################################
import Hazard_NPW_<MultiPil>
import string, time, re, os, types, copy

class TextProduct(Hazard_NPW_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_NPW_<MultiPil>.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "Hazard_NPW_<MultiPil> (Non-Precipitation)"

    #Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
    #Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    #Definition["includeOverview"] = 1   #If 1, the overview section is templated
    #Definition["accurateCities"] = 0  # If 1, cities are determined from grids

    def __init__(self):
        Hazard_NPW_<MultiPil>.TextProduct.__init__(self)

