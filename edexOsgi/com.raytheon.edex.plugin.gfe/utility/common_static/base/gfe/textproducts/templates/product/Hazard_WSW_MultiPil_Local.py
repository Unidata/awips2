##
##

##
# This is a base file that is not intended to be overridden.
##

########################################################################
# Hazard_WSW_Local.py
#
#
##########################################################################
import Hazard_WSW_<MultiPil>
import string, time, re, os, types, copy

class TextProduct(Hazard_WSW_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_WSW_<MultiPil>.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "Hazard_WSW_<MultiPil> (Winter Wx Product)"

    #Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
    #Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    #Definition["includeOverview"] = 1   #If 1, the overview section is templated
    #Definition["accurateCities"] = 0  # If 1, cities are determined from grids

    def __init__(self):
        Hazard_WSW_<MultiPil>.TextProduct.__init__(self)


