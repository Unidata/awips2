##
##

##
# This is a base file that is not intended to be overridden.
##

########################################################################
# Hazard_MWW_Local.py
#
##
##########################################################################
import Hazard_MWW_<MultiPil>
import string, time, re, os, types, copy

class TextProduct(Hazard_MWW_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_MWW_<MultiPil>.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "Hazard_MWW_<MultiPil> (Marine Weather)"

    #Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
    #Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    #Definition["includeOverview"] = 1   #If 1, the overview section is templated

    def __init__(self):
        Hazard_MWW_<MultiPil>.TextProduct.__init__(self)

