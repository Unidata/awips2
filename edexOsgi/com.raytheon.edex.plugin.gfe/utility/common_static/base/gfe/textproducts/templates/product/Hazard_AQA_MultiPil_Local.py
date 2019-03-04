##
##

##
# This is a base file that is not intended to be overridden.
##

########################################################################
# Hazard_AQA_Local.py
#
##
##########################################################################
import Hazard_AQA_<MultiPil>
import string, time, re, os, types, copy

class TextProduct(Hazard_AQA_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_AQA_<MultiPil>.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "Hazard_AQA_<MultiPil> (Air Quality Alert)"

    #Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
    #Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    #Definition["includeOverview"] = 1   #If 1, the overview section is templated

    def __init__(self):
        Hazard_AQA_<MultiPil>.TextProduct.__init__(self)

