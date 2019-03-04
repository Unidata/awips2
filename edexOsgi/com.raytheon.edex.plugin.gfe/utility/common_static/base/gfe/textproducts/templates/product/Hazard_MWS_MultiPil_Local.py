##
##

##
# This is a base file that is not intended to be overridden.
##

########################################################################
# Hazard_MWS_Local.py
#
#
##########################################################################
import Hazard_MWS_<MultiPil>
import string, time, re, os, types, copy

class TextProduct(Hazard_MWS_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_MWS_<MultiPil>.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "Hazard_MWS_<MultiPil> (Marine Weather Statement)"

    #Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)

    def __init__(self):
        Hazard_MWS_<MultiPil>.TextProduct.__init__(self)


