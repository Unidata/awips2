##
##

##
# This is a base file that is not intended to be overridden.
##

########################################################################
# Hazard_WCN_Local.py
#
##
##########################################################################
import Hazard_WCN_<MultiPil>
import string, time, re, os, types, copy, sets

class TextProduct(Hazard_WCN_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_WCN_<MultiPil>.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "Hazard_WCN_<MultiPil> (Convective Watch)"

    #Definition["easPhrase"] = ""      # Optional EAS phrase to be include in product header        
    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)

    #Definition["statePartMode"] = "byState"   #'byState' or 'byPart'

    def __init__(self):
        Hazard_WCN_<MultiPil>.TextProduct.__init__(self)


