##
##

##
# This is a base file that is not intended to be overridden.
##

########################################################################
# Hazard_FFA_Local.py
#
##
##########################################################################
import Hazard_FFA_<MultiPil>
import string, time, re, os, types, copy

class TextProduct(Hazard_FFA_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_FFA_<MultiPil>.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "Hazard_FFA_<MultiPil> (Flood Watch)"  # Flood Watch
    #Definition["areaType"] = "FIPS" # default is set to "ZONES"
    
    #Definition["easPhrase"] = ""      # Optional EAS phrase to be include in product header
    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
    #Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    #Definition["includeOverview"] = 1   #If 1, the overview section is templated
    #Definition["accurateCities"] = 0  # If 1, cities are determined from grids

    # DO NOT OVERRIDE THE FOLLOWING CODE BLOCK
    # It is necessary to properly set for zones or counties
    if Definition["areaType"] == "FIPS":
        Definition["defaultEditAreas"] = "EditAreas_FIPS_<site>_<MultiPil>"   #Where XXX = site id
        Definition["mapNameForCombinations"] = "FIPS_<site>"
    else:
        Definition["defaultEditAreas"] = "EditAreas_PublicZones_<site>_<MultiPil>"   #Where XXX = site id
        Definition["mapNameForCombinations"] = "Zones_<site>"

    def __init__(self):
        Hazard_FFA_<MultiPil>.TextProduct.__init__(self)
