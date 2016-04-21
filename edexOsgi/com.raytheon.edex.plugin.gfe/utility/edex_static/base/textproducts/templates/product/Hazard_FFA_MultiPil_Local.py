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
