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
# Hazard_CFW_Local.py
#
#
##########################################################################
import Hazard_CFW_<MultiPil>
import string, time, re, os, types, copy

class TextProduct(Hazard_CFW_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_CFW_<MultiPil>.TextProduct.Definition)

    Definition['displayName'] = None
    Definition['displayName'] = "Hazard_CFW_<MultiPil> (Coastal/LakeShore Flooding)"
    #Definition["easPhrase"] = ""      # Optional EAS phrase to be include in product header
    #Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
    #Definition["includeOverviewHeadline"] = 1   #If 1, the overview header is templated
    #Definition["includeOverview"] = 1   #If 1, the overview section is templated
    #Definition["accurateCities"] = 0  # If 1, cities are determined from grids

    def __init__(self):
        Hazard_CFW_<MultiPil>.TextProduct.__init__(self)


