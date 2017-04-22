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


