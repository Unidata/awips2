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
# ----------------------------------------------------------------------------
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    03/15/2020      DCS21339      NFTF           Updated for HazSimp format
#
##
# This is a base file that is not intended to be overridden.
##

########################################################################
# Hazard_MWW_Local.py
#
##
##########################################################################
import Hazard_MWW_<MultiPil>
import copy


class TextProduct(Hazard_MWW_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_MWW_<MultiPil>.TextProduct.Definition)

    Definition["displayName"] = "Hazard_MWW_<MultiPil> (Marine Weather)"

    # Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    # Definition["hazardSamplingThreshold"] = (10, None)  # (%cov, #points)

    def __init__(self):
        Hazard_MWW_<MultiPil>.TextProduct.__init__(self)
