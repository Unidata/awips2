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
# Hazard_WSW_Local.py
#
#
##########################################################################
import Hazard_WSW_<MultiPil>
import copy


class TextProduct(Hazard_WSW_<MultiPil>.TextProduct):
    Definition = copy.deepcopy(Hazard_WSW_<MultiPil>.TextProduct.Definition)

    Definition["displayName"] = "Hazard_WSW_<MultiPil> (Winter Wx Product)"

    # Definition["easPhrase"] = ""       # Optional EAS phrase to be include in product header
    # Definition["hazardSamplingThreshold"] = (10, None)  # (%cov, #points)
    # Definition["includeOverviewHeadline"] = 0   # If 1, the overview header is templated
    # Definition["includeOverview"] = 0   # If 1, the overview section is templated
    # Definition["accurateCities"] = 0  # 1: cities are based on grids; 0: full list is included

    def __init__(self):
        Hazard_WSW_<MultiPil>.TextProduct.__init__(self)
