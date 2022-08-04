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

import Avn
from com.raytheon.viz.aviation.monitor import CcfpData

#
# Retrieves cached ccfp data
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/15/09                      njensen        Initial Creation.
#    Feb 09, 2018    6763          njensen        Don't add blank data to empty list
#
#
#

##
# This is a base file that is not intended to be overridden.
##


def retrieve(siteID):
    vals = CcfpData.getReports(siteID)
    ret = []
    sz = vals.size()
    for i in range(sz):
        text = str(vals.get(i))
        ret.append(Avn.Bunch(text=text))
    return ret
