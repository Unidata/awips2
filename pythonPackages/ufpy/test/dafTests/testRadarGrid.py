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

from ufpy.dataaccess import DataAccessLayer as DAL
from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint

from . import baseRadarTestCase
from . import params
#
# Test DAF support for radar grid data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/25/16        2671          tgurney        Initial creation
#
#


class RadarTestCase(baseRadarTestCase.BaseRadarTestCase):
    """Test DAF support for radar data"""

    datatype = 'radar'

    parameterList = ['94']

    def runConstraintTest(self, key, operator, value):
        req = DAL.newDataRequest(self.datatype)
        constraint = RequestConstraint.new(operator, value)
        req.addIdentifier(key, constraint)
        req.setParameters(*self.parameterList)
        # Don't test shapes since they may differ.
        return self.runGridDataTest(req, testSameShape=False)

    def testGetGridData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(params.ENVELOPE)
        req.setLocationNames(self.radarLoc)
        req.setParameters(*self.parameterList)
        # Don't test shapes since they may differ.
        self.runGridDataTest(req, testSameShape=False)
