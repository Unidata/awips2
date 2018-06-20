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

import unittest

from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint
from ufpy.dataaccess import DataAccessLayer as DAL

import baseRadarTestCase
import params


#
# Test DAF support for radar graphics data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/25/16        2671          tgurney        Initial creation.
#    08/31/16        2671          tgurney        Add mesocyclone
#    09/08/16        2671          tgurney        Add storm track
#    09/27/16        2671          tgurney        Add hail index
#    09/30/16        2671          tgurney        Add TVS
#    12/07/16        5981          tgurney        Parameterize
#    12/19/16        5981          tgurney        Do not check data times on
#                                                 returned data
#
#
class RadarGraphicsTestCase(baseRadarTestCase.BaseRadarTestCase):
    """Test DAF support for radar data"""

    datatype = 'radar'

    def runConstraintTest(self, key, operator, value):
        req = DAL.newDataRequest(self.datatype)
        constraint = RequestConstraint.new(operator, value)
        req.addIdentifier(key, constraint)
        req.setParameters('166')
        # TODO: Cannot check datatimes on the result because the times returned
        # by getAvailableTimes have level = -1.0, while the time on the actual
        # data has the correct level set (>= 0.0).
        return self.runGeometryDataTest(req, checkDataTimes=False)

    def testGetGeometryDataMeltingLayer(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(params.ENVELOPE)
        req.setLocationNames(self.radarLoc)
        req.setParameters('166')
        self.runGeometryDataTest(req, checkDataTimes=False)

    def testGetGeometryDataMesocyclone(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(params.ENVELOPE)
        req.setLocationNames(self.radarLoc)
        req.setParameters('141')
        self.runGeometryDataTest(req, checkDataTimes=False)

    def testGetGeometryDataStormTrack(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(params.ENVELOPE)
        req.setLocationNames(self.radarLoc)
        req.setParameters('58')
        self.runGeometryDataTest(req, checkDataTimes=False)

    def testGetGeometryDataHailIndex(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(params.ENVELOPE)
        req.setLocationNames(self.radarLoc)
        req.setParameters('59')
        self.runGeometryDataTest(req, checkDataTimes=False)

    def testGetGeometryDataTVS(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(params.ENVELOPE)
        req.setLocationNames(self.radarLoc)
        req.setParameters('61')
        self.runGeometryDataTest(req, checkDataTimes=False)
