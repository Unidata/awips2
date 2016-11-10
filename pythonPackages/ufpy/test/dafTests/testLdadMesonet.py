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

from __future__ import print_function
from shapely.geometry import Polygon
from ufpy.dataaccess import DataAccessLayer as DAL

import baseDafTestCase
import unittest

#
# Test DAF support for ldadmesonet data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#
#


class LdadMesonetTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for ldadmesonet data"""

    datatype = "ldadmesonet"

    envelope = None

    @classmethod
    def getReqEnvelope(cls):
        # Restrict the output to only records with latitude and
        # longitude between -30 and 30.
        if not cls.envelope:
            vertices = [(-30, -30), (-30, 30), (30, 30), (30, -30)]
            polygon = Polygon(vertices)
            cls.envelope = polygon.envelope
        return cls.envelope

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(self.getReqEnvelope())
        self.runLocationsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(self.getReqEnvelope())
        self.runTimesTest(req)

    def testGetGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setParameters("highLevelCloud", "pressure")
        req.setEnvelope(self.getReqEnvelope())
        self.runGeometryDataTest(req)
