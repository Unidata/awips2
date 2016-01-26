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

import dafTestsUtil
import sys
import unittest

class TestLdadMesonet(unittest.TestCase):
    """
    Tests that ldadmesonet data can be retrieved through the DAF, simply
    ensuring that no unexpected exceptions are thrown while retrieving it and
    that the returned data is not None.
    """

    datatype = "ldadmesonet"

    envelope = None

    @classmethod
    def getReqEnvelope(cls):
        # Restrict the output to only records with latitude and 
        # longitude between -30 and 30.
        if not cls.envelope:
            vertices = [ (-30, -30), (-30, 30), (30, 30), (30, -30) ]
            polygon = Polygon(vertices)
            cls.envelope = polygon.envelope
        return cls.envelope

    @classmethod
    def setUpClass(cls):
        print("STARTING LDADMESONET TESTS\n\n")

    def testParameters(self):
        req = DAL.newDataRequest(self.datatype)

        dafTestsUtil.testParameters(req)

    def testLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(self.getReqEnvelope())

        dafTestsUtil.testLocations(req)

    def testTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(self.getReqEnvelope())

        dafTestsUtil.testTimes(req)

    def testGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setParameters("highLevelCloud", "pressure")
        req.setEnvelope(self.getReqEnvelope())

        dafTestsUtil.testGeometryData(req)

    @classmethod
    def tearDownClass(cls):
        print("LDADMESONET TESTS COMPLETE\n\n\n")

if __name__ == '__main__':
    dafTestsUtil.parseAndHandleArgs()
    unittest.main(argv=sys.argv[:1])
