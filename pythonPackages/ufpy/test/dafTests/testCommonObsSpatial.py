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
from shapely.geometry import box
from ufpy.dataaccess import DataAccessLayer as DAL

import baseDafTestCase
import dafTestsArgsUtil
import sys
import unittest

class CommonObsSpatialTestCase(baseDafTestCase.DafTestCase):
    """
    Tests that common_obs_spatial data can be retrieved through the DAF, simply
    ensuring that no unexpected exceptions are thrown while retrieving it and
    that the returned data is not None.
    """

    datatype = "common_obs_spatial"

    bbox = ["-96", "41", "-97", "42"]
    """
    Default request area (box around KOAX)
    """

    envelope = None

    @classmethod
    def getReqEnvelope(cls):
        if not cls.envelope:
            x1, y1, x2, y2 = map(float, cls.bbox)
            minX = min(x1, x2)
            maxX = max(x1, x2)
            minY = min(y1, y2)
            maxY = max(y1, y2)
            cls.envelope = box(minX, minY, maxX, maxY)
        return cls.envelope

    @classmethod
    def setUpClass(cls):
        print("STARTING COMMON_OBS_SPATIAL TESTS\n\n")

    def testParameters(self):
        req = DAL.newDataRequest(self.datatype)

        self.runParametersTest(req)

    def testLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("country", ["US", "CN"])

        self.runLocationsTest(req)

    def testIdentifiers(self):
        self.runIdentifiersTest(self.datatype)

    def testGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(self.getReqEnvelope())
        req.setParameters("name", "stationid")

        self.runGeometryDataTest(req)

    @classmethod
    def tearDownClass(cls):
        print("COMMON_OBS_SPATIAL TESTS COMPLETE\n\n\n")

def getArgs(): 
    parser = dafTestsArgsUtil.getParser()
    parser.add_argument("-p", action="store", dest="bbox", nargs=4, default=CommonObsSpatialTestCase.bbox,
                        help="Request area",
                        metavar="point")
    return parser.parse_args()

if __name__ == '__main__':
    args = getArgs()
    dafTestsArgsUtil.handleArgs(args)
    CommonObsSpatialTestCase.bbox = args.bbox
    unittest.main(argv=sys.argv[:1])

