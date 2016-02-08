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

class RadarSpatialTestCase(baseDafTestCase.DafTestCase):
    """
    Tests that radar_spatial data can be retrieved through the DAF, simply
    ensuring that no unexpected exceptions are thrown while retrieving it and
    that the returned data is not None.
    """

    datatype = "radar_spatial"

    bbox = ["-96", "41", "-97", "42"]
        """
        Default request area (box around KOAX)
        """

    locations = ["TORD", "TMDW"]

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
        print("STARTING RADAR_SPATIAL TESTS\n\n")

    def testLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(self.getReqEnvelope())

        self.runLocationsTest(req)

    def testParameters(self):
        req = DAL.newDataRequest(self.datatype)

        self.runParametersTest(req)

    def testIdentifiers(self):
        self.runIdentifiersTest(self.datatype)

    def testGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames(*self.locations)
        req.setParameters("wfo_id", "name", "elevmeter")

        self.runGeometryDataTest(req)

    @classmethod
    def tearDownClass(cls):
        print("RADAR_SPATIAL TESTS COMPLETE\n\n\n")

def getArgs():
    parser = dafTestsArgsUtil.getParser()
    parser.add_argument("-p", action="store", dest="bbox", nargs=4, default=RadarSpatialTestCase.bbox,
                        help="Request area for location names query",
                        metavar="point")
    parser.add_argument("-r", dest="locations", nargs="+", default=RadarSpatialTestCase.locations,
                        help="IDs of radar stations to retrieve geometry data for.",
                        metavar="locations")
    return parser.parse_args()


if __name__ == '__main__':
    args = getArgs()
    dafTestsArgsUtil.handleArgs(args)
    RadarSpatialTestCase.bbox = args.bbox
    RadarSpatialTestCase.locations = args.locations
    unittest.main(argv=sys.argv[:1])
