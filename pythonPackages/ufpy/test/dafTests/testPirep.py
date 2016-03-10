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
from ufpy.dataaccess import DataAccessLayer as DAL

import baseDafTestCase
import dafTestsArgsUtil
import sys
import unittest

class PirepTestCase(baseDafTestCase.DafTestCase):
    """
    Tests that pirep data can be retrieved through the DAF, simply ensuring
    that no unexpected exceptions are thrown while retrieving it and that the
    returned data is not None.
    """

    datatype = "pirep"

    station = "OMA"

    @classmethod
    def setUpClass(cls):
        print("STARTING PIREP TESTS\n\n")

    def testParameters(self):
        req = DAL.newDataRequest(self.datatype)

        self.runParametersTest(req)

    def testLocations(self):
        req = DAL.newDataRequest(self.datatype)

        self.runLocationsTest(req)

    def testTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames(self.station)

        self.runTimesTest(req)

    def testGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames(self.station)
        req.setParameters("temperature", "windSpeed", "hazardType", "turbType")

        print("Testing getGeometryData()")

        geomData = DAL.getGeometryData(req)
        self.assertIsNotNone(geomData)
        print("Number of geometry records: " + str(len(geomData)))
        print("Sample geometry data:")
        for record in geomData[:self.sampleDataLimit]:
            print("level=", record.getLevel(), end="")
            # One dimensional parameters are reported on the 0.0UNKNOWN level.
            # 2D parameters are reported on MB levels from pressure.
            if record.getLevel() == "0.0UNKNOWN":
                print(" temperature=" + record.getString("temperature") + record.getUnit("temperature"), end="")
                print(" windSpeed=" + record.getString("windSpeed") + record.getUnit("windSpeed"), end="")
            else:
                print(" hazardType=" + record.getString("hazardType"), end="")
                print(" turbType=" + record.getString("turbType"), end="")
            print(" geometry=", record.getGeometry())

        print("getGeometryData() complete\n")

    @classmethod
    def tearDownClass(cls):
        print("PIREP TESTS COMPLETE\n\n\n")

def getArgs():
    parser = dafTestsArgsUtil.getParser()
    parser.add_argument("-s", action="store", dest="station", default=PirepTestCase.station,
                        help="stationId",
                        metavar="station")
    return parser.parse_args()

if __name__ == '__main__':
    args = getArgs()
    dafTestsArgsUtil.handleArgs(args)
    PirepTestCase.station = args.station
    unittest.main(argv=sys.argv[:1])
