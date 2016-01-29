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

class ModelSoundingTestCase(baseDafTestCase.DafTestCase):
    """
    Tests that modelsounding data can be retrieved through the DAF, simply
    ensuring that no unexpected exceptions are thrown while retrieving it and
    that the returned data is not None.
    """

    datatype = "modelsounding"

    @classmethod
    def setUpClass(cls):
        print("STARTING MODELSOUNDING TESTS\n\n")

    def testParameters(self):
        req = DAL.newDataRequest(self.datatype)

        self.runParametersTest(req)

    def testLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("reportType", "ETA")

        self.runLocationsTest(req)

    def testTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("reportType", "ETA")
        req.setLocationNames("KOMA")

        self.runTimesTest(req)

    def testGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("reportType", "ETA")
        req.setLocationNames("KOMA")
        req.setParameters("temperature", "pressure", "specHum", "sfcPress", "temp2", "q2")

        print("Testing getGeometryData()")

        geomData = DAL.getGeometryData(req)
        print("Number of geometry records: " + str(len(geomData)))
        print("Sample geometry data:")
        for record in geomData[:self.sampleDataLimit]:
            print("level=" + record.getLevel(), end="")
            # One dimensional parameters are reported on the 0.0UNKNOWN level.
            # 2D parameters are reported on MB levels from pressure.
            if record.getLevel() == "0.0UNKNOWN":
                print(" sfcPress=" + record.getString("sfcPress") + record.getUnit("sfcPress"), end="")
                print(" temp2=" + record.getString("temp2") + record.getUnit("temp2"), end="")
                print(" q2=" + record.getString("q2") + record.getUnit("q2"), end="")

            else:
                print(" pressure=" + record.getString("pressure") + record.getUnit("pressure"), end="")
                print(" temperature=" + record.getString("temperature") + record.getUnit("temperature"), end="")
                print(" specHum=" + record.getString("specHum") + record.getUnit("specHum"), end="")
            print(" geometry=" + str(record.getGeometry()))

        print("getGeometryData() complete\n\n")

    @classmethod
    def tearDownClass(cls):
        print("MODELSOUNDING TESTS COMPLETE\n\n\n")

if __name__ == '__main__':
    dafTestsArgsUtil.parseAndHandleArgs()
    unittest.main(argv=sys.argv[:1])
