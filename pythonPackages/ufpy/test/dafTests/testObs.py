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

class ObsTestCase(baseDafTestCase.DafTestCase):
    """
    Tests that obs data can be retrieved through the DAF, simply ensuring
    that no unexpected exceptions are thrown while retrieving it and that the
    returned data is not None.
    """

    datatype = "obs"

    @classmethod
    def setUpClass(cls):
        print("STARTING OBS TESTS\n\n")

    def testParameters(self):
        req = DAL.newDataRequest(self.datatype)

        self.runParametersTest(req)

    def testLocations(self):
        req = DAL.newDataRequest(self.datatype)

        self.runLocationsTest(req)

    def testTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames("KOMA")

        self.runTimesTest(req)

    def testGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames("KOMA")
        req.setParameters("temperature", "seaLevelPress", "dewpoint")

        self.runGeometryDataTest(req)

    @classmethod
    def tearDownClass(cls):
        print("OBS TESTS COMPLETE\n\n\n")

if __name__ == '__main__':
    dafTestsArgsUtil.parseAndHandleArgs()
    unittest.main(argv=sys.argv[:1])
