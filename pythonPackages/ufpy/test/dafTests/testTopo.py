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
from ufpy.ThriftClient import ThriftRequestException

import baseDafTestCase
import dafTestsArgsUtil
import shapely.geometry
import sys
import unittest

class TopoTestCase(baseDafTestCase.DafTestCase):
    """
    Tests that topo data can be retrieved through the DAF, simply ensuring
    that no unexpected exceptions are thrown while retrieving it and that the
    returned data is not None. Also tests that requests for too much data throw
    an exception indicating that the request needs to be narrowed.
    """

    datatype = "topo"

    @classmethod
    def setUpClass(cls):
        print("STARTING TOPO TESTS\n\n")

    def testGridData(self):
        print("Testing getGridData()")

        print("defaultTopo")
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("group", "/")
        req.addIdentifier("dataset","full")
        poly = shapely.geometry.LinearRing(((-70, 40), (-71, 40), (-71, 42), (-70, 42)))
        req.setEnvelope(poly)
        gridData = DAL.getGridData(req)
        self.assertIsNotNone(gridData)
        print("Number of grid records: " + str(len(gridData)))
        print("Sample grid data shape:\n" + str(gridData[0].getRawData().shape) + "\n")
        print("Sample grid data:\n" + str(gridData[0].getRawData()) + "\n")

        for topoFile in ["srtm30", "gmted2010", "gtopo30.h5"]:
            print("\n" + topoFile)
            req.addIdentifier("topoFile", topoFile)
            gridData = DAL.getGridData(req)
            self.assertIsNotNone(gridData)
            print("Number of grid records: " + str(len(gridData)))
            print("Sample grid data shape:\n" + str(gridData[0].getRawData().shape) + "\n")
            print("Sample grid data:\n" + str(gridData[0].getRawData()) + "\n")

        print("getGridData() complete\n")

    def testTooMuchGridData(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("group", "/")
        req.addIdentifier("dataset","full")
        poly = shapely.geometry.LinearRing(((-180, 90), (180, 90), (180, -90), (-180, -90)))
        req.setEnvelope(poly)

        print("Testing request of too much grid data")

        with self.assertRaises(ThriftRequestException) as cm:
            DAL.getGridData(req)
        print("Expected exception thrown:", cm.exception)

        print("Too much grid data complete\n")

    @classmethod
    def tearDownClass(cls):
        print("TOPO TESTS COMPLETE\n\n\n")

if __name__ == '__main__':
    dafTestsArgsUtil.parseAndHandleArgs()
    unittest.main(argv=sys.argv[:1])
