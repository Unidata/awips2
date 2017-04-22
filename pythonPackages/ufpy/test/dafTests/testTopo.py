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
from awips.dataaccess import DataAccessLayer as DAL
from awips.ThriftClient import ThriftRequestException

import baseDafTestCase
import shapely.geometry
import unittest

#
# Test DAF support for topo data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#    05/26/16        5587          tgurney        Add test for
#                                                 getIdentifierValues()
#    06/01/16        5587          tgurney        Update testGetIdentifierValues
#
#


class TopoTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for topo data"""

    datatype = "topo"

    def testGetGridData(self):
        print("defaultTopo")
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("group", "/")
        req.addIdentifier("dataset", "full")
        poly = shapely.geometry.LinearRing(((-70, 40), (-71, 40), (-71, 42), (-70, 42)))
        req.setEnvelope(poly)
        gridData = DAL.getGridData(req)
        self.assertIsNotNone(gridData)
        print("Number of grid records: " + str(len(gridData)))
        print("Sample grid data shape:\n" + str(gridData[0].getRawData().shape) + "\n")
        print("Sample grid data:\n" + str(gridData[0].getRawData()) + "\n")

        for topoFile in ["gmted2010", "gtopo30"]:
            print("\n" + topoFile)
            req.addIdentifier("topoFile", topoFile)
            gridData = DAL.getGridData(req)
            self.assertIsNotNone(gridData)
            print("Number of grid records: " + str(len(gridData)))
            print("Sample grid data shape:\n" + str(gridData[0].getRawData().shape) + "\n")
            print("Sample grid data:\n" + str(gridData[0].getRawData()) + "\n")


    def testRequestingTooMuchDataThrowsResponseTooLargeException(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("group", "/")
        req.addIdentifier("dataset", "full")
        points = ((-180, 90), (180, 90), (180, -90), (-180, -90))
        poly = shapely.geometry.LinearRing(points)
        req.setEnvelope(poly)

        with self.assertRaises(ThriftRequestException) as cm:
            DAL.getGridData(req)
        self.assertIn('ResponseTooLargeException', str(cm.exception))

    def testGetIdentifierValues(self):
        req = DAL.newDataRequest(self.datatype)
        optionalIds = set(DAL.getOptionalIdentifiers(req))
        requiredIds = set(DAL.getRequiredIdentifiers(req))
        self.runGetIdValuesTest(optionalIds | requiredIds)

    def testGetInvalidIdentifierValuesThrowsException(self):
        self.runInvalidIdValuesTest()

    def testGetNonexistentIdentifierValuesThrowsException(self):
        self.runNonexistentIdValuesTest()
