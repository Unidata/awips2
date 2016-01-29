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

import unittest

#
# Base TestCase for DAF tests. Note that this class doesn't provide any test
# methods that are automatically called when running this TestCase (methods
# named test*()). Instead, it provides default implementations of various tests
# that subclasses must explicity choose to call from their test*() methods.
#
# The tests primarily check that no unexpected exceptions are thrown when
# retrieving data for DAF requests, instead of explicity asserting against
# expected outcomes.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    
# 
#

class DafTestCase(unittest.TestCase):

    sampleDataLimit = 5
        """
        Maximum number of levels, locations, times, and geometry/grid data to
        display
        """

    numTimesToLimit = 3
        """
        When limiting geometry/grid data requests with times, only retrieve data
        for this many times
        """

    def runParametersTest(self, req):
        print("Testing getAvailableParameters()")

        params = DAL.getAvailableParameters(req)
        self.assertIsNotNone(params)
        print(params)

        print("getAvailableParameters() complete\n")

    def runIdentifiersTest(self, datatype):
        print("Testing get*Identifiers()")

        required = DAL.getRequiredIdentifiers(datatype)
        self.assertIsNotNone(required)
        print("Required identifiers:", required)

        optional = DAL.getOptionalIdentifiers(datatype)
        self.assertIsNotNone(optional)
        print("Optional identifiers:", optional)

        print("get*Identifiers() complete\n")

    def runLevelsTest(self, req):
        print("Testing getAvailableLevels()")

        levels = DAL.getAvailableLevels(req)
        self.assertIsNotNone(levels)
        print("Number of levels: " + str(len(levels)))
        strLevels = map(str, levels[:self.sampleDataLimit])
        print("Sample levels:\n" + str(strLevels))

        print("getAvailableLevels() complete\n")

    def runLocationsTest(self, req):
        print("Testing getAvailableLocationNames()")

        locs = DAL.getAvailableLocationNames(req)
        self.assertIsNotNone(locs)
        print("Number of location names: " + str(len(locs)))
        print("Sample location names:\n" + str(locs[:self.sampleDataLimit]))

        print("getAvailableLocationNames() complete\n")

    def runTimesTest(self, req):
        print("Testing getAvailableTimes()")

        times = DAL.getAvailableTimes(req)
        self.assertIsNotNone(times)
        print("Number of times: " + str(len(times)))
        strTimes = map(str, times[:self.sampleDataLimit])
        print("Sample times:\n" + str(strTimes))

        print("getAvailableTimes() complete\n")

    def runGeometryDataTest(self, req, limitTimes=False):
        """
        Test that we are able to successfully retrieve geometry data for the given
        request.

        Args:
            limitTimes: whether or not to limit the request to the most recent times
        """
        print("Testing getGeometryData()")

        times = []
        if limitTimes:
            times = DAL.getAvailableTimes(req)

        geomData = DAL.getGeometryData(req, times[:self.numTimesToLimit])
        self.assertIsNotNone(geomData)
        print("Number of geometry records: " + str(len(geomData)))
        print("Sample geometry data:")
        for record in geomData[:self.sampleDataLimit]:
            print("geometry=" + str(record.getGeometry()), end="")
            for p in req.getParameters():
                print(" " + p + "=" + record.getString(p), end="")
            print()

        print("getGeometryData() complete\n")

    def runGridDataTest(self, req, limitTimes=False, testSameShape=True):
        """
        Test that we are able to successfully retrieve grid data for the given 
        request. 
        
        Args:
            limitTimes: whether or not to limit the request to the most recent times
            testSameShape: whether or not to verify that all the retrieved data have
                            the same shape (most data don't change shape)
        """
        print("Testing getGridData()")

        times = []
        if limitTimes:
            times = DAL.getAvailableTimes(req)

        gridData = DAL.getGridData(req, times[:self.numTimesToLimit])
        self.assertIsNotNone(gridData)
        print("Number of grid records: " + str(len(gridData)))
        if len(gridData) > 0:
            print("Sample grid data shape:\n" + str(gridData[0].getRawData().shape) + "\n")
            print("Sample grid data:\n" + str(gridData[0].getRawData()) + "\n")
            print("Sample lat-lon data:\n" + str(gridData[0].getLatLonCoords()) + "\n")

            if testSameShape:
                allGridsPopulated = True
                correctGridShape = gridData[0].getLatLonCoords()[0].shape
                for record in gridData:
                    if record.getRawData() is None or record.getRawData().shape != correctGridShape:
                        allGridsPopulated = False
                        break

                print("All grid data records have correct grid data: " + str(allGridsPopulated))
                self.assertTrue(allGridsPopulated)

        print("getGridData() complete\n")
