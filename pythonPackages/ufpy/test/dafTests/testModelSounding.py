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
from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint

import baseDafTestCase
import unittest

#
# Test DAF support for modelsounding data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#    06/09/16        5587          bsteffen       Add getIdentifierValues tests
#    06/13/16        5574          tgurney        Add advanced query tests
#    06/30/16        5725          tgurney        Add test for NOT IN
#    11/10/16        5985          tgurney        Mark expected failures prior
#                                                 to 17.3.1
#
#


class ModelSoundingTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for modelsounding data"""

    datatype = "modelsounding"

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)

        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("reportType", "ETA")

        self.runLocationsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("reportType", "ETA")
        req.setLocationNames("KOMA")

        self.runTimesTest(req)

    @unittest.expectedFailure
    def testGetGeometryData(self):
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

    def testGetIdentifierValues(self):
        req = DAL.newDataRequest(self.datatype)
        optionalIds = set(DAL.getOptionalIdentifiers(req))
        self.runGetIdValuesTest(optionalIds)

    def testGetInvalidIdentifierValuesThrowsException(self):
        self.runInvalidIdValuesTest()

    def testGetNonexistentIdentifierValuesThrowsException(self):
        self.runNonexistentIdValuesTest()

    def _runConstraintTest(self, key, operator, value):
        req = DAL.newDataRequest(self.datatype)
        constraint = RequestConstraint.new(operator, value)
        req.setParameters('dataURI')
        req.setLocationNames('KOMA', 'KORD', 'KOFK', 'KLNK')
        req.addIdentifier(key, constraint)
        return self.runGeometryDataTest(req)

    # We can filter on reportType but it is not possible to retrieve the value
    # of reportType directly. We can look inside the dataURI instead.
    #
    # For cases like '<=' and '>' the best we can do is send the request and
    # see if it throws back an exception.
    #
    # Can also eyeball the number of returned records.

    @unittest.expectedFailure
    def testGetDataWithEqualsString(self):
        geometryData = self._runConstraintTest('reportType', '=', 'ETA')
        for record in geometryData:
            self.assertIn('/ETA/', record.getString('dataURI'))

    @unittest.expectedFailure
    def testGetDataWithEqualsUnicode(self):
        geometryData = self._runConstraintTest('reportType', '=', u'ETA')
        for record in geometryData:
            self.assertIn('/ETA/', record.getString('dataURI'))

    # No numeric tests since no numeric identifiers are available.

    @unittest.expectedFailure
    def testGetDataWithEqualsNone(self):
        geometryData = self._runConstraintTest('reportType', '=', None)

    @unittest.expectedFailure
    def testGetDataWithNotEquals(self):
        geometryData = self._runConstraintTest('reportType', '!=', 'ETA')
        for record in geometryData:
            self.assertNotIn('/ETA/', record.getString('dataURI'))

    @unittest.expectedFailure
    def testGetDataWithNotEqualsNone(self):
        geometryData = self._runConstraintTest('reportType', '!=', None)

    @unittest.expectedFailure
    def testGetDataWithGreaterThan(self):
        geometryData = self._runConstraintTest('reportType', '>', 'ETA')

    @unittest.expectedFailure
    def testGetDataWithLessThan(self):
        geometryData = self._runConstraintTest('reportType', '<', 'ETA')

    @unittest.expectedFailure
    def testGetDataWithGreaterThanEquals(self):
        geometryData = self._runConstraintTest('reportType', '>=', 'ETA')

    @unittest.expectedFailure
    def testGetDataWithLessThanEquals(self):
        geometryData = self._runConstraintTest('reportType', '<=', 'ETA')

    @unittest.expectedFailure
    def testGetDataWithInTuple(self):
        collection = ('ETA', 'GFS')
        geometryData = self._runConstraintTest('reportType', 'in', collection)
        for record in geometryData:
            dataURI = record.getString('dataURI')
            self.assertTrue('/ETA/' in dataURI or '/GFS/' in dataURI)

    @unittest.expectedFailure
    def testGetDataWithInList(self):
        collection = ['ETA', 'GFS']
        geometryData = self._runConstraintTest('reportType', 'in', collection)
        for record in geometryData:
            dataURI = record.getString('dataURI')
            self.assertTrue('/ETA/' in dataURI or '/GFS/' in dataURI)

    @unittest.expectedFailure
    def testGetDataWithInGenerator(self):
        collection = ('ETA', 'GFS')
        generator = (item for item in collection)
        geometryData = self._runConstraintTest('reportType', 'in', generator)
        for record in geometryData:
            dataURI = record.getString('dataURI')
            self.assertTrue('/ETA/' in dataURI or '/GFS/' in dataURI)

    def testGetDataWithNotInList(self):
        collection = ['ETA', 'GFS']
        geometryData = self._runConstraintTest('reportType', 'not in', collection)
        for record in geometryData:
            dataURI = record.getString('dataURI')
            self.assertTrue('/ETA/' not in dataURI and '/GFS/' not in dataURI)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('reportType', 'junk', 'ETA')

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('reportType', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('reportType', 'in', [])

    def testGetDataWithNestedInConstraintThrowsException(self):
        collection = ('ETA', 'GFS', ())
        with self.assertRaises(TypeError):
            self._runConstraintTest('reportType', 'in', collection)
