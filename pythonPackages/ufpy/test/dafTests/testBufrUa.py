# #
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
# #

from __future__ import print_function
from awips.dataaccess import DataAccessLayer as DAL

from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint
import baseDafTestCase
import unittest

#
# Test DAF support for bufrua data
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
#
#


class BufrUaTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for bufrua data"""

    datatype = "bufrua"

    location = "72558"
    """stationid corresponding to KOAX"""

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("reportType", "2020")
        self.runLocationsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames(self.location)
        req.addIdentifier("reportType", "2020")
        self.runTimesTest(req)

    def testGetGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames(self.location)
        req.addIdentifier("reportType", "2020")
        req.setParameters("sfcPressure", "staName", "rptType", "tdMan")

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
                print(" sfcPressure=" + record.getString("sfcPressure") + record.getUnit("sfcPressure"), end="")
                print(" staName=" + record.getString("staName"), end="")
                print(" rptType=" + record.getString("rptType") + record.getUnit("rptType"), end="")
            else:
                print(" tdMan=" + str(record.getNumber("tdMan")) + record.getUnit("tdMan"), end="")
            print(" geometry=", record.getGeometry())

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
        req.addIdentifier(key, constraint)
        # As an identifier it is "reportType" but as a parameter it is
        # "rptType"... this is weird...
        req.setParameters("staName", "rptType")
        return self.runGeometryDataTest(req)

    def testGetDataWithEqualsString(self):
        geometryData = self._runConstraintTest('reportType', '=', '2022')
        for record in geometryData:
            self.assertEqual(record.getString('rptType'), '2022')

    def testGetDataWithEqualsUnicode(self):
        geometryData = self._runConstraintTest('reportType', '=', u'2022')
        for record in geometryData:
            self.assertEqual(record.getString('rptType'), '2022')

    def testGetDataWithEqualsInt(self):
        geometryData = self._runConstraintTest('reportType', '=', 2022)
        for record in geometryData:
            self.assertEqual(record.getString('rptType'), '2022')

    def testGetDataWithEqualsLong(self):
        geometryData = self._runConstraintTest('reportType', '=', 2022L)
        for record in geometryData:
            self.assertEqual(record.getString('rptType'), '2022')

    # No float test because no float identifiers are available


    def testGetDataWithEqualsNone(self):
        geometryData = self._runConstraintTest('reportType', '=', None)
        for record in geometryData:
            self.assertEqual(record.getType('rptType'), 'NULL')

    def testGetDataWithNotEquals(self):
        geometryData = self._runConstraintTest('reportType', '!=', 2022)
        for record in geometryData:
            self.assertNotEqual(record.getString('rptType'), '2022')

    def testGetDataWithNotEqualsNone(self):
        geometryData = self._runConstraintTest('reportType', '!=', None)
        for record in geometryData:
            self.assertNotEqual(record.getType('rptType'), 'NULL')

    def testGetDataWithGreaterThan(self):
        geometryData = self._runConstraintTest('reportType', '>', 2022)
        for record in geometryData:
            self.assertGreater(record.getString('rptType'), '2022')

    def testGetDataWithLessThan(self):
        geometryData = self._runConstraintTest('reportType', '<', 2022)
        for record in geometryData:
            self.assertLess(record.getString('rptType'), '2022')

    def testGetDataWithGreaterThanEquals(self):
        geometryData = self._runConstraintTest('reportType', '>=', 2022)
        for record in geometryData:
            self.assertGreaterEqual(record.getString('rptType'), '2022')

    def testGetDataWithLessThanEquals(self):
        geometryData = self._runConstraintTest('reportType', '<=', 2022)
        for record in geometryData:
            self.assertLessEqual(record.getString('rptType'), '2022')

    def testGetDataWithInTuple(self):
        collection = ('2022', '2032')
        geometryData = self._runConstraintTest('reportType', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getString('rptType'), collection)

    def testGetDataWithInList(self):
        collection = ['2022', '2032']
        geometryData = self._runConstraintTest('reportType', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getString('rptType'), collection)

    def testGetDataWithInGenerator(self):
        collection = ('2022', '2032')
        generator = (item for item in collection)
        geometryData = self._runConstraintTest('reportType', 'in', generator)
        for record in geometryData:
            self.assertIn(record.getString('rptType'), collection)

    def testGetDataWithNotInList(self):
        collection = ('2022', '2032')
        geometryData = self._runConstraintTest('reportType', 'not in', collection)
        for record in geometryData:
            self.assertNotIn(record.getString('rptType'), collection)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('reportType', 'junk', '2022')

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('reportType', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('rptType', 'in', [])

    def testGetDataWithNestedInConstraintThrowsException(self):
        collection = ('2022', '2032', ())
        with self.assertRaises(TypeError):
            self._runConstraintTest('rptType', 'in', collection)
