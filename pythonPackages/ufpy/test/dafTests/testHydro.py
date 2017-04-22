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
import datetime
from awips.dataaccess import DataAccessLayer as DAL
from awips.ThriftClient import ThriftRequestException

from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange
import baseDafTestCase
import unittest

#
# Test DAF support for hydro data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#    04/21/16        5596          tgurney        Add tests to verify #5596
#    04/26/16        5587          tgurney        Add identifier values tests
#    06/09/16        5574          tgurney        Add advanced query tests
#    06/13/16        5574          tgurney        Fix checks for None
#    06/21/16        5548          tgurney        Skip tests that cause errors
#    06/30/16        5725          tgurney        Add test for NOT IN
#    10/06/16        5926          dgilling       Add additional location tests.
#
#


class HydroTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for hydro data"""

    datatype = 'hydro'

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'height')
        self.runParametersTest(req)

    def testGetAvailableParametersFullyQualifiedTable(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'public.height')
        self.runParametersTest(req)

    def testGetAvailableParamsNoTableThrowsInvalidIdentifiersException(self):
        req = DAL.newDataRequest(self.datatype)
        with self.assertRaises(ThriftRequestException) as cm:
            self.runParametersTest(req)
        self.assertIn('InvalidIdentifiersException', str(cm.exception))

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'height')
        self.runLocationsTest(req)

    def testGetAvailableLocationsWithConstraint(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'height')
        req.addIdentifier('value', RequestConstraint.new('>', 5.0))
        self.runLocationsTest(req)

    def testGetAvailableLocationsWithInvalidTable(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'city')
        with self.assertRaises(ThriftRequestException) as cm:
            DAL.getAvailableLocationNames(req)
        self.assertIn('IncompatibleRequestException', str(cm.exception))

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'height')
        req.setParameters('lid', 'quality_code')
        self.runTimesTest(req)

    def testGetGeometryDataWithoutLocationSpecified(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'height')
        req.setParameters('lid', 'quality_code')
        self.runGeometryDataTest(req)

    def testGetGeometryDataWithLocationSpecified(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'fcstheight')
        locs = DAL.getAvailableLocationNames(req)
        if locs:
            req.setLocationNames(locs[0])
            req.setParameters('probability', 'value')
            data = self.runGeometryDataTest(req)
            self.assertNotEqual(len(data), 0)

    def testGetTableIdentifierValues(self):
        self.runGetIdValuesTest(['table'])

    def testGetColumnIdValuesWithTable(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'height')
        idValues = DAL.getIdentifierValues(req, 'lid')
        self.assertTrue(hasattr(idValues, '__iter__'))

    @unittest.skip('avoid EDEX error')
    def testGetColumnIdValuesWithNonexistentTableThrowsException(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'nonexistentjunk')
        with self.assertRaises(ThriftRequestException):
            idValues = DAL.getIdentifierValues(req, 'lid')

    def testGetColumnIdValuesWithoutTableThrowsException(self):
        req = DAL.newDataRequest(self.datatype)
        with self.assertRaises(ThriftRequestException):
            idValues = DAL.getIdentifierValues(req, 'lid')

    @unittest.skip('avoid EDEX error')
    def testGetNonexistentColumnIdValuesThrowsException(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'height')
        with self.assertRaises(ThriftRequestException):
            idValues = DAL.getIdentifierValues(req, 'nonexistentjunk')

    def testGetInvalidIdentifierValuesThrowsException(self):
        self.runInvalidIdValuesTest()

    def testGetNonexistentIdentifierValuesThrowsException(self):
        self.runNonexistentIdValuesTest()

    def _runConstraintTest(self, key, operator, value):
        req = DAL.newDataRequest(self.datatype)
        constraint = RequestConstraint.new(operator, value)
        req.addIdentifier(key, constraint)
        req.addIdentifier('table', 'height')
        req.addIdentifier('ts', 'RG')
        req.setParameters('value', 'lid', 'quality_code')
        return self.runGeometryDataTest(req)

    def testGetDataWithEqualsString(self):
        geometryData = self._runConstraintTest('value', '=', '3')
        for record in geometryData:
            self.assertEqual(record.getNumber('value'), 3)

    def testGetDataWithEqualsUnicode(self):
        geometryData = self._runConstraintTest('value', '=', u'3')
        for record in geometryData:
            self.assertEqual(record.getNumber('value'), 3)

    def testGetDataWithEqualsInt(self):
        geometryData = self._runConstraintTest('value', '=', 3)
        for record in geometryData:
            self.assertEqual(record.getNumber('value'), 3)

    def testGetDataWithEqualsLong(self):
        geometryData = self._runConstraintTest('value', '=', 3L)
        for record in geometryData:
            self.assertEqual(record.getNumber('value'), 3L)

    def testGetDataWithEqualsFloat(self):
        geometryData = self._runConstraintTest('value', '=', 3.0)
        for record in geometryData:
            self.assertEqual(round(record.getNumber('value'), 1), 3.0)

    def testGetDataWithEqualsNone(self):
        geometryData = self._runConstraintTest('value', '=', None)
        self.assertEqual(len(geometryData), 0)

    def testGetDataWithNotEquals(self):
        geometryData = self._runConstraintTest('value', '!=', 3)
        for record in geometryData:
            self.assertNotEqual(record.getNumber('value'), '3')

    def testGetDataWithNotEqualsNone(self):
        geometryData = self._runConstraintTest('value', '!=', None)
        self.assertNotEqual(len(geometryData), 0)
        for record in geometryData:
            self.assertNotEqual(record.getType('value'), 'NULL')

    def testGetDataWithGreaterThan(self):
        geometryData = self._runConstraintTest('value', '>', 3)
        for record in geometryData:
            self.assertGreater(record.getNumber('value'), 3)

    def testGetDataWithLessThan(self):
        geometryData = self._runConstraintTest('value', '<', 3)
        for record in geometryData:
            self.assertLess(record.getNumber('value'), 3)

    def testGetDataWithGreaterThanEquals(self):
        geometryData = self._runConstraintTest('value', '>=', 3)
        for record in geometryData:
            self.assertGreaterEqual(record.getNumber('value'), 3)

    def testGetDataWithLessThanEquals(self):
        geometryData = self._runConstraintTest('value', '<=', 3)
        for record in geometryData:
            self.assertLessEqual(record.getNumber('value'), 3)

    def testGetDataWithInTuple(self):
        collection = (3, 4)
        geometryData = self._runConstraintTest('value', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getNumber('value'), collection)

    def testGetDataWithInList(self):
        collection = [3, 4]
        geometryData = self._runConstraintTest('value', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getNumber('value'), collection)

    def testGetDataWithInGenerator(self):
        collection = (3, 4)
        generator = (item for item in collection)
        geometryData = self._runConstraintTest('value', 'in', generator)
        for record in geometryData:
            self.assertIn(record.getNumber('value'), collection)

    def testGetDataWithNotInList(self):
        collection = [3, 4]
        geometryData = self._runConstraintTest('value', 'not in', collection)
        for record in geometryData:
            self.assertNotIn(record.getNumber('value'), collection)

    def testGetDataWithTimeRange(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('table', 'height')
        req.addIdentifier('ts', 'RG')
        req.setParameters('value', 'lid', 'quality_code')
        times = DAL.getAvailableTimes(req)
        limitTimes = times[-self.numTimesToLimit:]
        startTime = datetime.datetime.utcfromtimestamp(limitTimes[0].getRefTime().getTime()/1000)
        endTime = datetime.datetime.utcnow()
        tr = TimeRange(startTime, endTime)
        self.runGeometryDataTestWithTimeRange(req, tr)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('value', 'junk', 3)

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('value', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('value', 'in', [])

    def testGetDataWithNestedInConstraintThrowsException(self):
        collection = ('3', '4', ())
        with self.assertRaises(TypeError):
            self._runConstraintTest('value', 'in', collection)
