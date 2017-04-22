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
from awips.dataaccess import DataAccessLayer as DAL

from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint
import baseDafTestCase
import unittest

#
# Test DAF support for common_obs_spatial data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#    05/26/16        5587          njensen        Added testGetIdentifierValues()
#    06/01/16        5587          tgurney        Move testIdentifiers() to
#                                                 superclass
#    06/13/16        5574          tgurney        Add advanced query tests
#    06/21/16        5548          tgurney        Skip tests that cause errors
#    06/30/16        5725          tgurney        Add test for NOT IN
#


class CommonObsSpatialTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for common_obs_spatial data"""

    datatype = "common_obs_spatial"

    envelope = box(-97.0, 41.0, -96.0, 42.0)
    """Default request area (box around KOAX)"""

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("country", ["US", "CN"])
        self.runLocationsTest(req)

    def testGetIdentifierValues(self):
        self.runGetIdValuesTest(['country'])

    @unittest.skip('avoid EDEX error')
    def testGetInvalidIdentifierValuesThrowsException(self):
        self.runInvalidIdValuesTest()

    @unittest.skip('avoid EDEX error')
    def testGetNonexistentIdentifierValuesThrowsException(self):
        self.runNonexistentIdValuesTest()

    def testGetGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(self.envelope)
        req.setParameters("name", "stationid")
        self.runGeometryDataTest(req)

    def testRequestingTimesThrowsTimeAgnosticDataException(self):
        req = DAL.newDataRequest(self.datatype)
        self.runTimeAgnosticTest(req)

    def _runConstraintTest(self, key, operator, value):
        req = DAL.newDataRequest(self.datatype)
        constraint = RequestConstraint.new(operator, value)
        req.addIdentifier(key, constraint)
        req.setParameters('catalogtype', 'elevation', 'state')
        return self.runGeometryDataTest(req)

    def testGetDataWithEqualsString(self):
        geometryData = self._runConstraintTest('state', '=', 'NE')
        for record in geometryData:
            self.assertEqual(record.getString('state'), 'NE')

    def testGetDataWithEqualsUnicode(self):
        geometryData = self._runConstraintTest('state', '=', u'NE')
        for record in geometryData:
            self.assertEqual(record.getString('state'), 'NE')

    def testGetDataWithEqualsInt(self):
        geometryData = self._runConstraintTest('catalogtype', '=', 32)
        for record in geometryData:
            self.assertEqual(record.getNumber('catalogtype'), 32)

    def testGetDataWithEqualsLong(self):
        geometryData = self._runConstraintTest('elevation', '=', 0L)
        for record in geometryData:
            self.assertEqual(record.getNumber('elevation'), 0)

    # No float test since there are no float identifiers available. Attempting
    # to filter a non-float identifier on a float value raises an exception.

    def testGetDataWithEqualsNone(self):
        geometryData = self._runConstraintTest('state', '=', None)
        for record in geometryData:
            self.assertEqual(record.getType('state'), 'NULL')

    def testGetDataWithNotEquals(self):
        geometryData = self._runConstraintTest('state', '!=', 'NE')
        for record in geometryData:
            self.assertNotEqual(record.getString('state'), 'NE')

    def testGetDataWithNotEqualsNone(self):
        geometryData = self._runConstraintTest('state', '!=', None)
        for record in geometryData:
            self.assertNotEqual(record.getType('state'), 'NULL')

    def testGetDataWithGreaterThan(self):
        geometryData = self._runConstraintTest('elevation', '>', 500)
        for record in geometryData:
            self.assertGreater(record.getNumber('elevation'), 500)

    def testGetDataWithLessThan(self):
        geometryData = self._runConstraintTest('elevation', '<', 100)
        for record in geometryData:
            self.assertLess(record.getNumber('elevation'), 100)

    def testGetDataWithGreaterThanEquals(self):
        geometryData = self._runConstraintTest('elevation', '>=', 500)
        for record in geometryData:
            self.assertGreaterEqual(record.getNumber('elevation'), 500)

    def testGetDataWithLessThanEquals(self):
        geometryData = self._runConstraintTest('elevation', '<=', 100)
        for record in geometryData:
            self.assertLessEqual(record.getNumber('elevation'), 100)

    def testGetDataWithInTuple(self):
        collection = ('NE', 'TX')
        geometryData = self._runConstraintTest('state', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getString('state'), collection)

    def testGetDataWithInList(self):
        collection = ['NE', 'TX']
        geometryData = self._runConstraintTest('state', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getString('state'), collection)

    def testGetDataWithInGenerator(self):
        collection = ('NE', 'TX')
        generator = (item for item in collection)
        geometryData = self._runConstraintTest('state', 'in', generator)
        for record in geometryData:
            self.assertIn(record.getString('state'), collection)

    def testGetDataWithNotInList(self):
        collection = ('NE', 'TX')
        geometryData = self._runConstraintTest('state', 'not in', collection)
        for record in geometryData:
            self.assertNotIn(record.getString('state'), collection)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('state', 'junk', 'NE')

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('state', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('state', 'in', [])
