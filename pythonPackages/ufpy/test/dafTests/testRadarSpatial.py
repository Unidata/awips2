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
# Test DAF support for radar_spatial data
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
#    06/30/16        5725          tgurney        Add test for NOT IN
#
#


class RadarSpatialTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for radar_spatial data"""

    datatype = "radar_spatial"

    envelope = box(-97.0, 41.0, -96.0, 42.0)
    """
    Default request area (box around KOAX)
    """

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(self.envelope)
        self.runLocationsTest(req)

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        self.runParametersTest(req)

    def testGetIdentifierValues(self):
        self.runGetIdValuesTest(['wfo_id'])

    def testGetGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames("TORD", "TMDW")
        req.setParameters("wfo_id", "name", "elevmeter")
        self.runGeometryDataTest(req)

    def testRequestingTimesThrowsTimeAgnosticDataException(self):
        req = DAL.newDataRequest(self.datatype)
        self.runTimeAgnosticTest(req)

    def _runConstraintTest(self, key, operator, value):
        req = DAL.newDataRequest(self.datatype)
        constraint = RequestConstraint.new(operator, value)
        req.addIdentifier(key, constraint)
        req.setParameters('elevmeter', 'eqp_elv', 'wfo_id', 'immutablex')
        return self.runGeometryDataTest(req)

    def testGetDataWithEqualsString(self):
        geometryData = self._runConstraintTest('wfo_id', '=', 'OAX')
        for record in geometryData:
            self.assertEqual(record.getString('wfo_id'), 'OAX')

    def testGetDataWithEqualsUnicode(self):
        geometryData = self._runConstraintTest('wfo_id', '=', u'OAX')
        for record in geometryData:
            self.assertEqual(record.getString('wfo_id'), 'OAX')

    def testGetDataWithEqualsInt(self):
        geometryData = self._runConstraintTest('immutablex', '=', 57)
        for record in geometryData:
            self.assertEqual(record.getNumber('immutablex'), 57)

    def testGetDataWithEqualsLong(self):
        geometryData = self._runConstraintTest('immutablex', '=', 57L)
        for record in geometryData:
            self.assertEqual(record.getNumber('immutablex'), 57)

    def testGetDataWithEqualsFloat(self):
        geometryData = self._runConstraintTest('immutablex', '=', 57.0)
        for record in geometryData:
            self.assertEqual(round(record.getNumber('immutablex'), 1), 57.0)

    def testGetDataWithEqualsNone(self):
        geometryData = self._runConstraintTest('wfo_id', '=', None)
        for record in geometryData:
            self.assertEqual(record.getType('wfo_id'), 'NULL')

    def testGetDataWithNotEquals(self):
        geometryData = self._runConstraintTest('wfo_id', '!=', 'OAX')
        for record in geometryData:
            self.assertNotEquals(record.getString('wfo_id'), 'OAX')

    def testGetDataWithNotEqualsNone(self):
        geometryData = self._runConstraintTest('wfo_id', '!=', None)
        for record in geometryData:
            self.assertNotEqual(record.getType('wfo_id'), 'NULL')

    def testGetDataWithGreaterThan(self):
        geometryData = self._runConstraintTest('elevmeter', '>', 1000)
        for record in geometryData:
            self.assertGreater(record.getNumber('elevmeter'), 1000)

    def testGetDataWithLessThan(self):
        geometryData = self._runConstraintTest('elevmeter', '<', 1000)
        for record in geometryData:
            self.assertLess(record.getNumber('elevmeter'), 1000)

    def testGetDataWithGreaterThanEquals(self):
        geometryData = self._runConstraintTest('eqp_elv', '>=', 1295)
        for record in geometryData:
            self.assertGreaterEqual(record.getNumber('eqp_elv'), 1295)

    def testGetDataWithLessThanEquals(self):
        geometryData = self._runConstraintTest('eqp_elv', '<=', 138)
        for record in geometryData:
            self.assertLessEqual(record.getNumber('eqp_elv'), 138)

    def testGetDataWithInTuple(self):
        collection = ('OAX', 'GID')
        geometryData = self._runConstraintTest('wfo_id', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getString('wfo_id'), collection)

    def testGetDataWithInList(self):
        collection = ['OAX', 'GID']
        geometryData = self._runConstraintTest('wfo_id', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getString('wfo_id'), collection)

    def testGetDataWithInGenerator(self):
        collection = ('OAX', 'GID')
        generator = (item for item in collection)
        geometryData = self._runConstraintTest('wfo_id', 'in', generator)
        for record in geometryData:
            self.assertIn(record.getString('wfo_id'), collection)

    def testGetDataWithNotInList(self):
        collection = ['OAX', 'GID']
        geometryData = self._runConstraintTest('wfo_id', 'not in', collection)
        for record in geometryData:
            self.assertNotIn(record.getString('wfo_id'), collection)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('wfo_id', 'junk', 'OAX')

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('wfo_id', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('wfo_id', 'in', [])
