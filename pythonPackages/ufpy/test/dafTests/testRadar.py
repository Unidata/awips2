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
from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint
from shapely.geometry import box
from ufpy.dataaccess import DataAccessLayer as DAL
from ufpy.ThriftClient import ThriftRequestException

import baseDafTestCase
import unittest

#
# Test DAF support for radar data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#    04/26/16        5587          tgurney        Move identifier values tests
#                                                 out of base class
#    06/01/16        5587          tgurney        Update testGetIdentifierValues
#    06/08/16        5574          mapeters       Add advanced query tests
#    06/13/16        5574          tgurney        Fix checks for None
#    06/14/16        5548          tgurney        Undo previous change (broke
#                                                 test)
#
#


class RadarTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for radar data"""

    datatype = 'radar'

    envelope = box(-97.0, 41.0, -96.0, 42.0)
    """Request area (box around KOAX)"""

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        self.runLocationsTest(req)

    def testGetAvailableLevels(self):
        req = DAL.newDataRequest(self.datatype)
        self.runLevelsTest(req)
        
    def testGetAvailableLevelsWithInvalidLevelIdentifierThrowsException(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('level.one.field', 'invalidLevelField')
        with self.assertRaises(ThriftRequestException) as cm:
            self.runLevelsTest(req)
        self.assertIn('IncompatibleRequestException', str(cm.exception))

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(self.envelope)
        self.runTimesTest(req)

    def testGetGridData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setEnvelope(self.envelope)
        req.setLocationNames('koax')
        req.setParameters('94')
        # Don't test shapes since they may differ.
        self.runGridDataTest(req, testSameShape=False)

    def testGetIdentifierValues(self):
        req = DAL.newDataRequest(self.datatype)
        optionalIds = set(DAL.getOptionalIdentifiers(req))
        requiredIds = set(DAL.getRequiredIdentifiers(req))
        self.runGetIdValuesTest(optionalIds | requiredIds)

    def testGetInvalidIdentifierValuesThrowsException(self):
        self.runInvalidIdValuesTest()

    def testGetNonexistentIdentifierValuesThrowsException(self):
        self.runNonexistentIdValuesTest()

    def _runConstraintTest(self, key, operator, value):
        req = DAL.newDataRequest(self.datatype)
        constraint = RequestConstraint.new(operator, value)
        req.addIdentifier(key, constraint)
        req.setParameters('94')
        # Don't test shapes since they may differ.
        return self.runGridDataTest(req, testSameShape=False)

    def testGetDataWithEqualsString(self):
        gridData = self._runConstraintTest('icao', '=', 'koax')
        for record in gridData:
            self.assertEqual(record.getAttribute('icao'), 'koax')

    def testGetDataWithEqualsUnicode(self):
        gridData = self._runConstraintTest('icao', '=', u'koax')
        for record in gridData:
            self.assertEqual(record.getAttribute('icao'), 'koax')

    def testGetDataWithEqualsInt(self):
        gridData = self._runConstraintTest('icao', '=', 1000)
        for record in gridData:
            self.assertEqual(record.getAttribute('icao'), 1000)

    def testGetDataWithEqualsLong(self):
        gridData = self._runConstraintTest('icao', '=', 1000L)
        for record in gridData:
            self.assertEqual(record.getAttribute('icao'), 1000)

    def testGetDataWithEqualsFloat(self):
        gridData = self._runConstraintTest('icao', '=', 1.0)
        for record in gridData:
            self.assertEqual(round(record.getAttribute('icao'), 1), 1.0)

    def testGetDataWithEqualsNone(self):
        gridData = self._runConstraintTest('icao', '=', None)
        for record in gridData:
            self.assertIsNone(record.getAttribute('icao'))

    def testGetDataWithNotEquals(self):
        gridData = self._runConstraintTest('icao', '!=', 'koax')
        for record in gridData:
            self.assertNotEqual(record.getAttribute('icao'), 'koax')

    def testGetDataWithNotEqualsNone(self):
        gridData = self._runConstraintTest('icao', '!=', None)
        for record in gridData:
            self.assertIsNotNone(record.getAttribute('icao'))

    def testGetDataWithGreaterThan(self):
        gridData = self._runConstraintTest('icao', '>', 'koax')
        for record in gridData:
            self.assertGreater(record.getAttribute('icao'), 'koax')

    def testGetDataWithLessThan(self):
        gridData = self._runConstraintTest('icao', '<', 'koax')
        for record in gridData:
            self.assertLess(record.getAttribute('icao'), 'koax')

    def testGetDataWithGreaterThanEquals(self):
        gridData = self._runConstraintTest('icao', '>=', 'koax')
        for record in gridData:
            self.assertGreaterEqual(record.getAttribute('icao'), 'koax')

    def testGetDataWithLessThanEquals(self):
        gridData = self._runConstraintTest('icao', '<=', 'koax')
        for record in gridData:
            self.assertLessEqual(record.getAttribute('icao'), 'koax')

    def testGetDataWithInTuple(self):
        gridData = self._runConstraintTest('icao', 'in', ('koax', 'tpbi'))
        for record in gridData:
            self.assertIn(record.getAttribute('icao'), ('koax', 'tpbi'))

    def testGetDataWithInList(self):
        gridData = self._runConstraintTest('icao', 'in', ['koax', 'tpbi'])
        for record in gridData:
            self.assertIn(record.getAttribute('icao'), ('koax', 'tpbi'))

    def testGetDataWithInGenerator(self):
        generator = (item for item in ('koax', 'tpbi'))
        gridData = self._runConstraintTest('icao', 'in', generator)
        for record in gridData:
            self.assertIn(record.getAttribute('icao'), ('koax', 'tpbi'))

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('icao', 'junk', 'koax')

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('icao', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('icao', 'in', [])
