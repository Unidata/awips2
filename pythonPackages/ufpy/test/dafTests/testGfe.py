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
from ufpy.dataaccess import DataAccessLayer as DAL

import baseDafTestCase
import unittest

#
# Test DAF support for GFE data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#    05/23/16        5637          bsteffen       Test vectors
#    05/31/16        5587          tgurney        Add getIdentifierValues tests
#    06/01/16        5587          tgurney        Update testGetIdentifierValues
#    06/17/16        5574          mapeters       Add advanced query tests
#
#


class GfeTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for GFE data"""

    datatype = 'gfe'

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('modelName', 'Fcst')
        self.runLocationsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('modelName', 'Fcst')
        req.addIdentifier('siteId', 'OAX')
        self.runTimesTest(req)

    def testGetGridData(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('modelName', 'Fcst')
        req.addIdentifier('siteId', 'OAX')
        req.setParameters('T')
        self.runGridDataTest(req)

    def testGetVectorGridData(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('modelName', 'Fcst')
        req.addIdentifier('siteId', 'OAX')
        req.setParameters('Wind')
        times = DAL.getAvailableTimes(req)
        if not(times):
            raise unittest.SkipTest('No Wind Data available for testing')
        gridData = DAL.getGridData(req, [times[0]])
        hasWind = False
        hasDir = False
        for grid in gridData:
            if grid.getParameter() == 'Wind':
                self.assertEqual(grid.getUnit(),'kts')
                hasWind = True
            elif grid.getParameter() == 'WindDirection':
                self.assertEqual(grid.getUnit(),'deg')
                hasDir = True
        self.assertTrue(hasWind, 'Wind Magnitude grid is not present')
        self.assertTrue(hasDir, 'Wind Direction grid is not present')

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
        req.setLocationNames('OAX')
        req.setParameters('T')
        return self.runGridDataTest(req)

    def testGetDataWithEqualsString(self):
        geometryData = self._runConstraintTest('modelName', '=', 'Fcst')
        for record in geometryData:
            self.assertEqual(record.getAttribute('modelName'), 'Fcst')

    def testGetDataWithEqualsUnicode(self):
        geometryData = self._runConstraintTest('modelName', '=', u'Fcst')
        for record in geometryData:
            self.assertEqual(record.getAttribute('modelName'), 'Fcst')

    # No numeric tests since no numeric identifiers are available.

    def testGetDataWithEqualsNone(self):
        geometryData = self._runConstraintTest('modelName', '=', None)
        for record in geometryData:
            self.assertIsNone(record.getAttribute('modelName'))

    def testGetDataWithNotEquals(self):
        geometryData = self._runConstraintTest('modelName', '!=', 'Fcst')
        for record in geometryData:
            self.assertNotEqual(record.getAttribute('modelName'), 'Fcst')

    def testGetDataWithNotEqualsNone(self):
        geometryData = self._runConstraintTest('modelName', '!=', None)
        for record in geometryData:
            self.assertIsNotNone(record.getAttribute('modelName'))

    def testGetDataWithGreaterThan(self):
        geometryData = self._runConstraintTest('modelName', '>', 'Fcst')
        for record in geometryData:
            self.assertGreater(record.getAttribute('modelName'), 'Fcst')

    def testGetDataWithLessThan(self):
        geometryData = self._runConstraintTest('modelName', '<', 'Fcst')
        for record in geometryData:
            self.assertLess(record.getAttribute('modelName'), 'Fcst')

    def testGetDataWithGreaterThanEquals(self):
        geometryData = self._runConstraintTest('modelName', '>=', 'Fcst')
        for record in geometryData:
            self.assertGreaterEqual(record.getAttribute('modelName'), 'Fcst')

    def testGetDataWithLessThanEquals(self):
        geometryData = self._runConstraintTest('modelName', '<=', 'Fcst')
        for record in geometryData:
            self.assertLessEqual(record.getAttribute('modelName'), 'Fcst')

    def testGetDataWithInTuple(self):
        collection = ('Fcst', 'SAT')
        geometryData = self._runConstraintTest('modelName', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getAttribute('modelName'), collection)

    def testGetDataWithInList(self):
        collection = ['Fcst', 'SAT']
        geometryData = self._runConstraintTest('modelName', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getAttribute('modelName'), collection)

    def testGetDataWithInGenerator(self):
        collection = ('Fcst', 'SAT')
        generator = (item for item in collection)
        geometryData = self._runConstraintTest('modelName', 'in', generator)
        for record in geometryData:
            self.assertIn(record.getAttribute('modelName'), collection)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('modelName', 'junk', 'Fcst')

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('modelName', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('modelName', 'in', [])