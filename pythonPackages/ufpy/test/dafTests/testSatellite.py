#!/usr/bin/env python
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
from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint

import baseDafTestCase
import unittest

#
# Test DAF support for satellite data
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
#    06/07/16        5574          tgurney        Add advanced query tests
#    06/13/16        5574          tgurney        Typo
#
#


class SatelliteTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for satellite data"""

    datatype = "satellite"

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        self.runLocationsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames("West CONUS")
        self.runTimesTest(req)

    def testGetGridData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setParameters("Imager 11 micron IR")
        req.setLocationNames("West CONUS")
        self.runGridDataTest(req)

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
        req.setParameters("Imager 11 micron IR")
        req.setLocationNames("West CONUS")
        return self.runGridDataTest(req)

    def testGetDataWithEqualsString(self):
        gridData = self._runConstraintTest('creatingEntity', '=', 'Composite')
        for record in gridData:
            self.assertEqual(record.getAttribute('creatingEntity'), 'Composite')

    def testGetDataWithEqualsUnicode(self):
        gridData = self._runConstraintTest('creatingEntity', '=', u'Composite')
        for record in gridData:
            self.assertEqual(record.getAttribute('creatingEntity'), 'Composite')

    def testGetDataWithEqualsInt(self):
        gridData = self._runConstraintTest('creatingEntity', '=', 1000)
        for record in gridData:
            self.assertEqual(record.getAttribute('creatingEntity'), 1000)

    def testGetDataWithEqualsLong(self):
        gridData = self._runConstraintTest('creatingEntity', '=', 1000L)
        for record in gridData:
            self.assertEqual(record.getAttribute('creatingEntity'), 1000)

    def testGetDataWithEqualsFloat(self):
        gridData = self._runConstraintTest('creatingEntity', '=', 1.0)
        for record in gridData:
            self.assertEqual(round(record.getAttribute('creatingEntity'), 1), 1.0)

    def testGetDataWithEqualsNone(self):
        gridData = self._runConstraintTest('creatingEntity', '=', None)
        for record in gridData:
            self.assertIsNone(record.getAttribute('creatingEntity'))

    def testGetDataWithNotEquals(self):
        gridData = self._runConstraintTest('creatingEntity', '!=', 'Composite')
        for record in gridData:
            self.assertNotEqual(record.getAttribute('creatingEntity'), 'Composite')

    def testGetDataWithNotEqualsNone(self):
        gridData = self._runConstraintTest('creatingEntity', '!=', None)
        for record in gridData:
            self.assertIsNotNone(record.getAttribute('creatingEntity'))

    def testGetDataWithGreaterThan(self):
        gridData = self._runConstraintTest('creatingEntity', '>', 'Composite')
        for record in gridData:
            self.assertGreater(record.getAttribute('creatingEntity'), 'Composite')

    def testGetDataWithLessThan(self):
        gridData = self._runConstraintTest('creatingEntity', '<', 'Composite')
        for record in gridData:
            self.assertLess(record.getAttribute('creatingEntity'), 'Composite')

    def testGetDataWithGreaterThanEquals(self):
        gridData = self._runConstraintTest('creatingEntity', '>=', 'Composite')
        for record in gridData:
            self.assertGreaterEqual(record.getAttribute('creatingEntity'), 'Composite')

    def testGetDataWithLessThanEquals(self):
        gridData = self._runConstraintTest('creatingEntity', '<=', 'Composite')
        for record in gridData:
            self.assertLessEqual(record.getAttribute('creatingEntity'), 'Composite')

    def testGetDataWithInTuple(self):
        collection = ('Composite', 'Miscellaneous')
        gridData = self._runConstraintTest('creatingEntity', 'in', collection)
        for record in gridData:
            self.assertIn(record.getAttribute('creatingEntity'), collection)

    def testGetDataWithInList(self):
        collection = ('Composite', 'Miscellaneous')
        gridData = self._runConstraintTest('creatingEntity', 'in', collection)
        for record in gridData:
            self.assertIn(record.getAttribute('creatingEntity'), collection)

    def testGetDataWithInGenerator(self):
        collection = ('Composite', 'Miscellaneous')
        generator = (item for item in collection)
        gridData = self._runConstraintTest('creatingEntity', 'in', generator)
        for record in gridData:
            self.assertIn(record.getAttribute('creatingEntity'), collection)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('creatingEntity', 'junk', 'Composite')

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('creatingEntity', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('creatingEntity', 'in', [])

    def testGetDataWithNestedInConstraintThrowsException(self):
        collection = ('Composite', 'Miscellaneous', ())
        with self.assertRaises(TypeError):
            self._runConstraintTest('creatingEntity', 'in', collection)
