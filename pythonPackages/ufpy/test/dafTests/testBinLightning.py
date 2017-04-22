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
from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint


import baseDafTestCase
import unittest

#
# Test DAF support for binlightning data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#    04/21/16        5551          tgurney        Add tests to verify #5551
#    04/25/16        5587          tgurney        Enable skipped test added in
#                                                 #5551
#    04/26/16        5587          tgurney        Move identifier values tests
#                                                 out of base class
#    06/01/16        5587          tgurney        Update testGetIdentifierValues
#    06/03/16        5574          tgurney        Add advanced query tests
#    06/13/16        5574          tgurney        Typo
#    06/30/16        5725          tgurney        Add test for NOT IN
#    11/08/16        5985          tgurney        Do not check data times
#    
#


class BinLightningTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for binlightning data"""

    datatype = "binlightning"

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        self.runParametersTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("source", "NLDN")
        self.runTimesTest(req)

    def testGetGeometryDataSingleSourceSingleParameter(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("source", "NLDN")
        req.setParameters('intensity')
        self.runGeometryDataTest(req, checkDataTimes=False)

    def testGetGeometryDataInvalidParamRaisesIncompatibleRequestException(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("source", "NLDN")
        req.setParameters('blahblahblah')
        with self.assertRaises(ThriftRequestException) as cm:
            self.runGeometryDataTest(req)
        self.assertIn('IncompatibleRequestException', str(cm.exception))

    def testGetGeometryDataSingleSourceAllParameters(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("source", "NLDN")
        req.setParameters(*DAL.getAvailableParameters(req))
        self.runGeometryDataTest(req, checkDataTimes=False)

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
        req.setParameters('intensity')
        return self.runGeometryDataTest(req, checkDataTimes=False)

    def testGetDataWithEqualsString(self):
        geomData = self._runConstraintTest('source', '=', 'NLDN')
        for record in geomData:
            self.assertEqual(record.getAttribute('source'), 'NLDN')

    def testGetDataWithEqualsUnicode(self):
        geomData = self._runConstraintTest('source', '=', u'NLDN')
        for record in geomData:
            self.assertEqual(record.getAttribute('source'), 'NLDN')

    def testGetDataWithEqualsInt(self):
        geomData = self._runConstraintTest('source', '=', 1000)
        for record in geomData:
            self.assertEqual(record.getAttribute('source'), 1000)

    def testGetDataWithEqualsLong(self):
        geomData = self._runConstraintTest('source', '=', 1000L)
        for record in geomData:
            self.assertEqual(record.getAttribute('source'), 1000)

    def testGetDataWithEqualsFloat(self):
        geomData = self._runConstraintTest('source', '=', 1.0)
        for record in geomData:
            self.assertEqual(round(record.getAttribute('source'), 1), 1.0)

    def testGetDataWithEqualsNone(self):
        geomData = self._runConstraintTest('source', '=', None)
        for record in geomData:
            self.assertIsNone(record.getAttribute('source'))

    def testGetDataWithNotEquals(self):
        geomData = self._runConstraintTest('source', '!=', 'NLDN')
        for record in geomData:
            self.assertNotEqual(record.getAttribute('source'), 'NLDN')

    def testGetDataWithNotEqualsNone(self):
        geomData = self._runConstraintTest('source', '!=', None)
        for record in geomData:
            self.assertIsNotNone(record.getAttribute('source'))

    def testGetDataWithGreaterThan(self):
        geomData = self._runConstraintTest('source', '>', 'NLDN')
        for record in geomData:
            self.assertGreater(record.getAttribute('source'), 'NLDN')

    def testGetDataWithLessThan(self):
        geomData = self._runConstraintTest('source', '<', 'NLDN')
        for record in geomData:
            self.assertLess(record.getAttribute('source'), 'NLDN')

    def testGetDataWithGreaterThanEquals(self):
        geomData = self._runConstraintTest('source', '>=', 'NLDN')
        for record in geomData:
            self.assertGreaterEqual(record.getAttribute('source'), 'NLDN')

    def testGetDataWithLessThanEquals(self):
        geomData = self._runConstraintTest('source', '<=', 'NLDN')
        for record in geomData:
            self.assertLessEqual(record.getAttribute('source'), 'NLDN')

    def testGetDataWithInTuple(self):
        geomData = self._runConstraintTest('source', 'in', ('NLDN', 'ENTLN'))
        for record in geomData:
            self.assertIn(record.getAttribute('source'), ('NLDN', 'ENTLN'))

    def testGetDataWithInList(self):
        geomData = self._runConstraintTest('source', 'in', ['NLDN', 'ENTLN'])
        for record in geomData:
            self.assertIn(record.getAttribute('source'), ('NLDN', 'ENTLN'))

    def testGetDataWithInGenerator(self):
        generator = (item for item in ('NLDN', 'ENTLN'))
        geomData = self._runConstraintTest('source', 'in', generator)
        for record in geomData:
            self.assertIn(record.getAttribute('source'), ('NLDN', 'ENTLN'))

    def testGetDataWithNotInList(self):
        geomData = self._runConstraintTest('source', 'not in', ['NLDN', 'blah'])
        for record in geomData:
            self.assertNotIn(record.getAttribute('source'), ('NLDN', 'blah'))

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('source', 'junk', 'NLDN')

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('source', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('source', 'in', [])
