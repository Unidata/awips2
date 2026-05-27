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


from dynamicserialize.dstypes.com.raytheon.uf.common.dataquery.requests import RequestConstraint
from ufpy.dataaccess import DataAccessLayer as DAL
from ufpy.ThriftClient import ThriftRequestException

from . import baseDafTestCase
from . import params

#
# Test DAF support for GFE edit area data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/08/17        6298          mapeters       Initial Creation.
#    09/27/17        6463          tgurney        Remove GID site identifier
#
#


class GfeEditAreaTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for GFE edit area data"""

    datatype = 'gfeEditArea'

    siteIdKey = 'siteId'

    editAreaNames = ['ISC_NHA', 'SDZ066', 'StormSurgeWW_EditArea']

    groupKey = 'group'

    groups = ['ISC', 'WFOs', 'FIPS_' + params.SITE_ID]

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier(self.siteIdKey, params.SITE_ID)
        with self.assertRaises(ThriftRequestException):
            self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier(self.siteIdKey, params.SITE_ID)
        self.runLocationsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier(self.siteIdKey, params.SITE_ID)
        with self.assertRaises(ThriftRequestException):
            self.runTimesTest(req)

    def testGetGeometryDataWithoutSiteIdThrowsException(self):
        req = DAL.newDataRequest(self.datatype)
        with self.assertRaises(ThriftRequestException):
            self.runGeometryDataTest(req)

    def testGetGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier(self.siteIdKey, params.SITE_ID)
        data = self.runGeometryDataTest(req)
        for item in data:
            self.assertEqual(params.SITE_ID, item.getAttribute(self.siteIdKey))

    def testGetGeometryDataWithLocNames(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier(self.siteIdKey, params.SITE_ID)
        req.setLocationNames(*self.editAreaNames)
        data = self.runGeometryDataTest(req)
        for item in data:
            self.assertEqual(params.SITE_ID, item.getAttribute(self.siteIdKey))
            self.assertIn(item.getLocationName(), self.editAreaNames)

    def testGetGeometryDataWithGroups(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier(self.siteIdKey, params.SITE_ID)
        req.addIdentifier(self.groupKey, RequestConstraint.new('in', self.groups))
        data = self.runGeometryDataTest(req)
        for item in data:
            self.assertEqual(params.SITE_ID, item.getAttribute(self.siteIdKey))
            self.assertIn(item.getAttribute(self.groupKey), self.groups)

    def testGetGeometryDataWithLocNamesAndGroupsThrowException(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier(self.siteIdKey, params.SITE_ID)
        req.setLocationNames(*self.editAreaNames)
        req.addIdentifier(self.groupKey, RequestConstraint.new('in', self.groups))
        with self.assertRaises(ThriftRequestException):
            self.runGeometryDataTest(req)

    def testGetGeometryDataWithEnvelope(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier(self.siteIdKey, params.SITE_ID)
        req.setEnvelope(params.ENVELOPE)
        data = self.runGeometryDataTest(req)
        for item in data:
            self.assertEqual(params.SITE_ID, item.getAttribute(self.siteIdKey))
            self.assertTrue(params.ENVELOPE.intersects(item.getGeometry()))

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
        req.setLocationNames(*self.editAreaNames)
        return self.runGeometryDataTest(req)

    def testGetDataWithEqualsString(self):
        geomData = self._runConstraintTest(self.siteIdKey, '=', params.SITE_ID)
        for record in geomData:
            self.assertEqual(record.getAttribute(self.siteIdKey), params.SITE_ID)

    # No numeric tests since no numeric identifiers are available.

    def testGetDataWithEqualsNone(self):
        geomData = self._runConstraintTest(self.siteIdKey, '=', None)
        for record in geomData:
            self.assertIsNone(record.getAttribute(self.siteIdKey))

    def testGetDataWithNotEquals(self):
        geomData = self._runConstraintTest(self.siteIdKey, '!=', params.SITE_ID)
        for record in geomData:
            self.assertNotEqual(record.getAttribute(self.siteIdKey), params.SITE_ID)

    def testGetDataWithNotEqualsNone(self):
        geomData = self._runConstraintTest(self.siteIdKey, '!=', None)
        for record in geomData:
            self.assertIsNotNone(record.getAttribute(self.siteIdKey))

    def testGetDataWithGreaterThan(self):
        geomData = self._runConstraintTest(self.siteIdKey, '>', params.SITE_ID)
        for record in geomData:
            self.assertGreater(record.getAttribute(self.siteIdKey), params.SITE_ID)

    def testGetDataWithLessThan(self):
        geomData = self._runConstraintTest(self.siteIdKey, '<', params.SITE_ID)
        for record in geomData:
            self.assertLess(record.getAttribute(self.siteIdKey), params.SITE_ID)

    def testGetDataWithGreaterThanEquals(self):
        geomData = self._runConstraintTest(self.siteIdKey, '>=', params.SITE_ID)
        for record in geomData:
            self.assertGreaterEqual(record.getAttribute(self.siteIdKey), params.SITE_ID)

    def testGetDataWithLessThanEquals(self):
        geomData = self._runConstraintTest(self.siteIdKey, '<=', params.SITE_ID)
        for record in geomData:
            self.assertLessEqual(record.getAttribute(self.siteIdKey), params.SITE_ID)

    def testGetDataWithInTuple(self):
        collection = (params.SITE_ID,)
        geomData = self._runConstraintTest(self.siteIdKey, 'in', collection)
        for record in geomData:
            self.assertIn(record.getAttribute(self.siteIdKey), collection)

    def testGetDataWithInList(self):
        collection = [params.SITE_ID,]
        geomData = self._runConstraintTest(self.siteIdKey, 'in', collection)
        for record in geomData:
            self.assertIn(record.getAttribute(self.siteIdKey), collection)

    def testGetDataWithInGenerator(self):
        collection = (params.SITE_ID,)
        generator = (item for item in collection)
        geomData = self._runConstraintTest(self.siteIdKey, 'in', generator)
        for record in geomData:
            self.assertIn(record.getAttribute(self.siteIdKey), collection)

    def testGetDataWithNotInList(self):
        collection = [params.SITE_ID,]
        geomData = self._runConstraintTest(self.siteIdKey, 'not in', collection)
        for record in geomData:
            self.assertNotIn(record.getAttribute(self.siteIdKey), collection)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest(self.siteIdKey, 'junk', params.SITE_ID)

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest(self.siteIdKey, '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest(self.siteIdKey, 'in', [])
