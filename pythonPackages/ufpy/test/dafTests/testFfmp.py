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

from . import baseDafTestCase
from . import params

#
# Test DAF support for ffmp data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#    04/18/16        5587          tgurney        Add test for sane handling of
#                                                 zero records returned
#    06/20/16        5587          tgurney        Add identifier values tests
#    07/01/16        5728          mapeters       Add advanced query tests,
#                                                 include huc and accumHrs in
#                                                 id values tests, test that
#                                                 accumHrs id is never required
#    08/03/16        5728          mapeters       Fixed minor bugs, replaced
#                                                 PRTM parameter since it isn't
#                                                 configured for ev-oma
#    11/08/16        5985          tgurney        Do not check data times
#    12/07/16        5981          tgurney        Parameterize
#    12/20/16        5981          tgurney        Do not check data times
#
#


class FfmpTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for ffmp data"""

    datatype = 'ffmp'
    location = params.RADAR.lower()

    @staticmethod
    def addIdentifiers(req):
        req.addIdentifier('wfo', params.SITE_ID)
        req.addIdentifier('siteKey', 'hpe')
        req.addIdentifier('dataKey', 'hpe')
        req.addIdentifier('huc', 'ALL')

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        self.addIdentifiers(req)
        self.runLocationsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        self.addIdentifiers(req)
        req.setParameters('DHRMOSAIC')
        self.runTimesTest(req)

    def testGetGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        self.addIdentifiers(req)
        req.setParameters('DHRMOSAIC')
        self.runGeometryDataTest(req, checkDataTimes=False)

    def testGetGeometryDataEmptyResult(self):
        req = DAL.newDataRequest(self.datatype)
        self.addIdentifiers(req)
        req.setParameters('blah blah blah') # force 0 records returned
        result = self.runGeometryDataTest(req, checkDataTimes=False)
        self.assertEqual(len(result), 0)

    def testGetIdentifierValues(self):
        req = DAL.newDataRequest(self.datatype)
        optionalIds = set(DAL.getOptionalIdentifiers(req))
        requiredIds = set(DAL.getRequiredIdentifiers(req))
        ids = requiredIds | optionalIds
        for id in ids:
            req = DAL.newDataRequest(self.datatype)
            if id == 'accumHrs':
                req.setParameters('ARI6H2YR')
                req.addIdentifier('wfo', params.SITE_ID)
                req.addIdentifier('siteKey', self.location)
                req.addIdentifier('huc', 'ALL')
            idValues = DAL.getIdentifierValues(req, id)
            self.assertTrue(hasattr(idValues, '__iter__'))
            print(id + " values: " + str(idValues))

    def testGetInvalidIdentifierValuesThrowsException(self):
        self.runInvalidIdValuesTest()

    def testGetNonexistentIdentifierValuesThrowsException(self):
        self.runNonexistentIdValuesTest()

    def _runConstraintTest(self, key, operator, value):
        req = DAL.newDataRequest(self.datatype)
        constraint = RequestConstraint.new(operator, value)
        req.addIdentifier(key, constraint)
        req.addIdentifier('wfo', params.SITE_ID)
        req.addIdentifier('huc', 'ALL')
        req.setParameters('QPFSCAN')
        return self.runGeometryDataTest(req, checkDataTimes=False)

    def testGetDataWithEqualsString(self):
        geometryData = self._runConstraintTest('siteKey', '=', self.location)
        for record in geometryData:
            self.assertEqual(record.getAttribute('siteKey'), self.location)

    # No numeric tests since no numeric identifiers are available that support
    # RequestConstraints.

    def testGetDataWithEqualsNone(self):
        geometryData = self._runConstraintTest('siteKey', '=', None)
        for record in geometryData:
            self.assertIsNone(record.getAttribute('siteKey'))

    def testGetDataWithNotEquals(self):
        geometryData = self._runConstraintTest('siteKey', '!=', self.location)
        for record in geometryData:
            self.assertNotEqual(record.getAttribute('siteKey'), self.location)

    def testGetDataWithNotEqualsNone(self):
        geometryData = self._runConstraintTest('siteKey', '!=', None)
        for record in geometryData:
            self.assertIsNotNone(record.getAttribute('siteKey'))

    def testGetDataWithGreaterThan(self):
        geometryData = self._runConstraintTest('siteKey', '>', self.location)
        for record in geometryData:
            self.assertGreater(record.getAttribute('siteKey'), self.location)

    def testGetDataWithLessThan(self):
        geometryData = self._runConstraintTest('siteKey', '<', self.location)
        for record in geometryData:
            self.assertLess(record.getAttribute('siteKey'), self.location)

    def testGetDataWithGreaterThanEquals(self):
        geometryData = self._runConstraintTest('siteKey', '>=', self.location)
        for record in geometryData:
            self.assertGreaterEqual(record.getAttribute('siteKey'), self.location)

    def testGetDataWithLessThanEquals(self):
        geometryData = self._runConstraintTest('siteKey', '<=', self.location)
        for record in geometryData:
            self.assertLessEqual(record.getAttribute('siteKey'), self.location)

    def testGetDataWithInList(self):
        collection = [self.location, 'kuex']
        geometryData = self._runConstraintTest('siteKey', 'in', collection)
        for record in geometryData:
            self.assertIn(record.getAttribute('siteKey'), collection)

    def testGetDataWithNotInList(self):
        collection = [self.location, 'kuex']
        geometryData = self._runConstraintTest('siteKey', 'not in', collection)
        for record in geometryData:
            self.assertNotIn(record.getAttribute('siteKey'), collection)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('siteKey', 'junk', self.location)

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('siteKey', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('siteKey', 'in', [])

    def testGetDataWithSiteKeyAndDataKeyConstraints(self):
        siteKeys = [self.location, 'hpe']
        dataKeys = ['kuex', 'kdmx']

        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('wfo', params.SITE_ID)
        req.addIdentifier('huc', 'ALL')

        siteKeysConstraint = RequestConstraint.new('in', siteKeys)
        req.addIdentifier('siteKey', siteKeysConstraint)
        dataKeysConstraint = RequestConstraint.new('in', dataKeys)
        req.addIdentifier('dataKey', dataKeysConstraint)

        req.setParameters('QPFSCAN')
        geometryData = self.runGeometryDataTest(req, checkDataTimes=False)
        for record in geometryData:
            self.assertIn(record.getAttribute('siteKey'), siteKeys)
            # dataKey attr. is comma-separated list of dataKeys that had data
            for dataKey in record.getAttribute('dataKey').split(','):
                self.assertIn(dataKey, dataKeys)

    def testGetGuidanceDataWithoutAccumHrsIdentifierSet(self):
        # Test that accumHrs identifier is not required for guidance data
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('wfo', params.SITE_ID)
        req.addIdentifier('siteKey', self.location)
        req.addIdentifier('huc', 'ALL')
        req.setParameters('FFG0124hr')
        self.runGeometryDataTest(req, checkDataTimes=False)
