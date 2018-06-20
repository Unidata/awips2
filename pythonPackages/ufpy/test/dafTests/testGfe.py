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
from shapely.geometry import box, Point

import baseDafTestCase
import params
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
#    06/30/16        5725          tgurney        Add test for NOT IN
#    11/07/16        5991          bsteffen       Improve vector tests
#    12/07/16        5981          tgurney        Parameterize
#    12/15/16        6040          tgurney        Add testGetGridDataWithDbType
#    12/20/16        5981          tgurney        Add envelope test
#    10/19/17        6491          tgurney        Add test for dbtype identifier
#    11/10/17        6491          tgurney        Replace modelName with
#                                                 parmId.dbId.modelName
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
        req.addIdentifier('parmId.dbId.modelName', 'Fcst')
        self.runLocationsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('parmId.dbId.modelName', 'Fcst')
        req.addIdentifier('parmId.dbId.siteId', params.SITE_ID)
        self.runTimesTest(req)

    def testGetGridData(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('parmId.dbId.modelName', 'Fcst')
        req.addIdentifier('parmId.dbId.siteId', params.SITE_ID)
        req.setParameters('T')
        gridDatas = self.runGridDataTest(req)
        for gridData in gridDatas:
            self.assertEqual(gridData.getAttribute('parmId.dbId.dbType'), '')

    def testGetGridDataWithEnvelope(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('parmId.dbId.modelName', 'Fcst')
        req.addIdentifier('parmId.dbId.siteId', params.SITE_ID)
        req.setParameters('T')
        req.setEnvelope(params.ENVELOPE)
        gridData = self.runGridDataTest(req)
        if not gridData:
            raise unittest.SkipTest('no data available')
        lons, lats = gridData[0].getLatLonCoords()
        lons = lons.reshape(-1)
        lats = lats.reshape(-1)

        # Ensure all points are within one degree of the original box
        # to allow slight margin of error for reprojection distortion.
        testEnv = box(params.ENVELOPE.bounds[0] - 1, params.ENVELOPE.bounds[1] - 1,
                      params.ENVELOPE.bounds[2] + 1, params.ENVELOPE.bounds[3] + 1 )

        for i in range(len(lons)):
            self.assertTrue(testEnv.contains(Point(lons[i], lats[i])))

    def testGetVectorGridData(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('parmId.dbId.modelName', 'Fcst')
        req.addIdentifier('parmId.dbId.siteId', params.SITE_ID)
        req.setParameters('Wind')
        times = DAL.getAvailableTimes(req)
        if not(times):
            raise unittest.SkipTest('No Wind Data available for testing')
        gridData = DAL.getGridData(req, [times[0]])
        rawWind = None
        rawDir = None
        for grid in gridData:
            if grid.getParameter() == 'Wind':
                self.assertEqual(grid.getUnit(),'kts')
                rawWind = grid.getRawData()
            elif grid.getParameter() == 'WindDirection':
                self.assertEqual(grid.getUnit(),'deg')
                rawDir = grid.getRawData()
        self.assertIsNotNone(rawWind, 'Wind Magnitude grid is not present')
        self.assertIsNotNone(rawDir, 'Wind Direction grid is not present')
        # rawWind and rawDir are numpy.ndarrays so comparison will result in boolean ndarrays.
        self.assertTrue((rawWind >= 0).all(), 'Wind Speed should not contain negative values')
        self.assertTrue((rawDir >= 0).all(), 'Wind Direction should not contain negative values')
        self.assertTrue((rawDir <= 360).all(), 'Wind Direction should be less than or equal to 360')
        self.assertFalse((rawDir == rawWind).all(), 'Wind Direction should be different from Wind Speed')        

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
        req.setLocationNames(params.SITE_ID)
        req.setParameters('T')
        return self.runGridDataTest(req)

    def testGetDataWithModelNameEqualsString(self):
        gridData = self._runConstraintTest('parmId.dbId.modelName', '=', 'Fcst')
        for record in gridData:
            self.assertEqual(record.getAttribute('parmId.dbId.modelName'), 'Fcst')

    def testGetDataWithDbTypeEqualsString(self):
        gridData = self._runConstraintTest('parmId.dbId.dbType', '=', 'Prac')
        for record in gridData:
            self.assertEqual(record.getAttribute('parmId.dbId.dbType'), 'Prac')

    def testGetDataWithEqualsUnicode(self):
        gridData = self._runConstraintTest('parmId.dbId.modelName', '=', u'Fcst')
        for record in gridData:
            self.assertEqual(record.getAttribute('parmId.dbId.modelName'), 'Fcst')

    # No numeric tests since no numeric identifiers are available.

    def testGetDataWithEqualsNone(self):
        gridData = self._runConstraintTest('parmId.dbId.modelName', '=', None)
        for record in gridData:
            self.assertIsNone(record.getAttribute('parmId.dbId.modelName'))

    def testGetDataWithNotEquals(self):
        gridData = self._runConstraintTest('parmId.dbId.modelName', '!=', 'Fcst')
        for record in gridData:
            self.assertNotEqual(record.getAttribute('parmId.dbId.modelName'), 'Fcst')

    def testGetDataWithNotEqualsNone(self):
        gridData = self._runConstraintTest('parmId.dbId.modelName', '!=', None)
        for record in gridData:
            self.assertIsNotNone(record.getAttribute('parmId.dbId.modelName'))

    def testGetDataWithInTuple(self):
        collection = ('Fcst', 'SAT')
        gridData = self._runConstraintTest('parmId.dbId.modelName', 'in', collection)
        for record in gridData:
            self.assertIn(record.getAttribute('parmId.dbId.modelName'), collection)

    def testGetDataWithInList(self):
        collection = ['Fcst', 'SAT']
        gridData = self._runConstraintTest('parmId.dbId.modelName', 'in', collection)
        for record in gridData:
            self.assertIn(record.getAttribute('parmId.dbId.modelName'), collection)

    def testGetDataWithInGenerator(self):
        collection = ('Fcst', 'SAT')
        generator = (item for item in collection)
        gridData = self._runConstraintTest('parmId.dbId.modelName', 'in', generator)
        for record in gridData:
            self.assertIn(record.getAttribute('parmId.dbId.modelName'), collection)

    def testGetDataWithNotInList(self):
        collection = ('Fcst', 'SAT')
        gridData = self._runConstraintTest('parmId.dbId.modelName', 'not in', collection)
        for record in gridData:
            self.assertNotIn(record.getAttribute('parmId.dbId.modelName'), collection)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('parmId.dbId.modelName', 'junk', 'Fcst')

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('parmId.dbId.modelName', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('parmId.dbId.modelName', 'in', [])

