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
from shapely.geometry import box, Point
from awips.dataaccess import DataAccessLayer as DAL
from awips.ThriftClient import ThriftRequestException

import baseDafTestCase
import unittest

#
# Test DAF support for grid data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#    06/09/16        5587          tgurney        Typo in id values test
#    07/06/16        5728          mapeters       Add advanced query tests
#    08/03/16        5728          mapeters       Add additional identifiers to testGetDataWith*
#                                                 tests to shorten run time and prevent EOFError
#    10/13/16        5942          bsteffen       Test envelopes
#    11/08/16        5985          tgurney        Skip certain tests when no
#                                                 data is available
#


class GridTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for grid data"""

    datatype = 'grid'

    model = 'GFS160'

    envelope = box(-97.0, 41.0, -96.0, 42.0)

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('info.datasetId', self.model)
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('info.datasetId', self.model)
        self.runLocationsTest(req)

    def testGetAvailableLevels(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('info.datasetId', self.model)
        self.runLevelsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('info.datasetId', self.model)
        req.setLevels('2FHAG')
        self.runTimesTest(req)

    def testGetGridData(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('info.datasetId', self.model)
        req.setLevels('2FHAG')
        req.setParameters('T')
        self.runGridDataTest(req)

    def testGetIdentifierValues(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('info.datasetId', 'ENSEMBLE')
        req.setLevels('2FHAG')
        req.setParameters('T')
        idValues = DAL.getIdentifierValues(req, 'info.ensembleId')
        self.assertTrue(hasattr(idValues, '__iter__'))
        if idValues:
            self.assertIn('ctl1', idValues)
            self.assertIn('p1', idValues)
            self.assertIn('n1', idValues)
        else:
            raise unittest.SkipTest("no data available")

    def testGetInvalidIdentifierValuesThrowsException(self):
        self.runInvalidIdValuesTest()

    def testGetNonexistentIdentifierValuesThrowsException(self):
        self.runNonexistentIdValuesTest()

        
    def testGetDataWithEnvelope(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('info.datasetId', self.model)
        req.setLevels('2FHAG')
        req.setParameters('T')
        req.setEnvelope(self.envelope)
        gridData = self.runGridDataTest(req)
        if not gridData:
            raise unittest.SkipTest('no data available')
        lons, lats = gridData[0].getLatLonCoords()
        lons = lons.reshape(-1)
        lats = lats.reshape(-1)
        
        # Ensure all points are within one degree of the original box
        # to allow slight margin of error for reprojection distortion.
        testEnv = box(self.envelope.bounds[0] - 1, self.envelope.bounds[1] - 1,
                      self.envelope.bounds[2] + 1, self.envelope.bounds[3] + 1 )
        
        for i in range(len(lons)):
            self.assertTrue(testEnv.contains(Point(lons[i], lats[i])))


    def _runConstraintTest(self, key, operator, value):
        req = DAL.newDataRequest(self.datatype)
        constraint = RequestConstraint.new(operator, value)
        req.addIdentifier(key, constraint)
        req.addIdentifier('info.datasetId', self.model)
        req.addIdentifier('info.level.masterLevel.name', 'FHAG')
        req.addIdentifier('info.level.leveltwovalue', 3000.0)
        req.setParameters('T')
        return self.runGridDataTest(req)

    def testGetDataWithEqualsString(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '=', '2000.0')
        for record in gridData:
            self.assertEqual(record.getAttribute('info.level.levelonevalue'), 2000.0)

    def testGetDataWithEqualsUnicode(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '=', u'2000.0')
        for record in gridData:
            self.assertEqual(record.getAttribute('info.level.levelonevalue'), 2000.0)

    def testGetDataWithEqualsInt(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '=', 2000)
        for record in gridData:
            self.assertEqual(record.getAttribute('info.level.levelonevalue'), 2000)

    def testGetDataWithEqualsLong(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '=', 2000L)
        for record in gridData:
            self.assertEqual(record.getAttribute('info.level.levelonevalue'), 2000)

    def testGetDataWithEqualsFloat(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '=', 2000.0)
        for record in gridData:
            self.assertEqual(round(record.getAttribute('info.level.levelonevalue'), 1), 2000.0)

    def testGetDataWithEqualsNone(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '=', None)
        for record in gridData:
            self.assertIsNone(record.getAttribute('info.level.levelonevalue'))

    def testGetDataWithNotEquals(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '!=', 2000.0)
        for record in gridData:
            self.assertNotEqual(record.getAttribute('info.level.levelonevalue'), 2000.0)

    def testGetDataWithNotEqualsNone(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '!=', None)
        for record in gridData:
            self.assertIsNotNone(record.getAttribute('info.level.levelonevalue'))

    def testGetDataWithGreaterThan(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '>', 2000.0)
        for record in gridData:
            self.assertGreater(record.getAttribute('info.level.levelonevalue'), 2000.0)

    def testGetDataWithLessThan(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '<', 2000.0)
        for record in gridData:
            self.assertLess(record.getAttribute('info.level.levelonevalue'), 2000.0)

    def testGetDataWithGreaterThanEquals(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '>=', 2000.0)
        for record in gridData:
            self.assertGreaterEqual(record.getAttribute('info.level.levelonevalue'), 2000.0)

    def testGetDataWithLessThanEquals(self):
        gridData = self._runConstraintTest('info.level.levelonevalue', '<=', 2000.0)
        for record in gridData:
            self.assertLessEqual(record.getAttribute('info.level.levelonevalue'), 2000.0)

    def testGetDataWithInList(self):
        collection = [2000.0, 1000.0]
        gridData = self._runConstraintTest('info.level.levelonevalue', 'in', collection)
        for record in gridData:
            self.assertIn(record.getAttribute('info.level.levelonevalue'), collection)

    def testGetDataWithNotInList(self):
        collection = [2000.0, 1000.0]
        gridData = self._runConstraintTest('info.level.levelonevalue', 'not in', collection)
        for record in gridData:
            self.assertNotIn(record.getAttribute('info.level.levelonevalue'), collection)

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('info.level.levelonevalue', 'junk', '2000.0')

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self._runConstraintTest('info.level.levelonevalue', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self._runConstraintTest('info.level.levelonevalue', 'in', [])

    def testGetDataWithLevelOneAndLevelTwoConstraints(self):
        req = DAL.newDataRequest(self.datatype)
        levelOneConstraint = RequestConstraint.new('>=', 2000.0)
        req.addIdentifier('info.level.levelonevalue', levelOneConstraint)
        levelTwoConstraint = RequestConstraint.new('in', (4000.0, 5000.0))
        req.addIdentifier('info.level.leveltwovalue', levelTwoConstraint)
        req.addIdentifier('info.datasetId', self.model)
        req.addIdentifier('info.level.masterLevel.name', 'FHAG')
        req.setParameters('T')
        gridData = self.runGridDataTest(req)
        for record in gridData:
            self.assertGreaterEqual(record.getAttribute('info.level.levelonevalue'), 2000.0)
            self.assertIn(record.getAttribute('info.level.leveltwovalue'), (4000.0, 5000.0))

    def testGetDataWithMasterLevelNameInConstraint(self):
        req = DAL.newDataRequest(self.datatype)
        masterLevelConstraint = RequestConstraint.new('in', ('FHAG', 'K'))
        req.addIdentifier('info.level.masterLevel.name', masterLevelConstraint)
        req.addIdentifier('info.level.levelonevalue', 2000.0)
        req.addIdentifier('info.level.leveltwovalue', 3000.0)
        req.addIdentifier('info.datasetId', 'GFS160')
        req.setParameters('T')
        gridData = self.runGridDataTest(req)
        for record in gridData:
            self.assertIn(record.getAttribute('info.level.masterLevel.name'), ('FHAG', 'K'))

    def testGetDataWithDatasetIdInConstraint(self):
        req = DAL.newDataRequest(self.datatype)
        # gfs160 is alias for GFS160 in this namespace
        req.addIdentifier('namespace', 'gfeParamInfo')
        datasetIdConstraint = RequestConstraint.new('in', ('gfs160', 'HRRR'))
        req.addIdentifier('info.datasetId', datasetIdConstraint)
        req.addIdentifier('info.level.masterLevel.name', 'FHAG')
        req.addIdentifier('info.level.levelonevalue', 2000.0)
        req.addIdentifier('info.level.leveltwovalue', 3000.0)
        req.setParameters('T')
        gridData = self.runGridDataTest(req, testSameShape=False)
        for record in gridData:
            self.assertIn(record.getAttribute('info.datasetId'), ('gfs160', 'HRRR'))

    def testGetDataWithMasterLevelNameLessThanEqualsConstraint(self):
        req = DAL.newDataRequest(self.datatype)
        masterLevelConstraint = RequestConstraint.new('<=', 'K')
        req.addIdentifier('info.level.masterLevel.name', masterLevelConstraint)
        req.addIdentifier('info.level.levelonevalue', 2000.0)
        req.addIdentifier('info.level.leveltwovalue', 3000.0)
        req.addIdentifier('info.datasetId', 'GFS160')
        req.setParameters('T')
        gridData = self.runGridDataTest(req)
        for record in gridData:
            self.assertLessEqual(record.getAttribute('info.level.masterLevel.name'), 'K')

    def testGetDataWithComplexConstraintAndNamespaceThrowsException(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier('namespace', 'grib')
        masterLevelConstraint = RequestConstraint.new('<=', 'K')
        req.addIdentifier('info.level.masterLevel.name', masterLevelConstraint)
        req.addIdentifier('info.datasetId', 'GFS160')
        req.setParameters('T')
        with self.assertRaises(ThriftRequestException) as cm:
            self.runGridDataTest(req)
        self.assertIn('IncompatibleRequestException', str(cm.exception))
        self.assertIn('info.level.masterLevel.name', str(cm.exception))