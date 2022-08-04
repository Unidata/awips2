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


from ufpy.dataaccess import DataAccessLayer as DAL
from ufpy.ThriftClient import ThriftRequestException

from . import baseDafTestCase
from . import params

#
# Tests common to all radar factories
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
#    06/30/16        5725          tgurney        Add test for NOT IN
#    08/25/16        2671          tgurney        Rename to baseRadarTestCase
#                                                 and move factory-specific
#                                                 tests
#    12/07/16        5981          tgurney        Parameterize
#
#


class BaseRadarTestCase(baseDafTestCase.DafTestCase):
    """Tests common to all radar factories"""

    # datatype is specified by subclass
    datatype = None

    radarLoc = params.RADAR.lower()

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
        req.setEnvelope(params.ENVELOPE)
        self.runTimesTest(req)

    def testGetIdentifierValues(self):
        req = DAL.newDataRequest(self.datatype)
        optionalIds = set(DAL.getOptionalIdentifiers(req))
        requiredIds = set(DAL.getRequiredIdentifiers(req))
        self.runGetIdValuesTest(optionalIds | requiredIds)

    def testGetInvalidIdentifierValuesThrowsException(self):
        self.runInvalidIdValuesTest()

    def testGetNonexistentIdentifierValuesThrowsException(self):
        self.runNonexistentIdValuesTest()

    def runConstraintTest(self, key, operator, value):
        raise NotImplementedError

    def testGetDataWithEqualsString(self):
        gridData = self.runConstraintTest('icao', '=', self.radarLoc)
        for record in gridData:
            self.assertEqual(record.getAttribute('icao'), self.radarLoc)

    def testGetDataWithEqualsInt(self):
        gridData = self.runConstraintTest('icao', '=', 1000)
        for record in gridData:
            self.assertEqual(record.getAttribute('icao'), 1000)

    def testGetDataWithEqualsFloat(self):
        gridData = self.runConstraintTest('icao', '=', 1.0)
        for record in gridData:
            self.assertEqual(round(record.getAttribute('icao'), 1), 1.0)

    def testGetDataWithEqualsNone(self):
        gridData = self.runConstraintTest('icao', '=', None)
        for record in gridData:
            self.assertIsNone(record.getAttribute('icao'))

    def testGetDataWithNotEquals(self):
        gridData = self.runConstraintTest('icao', '!=', self.radarLoc)
        for record in gridData:
            self.assertNotEqual(record.getAttribute('icao'), self.radarLoc)

    def testGetDataWithNotEqualsNone(self):
        gridData = self.runConstraintTest('icao', '!=', None)
        for record in gridData:
            self.assertIsNotNone(record.getAttribute('icao'))

    def testGetDataWithGreaterThan(self):
        gridData = self.runConstraintTest('icao', '>', self.radarLoc)
        for record in gridData:
            self.assertGreater(record.getAttribute('icao'), self.radarLoc)

    def testGetDataWithLessThan(self):
        gridData = self.runConstraintTest('icao', '<', self.radarLoc)
        for record in gridData:
            self.assertLess(record.getAttribute('icao'), self.radarLoc)

    def testGetDataWithGreaterThanEquals(self):
        gridData = self.runConstraintTest('icao', '>=', self.radarLoc)
        for record in gridData:
            self.assertGreaterEqual(record.getAttribute('icao'), self.radarLoc)

    def testGetDataWithLessThanEquals(self):
        gridData = self.runConstraintTest('icao', '<=', self.radarLoc)
        for record in gridData:
            self.assertLessEqual(record.getAttribute('icao'), self.radarLoc)

    def testGetDataWithInTuple(self):
        gridData = self.runConstraintTest('icao', 'in', (self.radarLoc, 'tpbi'))
        for record in gridData:
            self.assertIn(record.getAttribute('icao'), (self.radarLoc, 'tpbi'))

    def testGetDataWithInList(self):
        gridData = self.runConstraintTest('icao', 'in', [self.radarLoc, 'tpbi'])
        for record in gridData:
            self.assertIn(record.getAttribute('icao'), (self.radarLoc, 'tpbi'))

    def testGetDataWithInGenerator(self):
        generator = (item for item in (self.radarLoc, 'tpbi'))
        gridData = self.runConstraintTest('icao', 'in', generator)
        for record in gridData:
            self.assertIn(record.getAttribute('icao'), (self.radarLoc, 'tpbi'))

    def testGetDataWithNotInList(self):
        gridData = self.runConstraintTest('icao', 'not in', ['zzzz', self.radarLoc])
        for record in gridData:
            self.assertNotIn(record.getAttribute('icao'), ('zzzz', self.radarLoc))

    def testGetDataWithInvalidConstraintTypeThrowsException(self):
        with self.assertRaises(ValueError):
            self.runConstraintTest('icao', 'junk', self.radarLoc)

    def testGetDataWithInvalidConstraintValueThrowsException(self):
        with self.assertRaises(TypeError):
            self.runConstraintTest('icao', '=', {})

    def testGetDataWithEmptyInConstraintThrowsException(self):
        with self.assertRaises(ValueError):
            self.runConstraintTest('icao', 'in', [])
