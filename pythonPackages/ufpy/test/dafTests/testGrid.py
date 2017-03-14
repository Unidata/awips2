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
from shapely.geometry import box, Point
from ufpy.dataaccess import DataAccessLayer as DAL

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
#    10/13/16        5942          bsteffen       Test envelopes
#    11/08/16        5985          tgurney        Skip certain tests when no
#                                                 data is available
#


class GridTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for grid data"""

    datatype = "grid"

    model = "GFS160"

    envelope = box(-97.0, 41.0, -96.0, 42.0)

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("info.datasetId", self.model)
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("info.datasetId", self.model)
        self.runLocationsTest(req)

    def testGetAvailableLevels(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("info.datasetId", self.model)
        self.runLevelsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("info.datasetId", self.model)
        req.setLevels("2FHAG")
        self.runTimesTest(req)

    def testGetGridData(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("info.datasetId", self.model)
        req.setLevels("2FHAG")
        req.setParameters("T")
        self.runGridDataTest(req)

    def testGetIdentifierValues(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("info.datasetId", 'ENSEMBLE')
        req.setLevels("2FHAG")
        req.setParameters("T")
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

