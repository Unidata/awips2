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
from shapely.geometry import box

import baseDafTestCase
import params
import unittest

#
# Base TestCase for BufrMos* tests.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    12/07/16        5981          tgurney        Parameterize
#    12/15/16        5981          tgurney        Add envelope test
#
#


class BufrMosTestCase(baseDafTestCase.DafTestCase):
    """Base class for testing DAF support of bufrmos data"""
    
    data_params = "temperature", "dewpoint"
    
    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        self.runLocationsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames(params.OBS_STATION)
        self.runTimesTest(req)

    def testGetGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames(params.OBS_STATION)
        req.setParameters(*self.data_params)
        self.runGeometryDataTest(req)

    def testGetGeometryDataWithEnvelope(self):
        req = DAL.newDataRequest(self.datatype)
        req.setParameters(*self.data_params)
        req.setEnvelope(params.ENVELOPE)
        data = self.runGeometryDataTest(req)
        for item in data:
            self.assertTrue(params.ENVELOPE.contains(item.getGeometry()))
