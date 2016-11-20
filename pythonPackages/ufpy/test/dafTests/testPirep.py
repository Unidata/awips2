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

import baseDafTestCase
import unittest

#
# Test DAF support for pirep data
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    04/11/16        5548          tgurney        Cleanup
#    04/18/16        5548          tgurney        More cleanup
#
#


class PirepTestCase(baseDafTestCase.DafTestCase):
    """Test DAF support for pirep data"""

    datatype = "pirep"

    def testGetAvailableParameters(self):
        req = DAL.newDataRequest(self.datatype)
        self.runParametersTest(req)

    def testGetAvailableLocations(self):
        req = DAL.newDataRequest(self.datatype)
        self.runLocationsTest(req)

    def testGetAvailableTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames('OMA')
        self.runTimesTest(req)

    def testGetGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames('OMA')
        req.setParameters("temperature", "windSpeed", "hazardType", "turbType")

        print("Testing getGeometryData()")

        geomData = DAL.getGeometryData(req)
        self.assertIsNotNone(geomData)
        print("Number of geometry records: " + str(len(geomData)))
        print("Sample geometry data:")
        for record in geomData[:self.sampleDataLimit]:
            print("level=", record.getLevel(), end="")
            # One dimensional parameters are reported on the 0.0UNKNOWN level.
            # 2D parameters are reported on MB levels from pressure.
            if record.getLevel() == "0.0UNKNOWN":
                print(" temperature=" + record.getString("temperature") + record.getUnit("temperature"), end="")
                print(" windSpeed=" + record.getString("windSpeed") + record.getUnit("windSpeed"), end="")
            else:
                print(" hazardType=" + record.getString("hazardType"), end="")
                print(" turbType=" + record.getString("turbType"), end="")
            print(" geometry=", record.getGeometry())

        print("getGeometryData() complete\n")

