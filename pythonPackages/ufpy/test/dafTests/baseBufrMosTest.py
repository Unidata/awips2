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

import dafTestsUtil
import unittest

class BaseBufrMosTest(unittest.TestCase):
    """
    Base class for testing that bufrmos data can be retrieved through the DAF.
    """

    def testParameters(self):
        req = DAL.newDataRequest(self.datatype)

        dafTestsUtil.testParameters(req)

    def testLocations(self):
        req = DAL.newDataRequest(self.datatype)

        dafTestsUtil.testLocations(req)

    def testTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames("KOMA")

        dafTestsUtil.testTimes(req)

    def testGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.setLocationNames("KOMA")
        req.setParameters("temperature", "dewpoint")

        dafTestsUtil.testGeometryData(req)
