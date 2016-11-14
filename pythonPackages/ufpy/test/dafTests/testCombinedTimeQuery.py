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

from ufpy.dataaccess import CombinedTimeQuery as CTQ

import unittest
import os

#
# Test the CombinedTimedQuery module
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/24/16        5591          bsteffen       Initial Creation.
#
#
#

class CombinedTimeQueryTestCase(unittest.TestCase):

    @classmethod
    def setUp(cls):
        host = os.environ.get('DAF_TEST_HOST')
        if host is None:
            host = 'localhost'
        DAL.changeEDEXHost(host)

    def testSuccessfulQuery(self):
        req = DAL.newDataRequest('grid')
        req.setLocationNames('GFS160')
        req.setParameters('T','GH')
        req.setLevels('300MB', '500MB','700MB')
        times = CTQ.getAvailableTimes(req);
        self.assertNotEqual(len(times), 0)
    
    def testNonIntersectingQuery(self):
        """
        Test that when a parameter is only available on one of the levels that no times are returned.
        """
        req = DAL.newDataRequest('grid')
        req.setLocationNames('GFS160')
        req.setParameters('T','GH', 'TP6hr')
        req.setLevels('300MB', '500MB','700MB','0.0SFC')
        times = CTQ.getAvailableTimes(req);
        self.assertEqual(len(times), 0)