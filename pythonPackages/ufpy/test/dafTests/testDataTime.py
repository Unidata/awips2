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

from dynamicserialize.dstypes.com.raytheon.uf.common.time import DataTime

import unittest

#
# Unit tests for Python implementation of RequestConstraint
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/02/16        2416          tgurney        Initial creation
#
#


class DataTimeTestCase(unittest.TestCase):

    def testFromStrRefTimeOnly(self):
        s = '2016-08-02 01:23:45'
        expected = s
        self.assertEqual(expected, str(DataTime(s)))
        s = s.replace(' ', '_')
        self.assertEqual(expected, str(DataTime(s)))

    def testFromStrRefTimeOnlyZeroMillis(self):
        s = '2016-08-02 01:23:45.0'
        # result of str() will always drop trailing .0 milliseconds
        expected = '2016-08-02 01:23:45'
        self.assertEqual(expected, str(DataTime(s)))
        s = s.replace(' ', '_')
        self.assertEqual(expected, str(DataTime(s)))

    def testFromStrRefTimeOnlyWithMillis(self):
        s = '2016-08-02 01:23:45.1'
        expected = '2016-08-02 01:23:45.001000'
        self.assertEqual(expected, str(DataTime(s)))
        s = s.replace(' ', '_')
        self.assertEqual(expected, str(DataTime(s)))
    
    def testFromStrWithFcstTimeHr(self):
        s = '2016-08-02 01:23:45 (17)'
        expected = s
        self.assertEqual(expected, str(DataTime(s)))
        s = s.replace(' ', '_')
        self.assertEqual(expected, str(DataTime(s)))
        
    def testFromStrWithFcstTimeHrZeroMillis(self):
        s = '2016-08-02 01:23:45.0 (17)'
        expected = '2016-08-02 01:23:45 (17)'
        self.assertEqual(expected, str(DataTime(s)))
        s = s.replace(' ', '_')
        self.assertEqual(expected, str(DataTime(s)))

    def testFromStrWithFcstTimeHrAndMillis(self):
        s = '2016-08-02 01:23:45.1 (17)'
        expected = '2016-08-02 01:23:45.001000 (17)'
        self.assertEqual(expected, str(DataTime(s)))
        s = s.replace(' ', '_')
        self.assertEqual(expected, str(DataTime(s)))
        
    def testFromStrWithFcstTimeHrMin(self):
        s = '2016-08-02 01:23:45 (17:34)'
        expected = s
        self.assertEqual(expected, str(DataTime(s)))
        s = s.replace(' ', '_')
        self.assertEqual(expected, str(DataTime(s)))

    def testFromStrWithFcstTimeHrMinZeroMillis(self):
        s = '2016-08-02 01:23:45.0 (17:34)'
        expected = '2016-08-02 01:23:45 (17:34)'
        self.assertEqual(expected, str(DataTime(s)))
        s = s.replace(' ', '_')
        self.assertEqual(expected, str(DataTime(s)))
        
    def testFromStrWithPeriod(self):
        s = '2016-08-02 01:23:45[2016-08-02 02:34:45--2016-08-02 03:45:56]'
        expected = s
        self.assertEqual(expected, str(DataTime(s)))
        s = s.replace(' ', '_')
        self.assertEqual(expected, str(DataTime(s)))
    
    def testFromStrWithPeriodZeroMillis(self):
        s = '2016-08-02 01:23:45.0[2016-08-02 02:34:45.0--2016-08-02 03:45:56.0]'
        expected = '2016-08-02 01:23:45[2016-08-02 02:34:45--2016-08-02 03:45:56]'
        self.assertEqual(expected, str(DataTime(s)))
        s = s.replace(' ', '_')
        self.assertEqual(expected, str(DataTime(s)))
    
    def testFromStrWithEverything(self):
        s = '2016-08-02 01:23:45.0_(17:34)[2016-08-02 02:34:45.0--2016-08-02 03:45:56.0]'
        expected = '2016-08-02 01:23:45 (17:34)[2016-08-02 02:34:45--2016-08-02 03:45:56]'
        self.assertEqual(expected, str(DataTime(s)))
        s = s.replace(' ', '_')
        self.assertEqual(expected, str(DataTime(s)))
        
    def testDataTimeReconstructItselfFromString(self):
        times = [
             '2016-08-02 01:23:45',
             '2016-08-02 01:23:45.0',
             '2016-08-02 01:23:45.1',
             '2016-08-02 01:23:45.123000',
             '2016-08-02 01:23:45 (17)',
             '2016-08-02 01:23:45.0 (17)',
             '2016-08-02 01:23:45.1 (17)',
             '2016-08-02 01:23:45 (17:34)',
             '2016-08-02 01:23:45.0 (17:34)',
             '2016-08-02 01:23:45.1 (17:34)',
             '2016-08-02 01:23:45.0[2016-08-02_02:34:45.0--2016-08-02_03:45:56.0]',
             '2016-08-02 01:23:45.0[2016-08-02_02:34:45.123--2016-08-02_03:45:56.456]',
             '2016-08-02 01:23:45.456_(17:34)[2016-08-02_02:34:45.0--2016-08-02_03:45:56.0]'
             ]
        for time in times:
            self.assertEqual(DataTime(time), DataTime(str(DataTime(time))), time)