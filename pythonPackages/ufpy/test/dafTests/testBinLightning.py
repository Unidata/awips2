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
import dafTestsArgsUtil
import sys
import unittest

class BinLightningTestCase(baseDafTestCase.DafTestCase):
    """
    Tests that binlightning data can be retrieved through the DAF, simply
    ensuring that no unexpected exceptions are thrown while retrieving it and
    that the returned data is not None.
    """

    datatype = "binlightning"

    params = "intensity"

    @classmethod
    def setUpClass(cls):
        print("STARTING BINLIGHTNING TESTS\n\n")

    def testParameters(self):
        req = DAL.newDataRequest(self.datatype)

        self.runParametersTest(req) 

    def testTimes(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("source", "NLDN")

        self.runTimesTest(req)

    def testGeometryData(self):
        req = DAL.newDataRequest(self.datatype)
        req.addIdentifier("source", "NLDN")
        req.setParameters(*self.params.split(','))

        # Limit the times in the geometry data test to limit the amount of data
        # returned.
        self.runGeometryDataTest(req, limitTimes=True)

    @classmethod
    def tearDownClass(cls):
        print("BINLIGHTNING TESTS COMPLETE\n\n\n")

def getArgs():
    parser = dafTestsArgsUtil.getParser()
    parser.add_argument("-p", action="store", dest="params", default=BinLightningTestCase.params,
                         help="Lightning parameters comma separated",
                         metavar="parameters")
    return parser.parse_args()

if __name__ == '__main__':
    args = getArgs()
    dafTestsArgsUtil.handleArgs(args)
    BinLightningTestCase.params = args.params
    unittest.main(argv=sys.argv[:1])
