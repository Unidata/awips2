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

import dafTestsUtil
import os
import sys
import unittest

#
# Framework for auto-detecting and running all DAF unit tests (test*.py) in 
# the same directory as this file.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    
# 
#

def load_tests(loader, tests, pattern):
    testDir = os.path.dirname(os.path.realpath(__file__))
    return loader.discover(start_dir=testDir)

if __name__ == "__main__":
    dafTestsUtil.parseAndHandleArgs()
    unittest.main(argv=sys.argv[:1])
