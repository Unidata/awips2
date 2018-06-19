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

import sys
from optparse import OptionParser

class UsageOptionParser(OptionParser):
    """
    A subclass of OptionParser that prints that overrides error() to print the
    whole help text, rather than just the usage string.
    """
    def error(self, msg):
        """
        Print the help text and exit.
        """
        self.print_help(sys.stderr)
        sys.stderr.write("\n")
        sys.stderr.write(msg)
        sys.stderr.write("\n")
        sys.exit(2)

