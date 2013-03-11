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

import argparse
import sys

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID


class UsageArgumentParser(argparse.ArgumentParser):
    """
    A subclass of ArgumentParser that overrides error() to print the
    whole help text, rather than just the usage string.
    """
    def error(self, message):
        sys.stderr.write('%s: error: %s\n' % (self.prog, message))
        self.print_help()
        sys.exit(2)

## Custom actions for ArgumentParser objects ##
class StoreDatabaseIDAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        did = DatabaseID(values)
        if did.isValid():
            setattr(namespace, self.dest, did)
        else:
            parser.error("DatabaseID [" + values + "] not a valid identifier")

class AppendParmNameAndLevelAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        tx = ParmID.parmNameAndLevel(values)
        comp = tx[0] + '_' + tx[1]
        if (hasattr(namespace, self.dest)) and \
            (getattr(namespace, self.dest) is not None):
                currentValues = getattr(namespace, self.dest)
                currentValues.append(comp)
                setattr(namespace, self.dest, currentValues)
        else:
            setattr(namespace, self.dest, [comp])

