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

#
# Provides convenience methods for redirecting stdout and stderr to logging
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/12/11                      randerso       Initial Creation.
#    
# 
#

origout = sys.stdout
origerr = sys.stderr

def redirect(logger, stdout=False, stderr=False):
    """
        Optionally redirect stdout and/or stderr to logger
    """
    if stdout:
        sys.stdout = StdOutWrapper(logger)
        
    if stderr:
        sys.stderr = StdErrWrapper(logger)

def restore():
    """
        Restore stderr and stdout to their original values
    """
    sys.stdout = origout
    sys.stderr = origerr
    
class StdOutWrapper:
    """
        Call wrapper for stdout
    """
    def __init__(self, logger):
        self.__logger = logger
        
    def write(self, s):
        if (s.endswith('\n')):
            s=s[:-1]
            if (len(s) == 0):
                return
        self.__logger.info("stdout: "+s)

class StdErrWrapper:
    """
        Call wrapper for stderr
    """
    def __init__(self, logger):
        self.__logger = logger
        
    def write(self, s):
        if (s.endswith('\n')):
            s=s[:-1]
            if (len(s) == 0):
                return
        self.__logger.error("stderr: "+s)
