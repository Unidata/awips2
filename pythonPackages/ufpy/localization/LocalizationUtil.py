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

#
# Utilities for localization.
#
# SOFTWARE HISTORY
#
# Date      Ticket#  Engineer  Description
# --------- -------- --------- --------------------------
# 04/23/19  7756     mapeters  Initial creation

import getpass

def getUser():
    '''
    Get the user context name.
    '''
    try:
        # Match Java's way of determining the user if we have Jep access
        from java.lang import System
        user = System.getProperty('user.name')
    except:
        # Otherwise use built-in getpass module. With IdM, this can return
        # user.name@REALM, so strip the @REALM portion if it exists.
        user = getpass.getuser()
        user = user.split('@')[0]
    return user
