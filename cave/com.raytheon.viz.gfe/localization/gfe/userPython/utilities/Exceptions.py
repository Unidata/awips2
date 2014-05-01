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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
#    Exceptions  -- library of exceptions for Smart Tools and Scripts
#
# Author: hansen
# ----------------------------------------------------------------------------
########################################################################

import exceptions

class EditActionError(exceptions.Exception):
    def __init__(self, errorType=None, errorInfo=None):
        exceptions.Exception.__init__(self)
        self.__errorType = errorType
        self.__errorInfo = errorInfo

    def errorType(self):
        return self.__errorType
    def errorInfo(self):
        return self.__errorInfo
    def setErrorInfo(self, info):
        self.__errorInfo = info
