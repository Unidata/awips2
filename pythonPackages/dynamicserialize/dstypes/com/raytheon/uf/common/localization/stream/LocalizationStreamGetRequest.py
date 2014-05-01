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

# File auto-generated against equivalent DynamicSerialize Java class

import os
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.stream import AbstractLocalizationStreamRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.plugin.nwsauth.user import User

class LocalizationStreamGetRequest(AbstractLocalizationStreamRequest):

    def __init__(self):
        super(LocalizationStreamGetRequest, self).__init__()
        self.offset = None
        self.numBytes = None

    def getOffset(self):
        return self.offset

    def setOffset(self, offset):
        self.offset = offset

    def getNumBytes(self):
        return self.numBytes

    def setNumBytes(self, numBytes):
        self.numBytes = numBytes

