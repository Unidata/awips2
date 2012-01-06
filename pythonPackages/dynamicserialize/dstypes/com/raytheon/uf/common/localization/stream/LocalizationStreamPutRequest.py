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
import uuid
from dynamicserialize.dstypes.com.raytheon.uf.common.localization.stream import AbstractLocalizationStreamRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.plugin.nwsauth.user import User


class LocalizationStreamPutRequest(AbstractLocalizationStreamRequest):

    def __init__(self):
        super(LocalizationStreamPutRequest, self).__init__()
        self.id = str(uuid.uuid4())
        self.bytes = None
        self.end = None
        self.offset = None
        self.localizedSite = None

    def getId(self):
        return self.id

    def setId(self, id):
        self.id = id

    def getBytes(self):
        return self.bytes

    def setBytes(self, bytes):
        self.bytes = bytes

    def getEnd(self):
        return self.end

    def setEnd(self, end):
        self.end = end

    def getOffset(self):
        return self.offset

    def setOffset(self, offset):
        self.offset = offset

    def getLocalizedSite(self):
        return self.localizedSite

    def setLocalizedSite(self, localizedSite):
        self.localizedSite = localizedSite

