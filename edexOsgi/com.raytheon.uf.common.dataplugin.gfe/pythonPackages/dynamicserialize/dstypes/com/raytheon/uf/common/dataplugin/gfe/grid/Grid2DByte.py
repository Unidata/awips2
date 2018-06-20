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

import numpy


class Grid2DByte(object):

    def __init__(self):
        self.buffer = None
        self.xdim = None
        self.ydim = None

    def getBuffer(self):
        return self.buffer

    def setBuffer(self, buffer):
        self.buffer = buffer

    def getXdim(self):
        return self.xdim

    def setXdim(self, xdim):
        self.xdim = xdim

    def getYdim(self):
        return self.ydim

    def setYdim(self, ydim):
        self.ydim = ydim
        
    def getNumPyGrid(self):
        return numpy.resize(self.buffer, (self.xdim, self.ydim))

