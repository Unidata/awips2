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

# This class is a dummy implementation of the 
# com.vividsolutions.jts.geom.Envelope class. It was simply created to allow
# serialization/deserialization of IDataRequest objects from the Data Access
# Framework. This should be re-implemented if useful work needs to be 
# performed against serialized Envelope objects.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/29/13         2023         dgilling       Initial Creation.
#
#

class Envelope(object):

    def __init__(self, env=None):
        self.maxx = -1.0
        self.maxy = -1.0
        self.minx = 0.0
        self.miny = 0.0
        if env is not None:
            (self.minx, self.miny, self.maxx, self.maxy) = env.bounds

    def getMaxX(self):
        return self.maxx

    def getMaxY(self):
        return self.maxy

    def getMinX(self):
        return self.minx

    def getMinY(self):
        return self.miny

    def setMaxX(self, value):
        self.maxx = value

    def setMaxY(self, value):
        self.maxy = value

    def setMinX(self, value):
        self.minx = value

    def setMinY(self, value):
        self.miny = value

