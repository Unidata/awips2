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

## NOTE: This is a dummy class that is only used for deserialization
## support. Further work required if it is need in the pure Python
## environment.

class Timestamp(object):

    def __init__(self, time=None):
        self.time = time
        
    def getTime(self):
        return self.time
    
    def setTime(self, timeInMillis):
        self.time = timeInMillis
        
    def __str__(self):
        return self.__repr__()
        
    def __repr__(self):
        from time import gmtime, strftime
        
        return strftime("%b %d %y %H:%M:%S GMT", gmtime(self.time/1000.0))
