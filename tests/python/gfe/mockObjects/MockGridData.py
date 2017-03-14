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
##
# This is a class meant to act as a placeholder for a Java
# IGridData object in unit testing of HazardUtils.py.
# It has a single field, slice, and a single method, getSlice(),
# which returns whatever was set for the slice in the constructor
# or through direct manipulation of the slice field. 
class MockGridData(object):
    "Crippled substitute for IGridData java class."
    
    def __init__(self, slice=None):
        "Constructor"
        self.slice = slice
        
    def getGridSlice(self):
        "Return the slice field."
        return self.slice
    