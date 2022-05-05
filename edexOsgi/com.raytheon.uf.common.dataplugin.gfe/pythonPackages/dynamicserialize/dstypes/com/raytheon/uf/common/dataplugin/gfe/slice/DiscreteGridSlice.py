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
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Jan 04, 2018  7178     randerso  Changed getKey() and setKey() to getKeys()
#                                  and setKeys()
#
# File auto-generated against equivalent DynamicSerialize Java class

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.slice import AbstractGridSlice


class DiscreteGridSlice(AbstractGridSlice):

    def __init__(self):
        super(DiscreteGridSlice, self).__init__()
        self.discreteGrid = None
        self.keys = []

    def getDiscreteGrid(self):
        return self.discreteGrid

    def setDiscreteGrid(self, discreteGrid):
        self.discreteGrid = discreteGrid
        
    def getNumPyGrid(self):
        return (self.discreteGrid.getNumPyGrid(), self.key)

    def getKeys(self):
        return self.keys

    def setKeys(self, keys):
        self.keys = keys
