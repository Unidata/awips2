#!/usr/bin/env python
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
# Evaluator.py
# Class for evaluating GFE expressions
#
# Author: hansen
# ----------------------------------------------------------------------------
import Query, DBSSClient, numpy
import AbsTime

class Evaluator:
    def __init__(self, dataMgr):
        self.__gloc = dataMgr.getParmManager().compositeGridLocation()
        self._query = Query.Query(DBSSClient.DBSSClient(dataMgr))
        self.__dm = dataMgr
        d = self.__gloc.gridSize()
        self._shape = (d.y, d.x)

    def gloc(self):
        return self.__gloc

    def evaluate(self, expression, timeInfluence=None):
        # if no expression, return the empty ref set
        if len(expression) == 0:
            return self.__dm.getRefManager().emptyRefSet()

        if timeInfluence is None:
            timeInfluence = self.__dm.getSpatialDisplayManager().getSpatialEditorTime()
        
        if not isinstance(timeInfluence, AbsTime.AbsTime):
            timeInfluence = AbsTime.AbsTime(timeInfluence)
        
        self._query.setTime(timeInfluence.unixTime())
        grid = self._query.eval(expression)
        if type(grid) != type(numpy.array([])) or grid.shape != self._shape:
            raise TypeError("query did not eval to a grid of shape: "
                            + `self._shape`)

        from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData, ReferenceID
        from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DBit        

        if grid.dtype == 'bool':
            grid = numpy.array(grid, 'byte') 
        bits = Grid2DBit.createBitGrid(grid.shape[1], grid.shape[0], grid)  
        return ReferenceData(self.gloc(),
                             ReferenceID(expression), 
                             expression, bits)
    
    def willRecurse(self, name, str):
        return self._query.willRecurse(name, str)                                            

    def cleanUp(self):
        pass
