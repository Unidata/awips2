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

import string, getopt, sys, time, gzip, os, iscTime, stat
import numpy
import LogStream, fcntl

#
# merges two grids and histories together, input gridA is merged into gridB
# result is returned from mergeGrid. Grids are represented in the following
# manner:
# Scalar: (grid, history)
# Vector: ((magGrid, dirGrid), history)
# Weather: ((byteGrid, key), history)
# Discrete: ((byteGrid, key), history)
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/06/09        1995          bphillip       Initial Creation.
#    11/05/13        2517          randerso       Improve memory utilization
#
#
#


class MergeGrid:

    #---------------------------------------------------------------------
    # Constructor
    # Takes creationtime - seconds since Jan 1, 1970, to be used
    #  in the updating of the histories.
    # siteID = site identifier for input grid
    # inFillValue = input fill value indicator
    # outFillValue = output fill value indicator
    # areaMask = numerical mask of areas to merge from grid1 to grid2
    # gridType = 'SCALAR', 'VECTOR', 'WEATHER', 'DISCRETE'
    #---------------------------------------------------------------------
    def __init__(self, creationTime, siteID, inFillValue, outFillValue,
      areaMask, gridType, discreteKeys=None):
        self.__creationTime = creationTime
        self.__siteID = siteID
        self.__inFillV = inFillValue
        self.__outFillV = outFillValue
        self.__areaMask = areaMask
        self.__gridType = gridType
        self.__discreteKeys = discreteKeys



    #---------------------------------------------------------------------
    # find key
    # key = input key
    # keymap = existing key maps (updated on exit)
    # returns the index to use for the key.
    #---------------------------------------------------------------------
    def __findKey(self, key, keyMap):
        try:
            index = keyMap.index(key)
            return index
        except:
            keyMap.append(key)
            return len(keyMap) - 1

    #---------------------------------------------------------------------
    # commonize key
    # wxA = input grid and key
    # wxB = input grid and key
    # returns a tuple (commonkey, gridA, gridB) where gridA and gridB
    #  now use the commonkey
    #---------------------------------------------------------------------
    def __commonizeKey(self, wxA, wxB):
        # make common key and make data changes in B
        gridB = wxB[0]
        key = wxA[1]
        newGrid = numpy.zeros_like(gridB)

        for k in range(len(wxB[1])):
            index = self.__findKey(wxB[1][k], key)
            newGrid[gridB == k] = index

        return (key, wxA[0], newGrid)


    #---------------------------------------------------------------------
    # update history strings
    # historyA = history from input grid (None to delete history entry)
    # historyB = history from base grid, list (None for no old grid.)
    # returns an updated list of strings, each string is an encoded history
    # returns None if no history is present.
    #---------------------------------------------------------------------
    def __updateHistoryStrings(self, historyA, historyB):

        out = []

        # removal any old entry
        if historyB is not None:
            for h in historyB:
                index = string.find(h, ":" + self.__siteID + "_GRID")
                if index == -1:
                    out.append(h)

        # if add mode, add in new entries
        if historyA is not None:
            for h in historyA:
                out.append(h)

        if len(out) > 0:
            return out
        else:
            return None

    #---------------------------------------------------------------------
    # merge scalar grid
    #   Note: gridA can be None, which indicates that the data
    #   is to be blanked out, i.e., made invalid.  gridB can also be
    #   none, which indicates that there is no destination grid and one must
    #   be created.
    #---------------------------------------------------------------------
    def __mergeScalarGrid(self, gridA, gridB):
        if gridA is None and gridB is None:
            return None

        # merge the grids
        if gridA is not None:
            mask = numpy.not_equal(gridA, self.__inFillV)
            numpy.logical_and(mask, self.__areaMask, mask)

            if gridB is None:
                return numpy.where(mask, gridA, self.__outFillV)
            else:
                return numpy.where(mask, gridA, gridB)

        # blank out the data
        else:
            return numpy.where(self.__areaMask, self.__outFillV, gridB)

    #---------------------------------------------------------------------
    # merge vector grid
    #   Note: gridA can be None, which indicates that the data
    #   is to be blanked out, i.e., made invalid.  gridB can also be
    #   none, which indicates that there is no destination grid and one must
    #   be created.
    #---------------------------------------------------------------------
    def __mergeVectorGrid(self, gridA, gridB):
        if gridA is None and gridB is None:
            return None

        # merge the grids
        if gridA is not None:
            mask = numpy.not_equal(gridA[0], self.__inFillV)
            numpy.logical_and(mask, self.__areaMask, mask)

            if gridB is None:
                magGrid = numpy.where(mask, gridA[0], self.__outFillV)
                dirGrid = numpy.where(mask, gridA[1], 0.0)
            else:
                magGrid = numpy.where(mask, gridA[0], gridB[0])
                dirGrid = numpy.where(mask, gridA[1], gridB[1])
            return (magGrid, dirGrid)

        # blank out the data
        else:
            magGrid = numpy.where(self.__areaMask, self.__outFillV, gridB[0])
            dirGrid = numpy.where(self.__areaMask, 0.0, gridB[1])
            return (magGrid, dirGrid)


    #---------------------------------------------------------------------
    # merge weather grid
    #
    # Note the outFillV is ignored for now, all out-of-bounds points will
    # get the <NoWx> value.
    #---------------------------------------------------------------------
    def __mergeWeatherGrid(self, gridA, gridB):

        if gridA is None and gridB is None:
            return None

        noWx = "<NoCov>:<NoWx>:<NoInten>:<NoVis>:"
        # merge the grids
        if gridA is not None:
            mask = numpy.not_equal(gridA[0], self.__inFillV)
            numpy.logical_and(mask, self.__areaMask, mask)

            if gridB is None:   #make an empty grid
                noWxKeys = []
                noWxGrid = numpy.empty_like(gridA[0])
                noWxGrid.fill(self.__findKey(noWx, noWxKeys))
                gridB = (noWxGrid, noWxKeys)
            (commonkey, remapG, dbG) = self.__commonizeKey(gridA, gridB)
            mergedGrid = numpy.where(mask, remapG, dbG)
            return (mergedGrid, commonkey)

        # blank out the data
        else:
            blankGrid = numpy.empty_like(gridB[0])
            blankGrid.fill(self.__findKey(noWx, gridB[1]))
            key = gridB[1]
            grid = numpy.where(self.__areaMask, blankGrid, gridB[0])
            return (grid, key)

    #---------------------------------------------------------------------
    # merge discrete grid
    #
    # Note the outFillV is ignored for now, all out-of-bounds points will
    # get the first value in the discrete key.
    #---------------------------------------------------------------------
    def __mergeDiscreteGrid(self, gridA, gridB):
        if gridA is None and gridB is None:
            return None

        noKey = self.__discreteKeys[0]

        # merge the grids
        if gridA is not None:
            mask = numpy.not_equal(gridA[0], self.__inFillV)
            numpy.logical_and(mask, self.__areaMask, mask)

            if gridB is None:   #make an empty grid
                noKeys = []
                noGrid = numpy.empty_like(gridA[0])
                noGrid.fill(self.__findKey(noKey, noKeys))
                gridB = (noGrid, noKeys)

            (commonkey, remapG, dbG) = \
              self.__commonizeKey(gridA, gridB)
            mergedGrid = numpy.where(mask, remapG, dbG)
            return (mergedGrid, commonkey)

        # blank out the data
        else:
            blankGrid = numpy.empty_like(gridB[0])
            blankGrid.fill(self.__findKey(noKey, gridB[1]))
            key = gridB[1]
            grid = numpy.where(self.__areaMask, blankGrid, gridB[0])
            return (grid, key)

    #---------------------------------------------------------------------
    # mergeGrid
    # Merges the grid
    # Scalar: (grid, history)
    # Vector: ((magGrid, dirGrid), history)
    # Weather: ((byteGrid, key), history)
    # Discrete: ((byteGrid, key), history)
    # gridA = input remapped grid, contains inFillV to denote invalid
    # gridB = grid to have gridA mosaic'd into
    #   Note: gridA can be None, which indicates that the data
    #   is to be blanked out, i.e., made invalid.  gridB can also be
    #   none, which indicates that there is no destination grid and one must
    #   be created.
    #---------------------------------------------------------------------
    def mergeGrid(self, gridAIn, gridBIn):
        # merge the grids
        if gridAIn is not None:
            gridA = gridAIn[0]
            historyA = gridAIn[1]
        else:
            gridA = None
            historyA = None
        if gridBIn is not None:
            gridB = gridBIn[0]
            historyB = gridBIn[1]
        else:
            gridB = None
            historyB = None

        if self.__gridType == 'SCALAR':
            mergedGrid = self.__mergeScalarGrid(gridA, gridB)

        elif self.__gridType == 'VECTOR':
            mergedGrid = self.__mergeVectorGrid(gridA, gridB)

        elif self.__gridType == 'WEATHER':
            mergedGrid = self.__mergeWeatherGrid(gridA, gridB)

        elif self.__gridType == 'DISCRETE':
            mergedGrid = self.__mergeDiscreteGrid(gridA, gridB)

        else:
            mergedGrid = None

        # merge History
        history = self.__updateHistoryStrings(historyA, historyB)

        return (mergedGrid, history)
