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
# ------------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# Jul 06, 2009 1995       bphillip    Initial Creation.
# Nov 05, 2013 2517       randerso    Improve memory utilization
# Aug 06, 2015 4718       dgilling    Optimize casting when using where with
#                                     NumPy 1.9.
# Apr 07, 2016 5539       randerso    Fixed issues with Wx/Discretes with
#                                     large number of keys
# Feb 22, 2017 6143       randerso    Improved performance of __collapseKey
# Jun 23  2017 20099      bhunder     Corrected _collapseKey
# Apr 02, 2020 8125       randerso    Fixed error introduced in Python 3 update
#                                     that caused received data to wipe out data
#                                     from other CWAs
#
##

##
# This is a base file that is not intended to be overridden.
##

import numpy


class MergeGrid:
    """ Merges two grids and their histories together.
    
    Call mergeGrid to merge the grids. Data from gridA is merged into gridB
    based on an area mask. Grids are represented in the following manner:
    
    Scalar: (grid, history)
    Vector: ((magGrid, dirGrid), history)
    Weather: ((byteGrid, key), history)
    Discrete: ((byteGrid, key), history)
    
    Attributes:
        __creationTime: time in epoch seconds
        __siteID: 3 character site identifier for the input grid
        __inFillV: input grid fill value, indicates missing or no value
        __outFillV: output grid fill value
        __areaMask: mask of areas to merge from gridA to gridB
        __gridType: Type of grids being merged. Can 'SCALAR', 'VECTOR', 
                    'WEATHER' or 'DISCRETE'
        __discreteKeys: (optional) For discrete grids, the list of all valid 
                        key values.
    """

    def __init__(self, creationTime, siteID, inFillValue, outFillValue,
      areaMask, gridType, discreteKeys=None):
        """Creates a new MergeGrid instance.
        
        Args:
            creationTime: time in epoch seconds
            siteID: 3 character site identifier for the input grid
            inFillValue: input grid fill value, indicates missing or no value
            outFillValue: output grid fill value
            areaMask: mask of areas to merge from gridA to gridB
            gridType: Type of grids being merged. Can 'SCALAR', 'VECTOR', 
                      'WEATHER' or 'DISCRETE'
            discreteKeys: (optional) For discrete grids, the list of all valid 
                        key values. Default: None.
        """ 

        self.__creationTime = creationTime
        self.__siteID = siteID
        self.__inFillV = inFillValue
        self.__outFillV = numpy.float32(outFillValue)
        self.__areaMask = areaMask
        self.__gridType = gridType
        self.__discreteKeys = discreteKeys

    def __findKey(self, key, keyMap):
        """Finds the index of a weather or discrete key in the list of keys.
        
        If the key cannot be found, it is added to the list and that key's new
        index is returned.
        
        Args:
            key: weather or discrete key to find
            keyMap: list of valid weather and discrete keys

        Returns:
            The index of key in keyMap. If key was not found it is added to the
            end of keyMap and len - 1 is returned.
            
        Raises:
            IndexError: If the size of keyMap exceeds 255 unique values.
        """ 
        try:
            index = keyMap.index(key)
            return index
        except:
            if (len(keyMap) >= 256):
                raise IndexError("Attempt to create more than 256 Wx keys")

            keyMap.append(key)
            return len(keyMap) - 1

    def __commonizeKey(self, wxA, wxB):
        """Merges the weather key values of two grids and commonizes their indices.
        
        To commonize the key values means that we use the same byte value for 
        the same weather key in both grids. Any key values that appear in one 
        grid but not the other will be added to the list of keys and a new byte
        value assigned.
        
        Args:
            wxA: input grid in a (byte_grid, keys) tuple
            wxB: base grid in a (byte_grid, keys) tuple

        Returns:
            A tuple containing the merged weather keys, the input byte grid and
            the merged byte grid.
            
        Raises:
            IndexError: If the size of the new weather keys list exceeds 255 
            unique values.
        """ 

        # make common key and make data changes in B
        gridB = wxB[0]
        key = wxA[1]
        newGrid = numpy.zeros_like(gridB)

        for k in range(len(wxB[1])):
            index = self.__findKey(wxB[1][k], key)
            newGrid[gridB == k] = index

        return (key, wxA[0], newGrid)

    def __updateHistoryStrings(self, historyA, historyB):
        """Merges the history strings for two grids.
        
        Args:
            historyA: history list from input grid. If None, delete existing 
                      history entries from gridA.
            historyB: history list from input grid. If None, there was no 
                      existing base grid.

        Returns:
            A list containing the merged histories in the string encoded 
            format. Returns None if no history entries were present in either
            grid.
        """ 

        out = []

        # removal any old entry
        if historyB is not None:
            for h in historyB:
                if f":{self.__siteID}_GRID" not in h:
                    out.append(h)

        # if add mode, add in new entries
        if historyA is not None:
            for h in historyA:
                out.append(h)

        if len(out) > 0:
            return out
        else:
            return None

    def __mergeScalarGrid(self, gridA, gridB):
        """Merge the values of two scalar grids together.
        
        The merge is performed by taking the values from gridA that overlap the
        mask __areaMask and the values from gridB outside the mask and creating
        a new grid by merge the two areas together.
        
        Args:
            gridA: The grid to merge into gridA. 
            gridB: The grid to merged with gridA.
            
        Returns:
            A new grid instance that is the result of merging gridA into gridB.
        """ 

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

    def __mergeVectorGrid(self, gridA, gridB):
        """Merge the values of two vector grids together.
        
        The merge is performed by taking the values from gridA that overlap the
        mask __areaMask and the values from gridB outside the mask and creating
        a new grid by merge the two areas together.
        
        When merging direction grids __outFillV is ignored, all out-of-bounds 
        points will get the value 0.0.
        
        Args:
            gridA: The grid to merge into gridA. 
            gridB: The grid to merged with gridA.
            
        Returns:
            A new grid instance that is the result of merging gridA into gridB.
        """ 

        if gridA is None and gridB is None:
            return None

        # merge the grids
        if gridA is not None:
            mask = numpy.not_equal(gridA[0], self.__inFillV)
            numpy.logical_and(mask, self.__areaMask, mask)

            if gridB is None:
                magGrid = numpy.where(mask, gridA[0], self.__outFillV)
                dirGrid = numpy.where(mask, gridA[1], numpy.float32(0.0))
            else:
                magGrid = numpy.where(mask, gridA[0], gridB[0])
                dirGrid = numpy.where(mask, gridA[1], gridB[1])
            return (magGrid, dirGrid)

        # blank out the data
        else:
            magGrid = numpy.where(self.__areaMask, self.__outFillV, gridB[0])
            dirGrid = numpy.where(self.__areaMask, numpy.float32(0.0), gridB[1])
            return (magGrid, dirGrid)

    def __collapseKey(self, grid, keys):
        """Remove unused key values from weather and discrete grids.
        
        Args:
            grid: byte grid for the weather or discrete grid.
            keys: list of weather or discrete keys.

        Returns:
            A tuple containing the updated grid and updated keys.
        """ 

        #make list of unique indices in the grid
        usedIndices = numpy.unique(grid.astype(numpy.uint8))

        #make reverse newIndices
        newIndices = []
        newKeys = []
        j = 0
        for i, key in enumerate(keys):
           if i in usedIndices:
               newIndices.append(j)
               newKeys.append(key)
               j += 1
           else:
               newIndices.append(-1)

        # modify the data
        newGrid = grid.copy()
        for k, newIndex in enumerate(newIndices):
           mask = numpy.equal(numpy.int8(k), grid)
           newGrid[mask] = numpy.int8(newIndex)

        return (newGrid, newKeys)

    def __mergeWeatherGrid(self, gridA, gridB):
        """Merge the values of two weather grids together.
        
        The merge is performed by taking the values from gridA that overlap the
        mask __areaMask and the values from gridB outside the mask and creating
        a new grid by merge the two areas together.
        
        When merging weather grids __outFillV is ignored, all out-of-bounds 
        points will get the <NoWx> value.
        
        Args:
            gridA: The grid to merge into gridA. 
            gridB: The grid to merged with gridA.
            
        Returns:
            A new grid instance that is the result of merging gridA into gridB.
        """ 

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
            else:
                # clear out the masked area in gridB and collapse gridB's keys
                grid, keys = gridB
                grid[mask]= self.__findKey(noWx, keys)
                gridB = self.__collapseKey(grid, keys)
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

    def __mergeDiscreteGrid(self, gridA, gridB):
        """Merge the values of two discrete grids together.
        
        The merge is performed by taking the values from gridA that overlap the
        mask __areaMask and the values from gridB outside the mask and creating
        a new grid by merge the two areas together.
        
        When merging weather grids __outFillV is ignored, all out-of-bounds 
        points will get the first discrete key value.
        
        Args:
            gridA: The grid to merge into gridA. 
            gridB: The grid to merged with gridA.
            
        Returns:
            A new grid instance that is the result of merging gridA into gridB.
        """ 

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
            else:
                # clear out the masked area in gridB and collapse gridB's keys
                grid, keys = gridB
                grid[mask] = self.__findKey(noKey, keys)
                gridB = self.__collapseKey(grid, keys)

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

    def mergeGrid(self, gridAIn, gridBIn):
        """Merge the values of two grids together.
        
        The merge is performed by taking the values from gridA that overlap the
        mask __areaMask and the values from gridB outside the mask and creating
        a new grid by merge the two areas together.
        
        Grids are represented by:
            Scalar: (grid, history)
            Vector: ((magGrid, dirGrid), history)
            Weather: ((byteGrid, key), history)
            Discrete: ((byteGrid, key), history)
        
        Args:
            gridA: The grid to merge into gridA. If None, the data is blanked 
                   out (i.e., automatically assigned __outFillV).
            gridB: The grid that grid will be mosaic-ed into. If None, that 
                   indicates there is no destination grid and it will be 
                   created using the value of __outFillV.
            
        Returns:
            A new grid instance that is the result of merging gridA into gridB.
        """ 

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
