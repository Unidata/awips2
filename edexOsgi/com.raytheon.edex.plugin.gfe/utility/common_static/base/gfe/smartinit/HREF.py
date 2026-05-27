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
################################################################################
##
# SOFTWARE HISTORY
#
# Date        Ticket#  Engineer   Description
# ----------- -------- ---------- --------------------------------------------
# 07/26/2021  DCS22682 ajust,NSI  HREF v3 initial baseline version.
#
##

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##
################################################################################
# NWS version of HREF created by NWS_HREF/make-NWS_HREF.sh
#
# This file is under NSI configuration management. DO NOT CHANGE
#  Copy methods you wish to change into the Local_HREF module and make desired
#  changes there.
#

from Init import *
import numpy as np


def main():
    HREF().run()


class HREF(Forecaster):

    # From: NWS_HREF.__init__.py
    def __init__(self, srcName="HREF", dstName=None):
         Forecaster.__init__(self, "HREF")

    # From: ../../methods/levels/HREF.levels.py
    def levels(self):
        """levels - returns valid levels for data in order of ascending height"""

        return ["MB1000", "MB925", "MB850", "MB700", "MB500", "MB250"]

    # From: ../../methods/calcCigProb10/cigc2_CLG.calcCigProb10.py
    def calcCigProb10(self, cigc2_CLG):
        """calcCigProb10 - Return Prob Cig < 1000 ft"""
        return cigc2_CLG

    # From: ../../methods/calcCigProb20/cigprob2k_CLG.calcCigProb20.py
    def calcCigProb20(self, cigprob2k_CLG):
        """calcCigProb20 - Return Prob Cig < 2000 ft"""
        return cigprob2k_CLG

    # From: ../../methods/calcCigProb30/cigprob3k_CLG.calcCigProb30.py
    def calcCigProb30(self, cigprob3k_CLG):
        """calcCigProb30 - Return Prob Cig < 3000 ft"""
        return cigprob3k_CLG

    # From: ../../methods/calcCeiling/ghmean.calcCeiling.py
    def calcCeiling(self, ghmean_CLG):
        """calcCeiling - converts model ceiling from meters to feet AGL

        Args:
            ghmean_CLG: ndarray of ceiling height (m)

        Returns:
            float32 ndarray with the ceiling height (ft)
        """

        # Convert the model ceiling from meters to feet AGL
        Ceiling = ghmean_CLG / 0.3048

        # D2D displays -3.28m in clear areas outside the fog areas, so we can take advantage
        # this information to distinguish clear areas from the dense fog associated with 0
        # values.
        Ceiling[Ceiling < 0] = -30000

        # Return this field
        return Ceiling

    # From: ../../methods/calcCloudBasePrimary/XX_HREF.calcCloudBasePrimary.py
    def calcCloudBasePrimary(self, ghmean_CBL, Sky, Ceiling, topo):
        """CloudBasePrimary convert from M to 100's of FT"""

        CloudBasePrimary = np.where(
            ghmean_CBL <= 0, ((ghmean_CBL - topo) * 0.0328083989501), 250
        )
        CeilingKft = Ceiling / 100
        # Make sure that if there is a Ceiling grid there is a CloudBasePrimary grid.
        Mask = (Sky >= 62.5) & (CeilingKft < CloudBasePrimary)
        CloudBasePrimary[Mask] = CeilingKft[Mask]

        return CloudBasePrimary.clip(0, 250)

    # From: ../../methods/calcHaines/hindexmean.calcHaines.py
    def calcHaines(self, hindexmean_SFC):
        return hindexmean_SFC

    # From: ../../methods/calcMaxRH/calcMaxRH.py
    def calcMaxRH(self, RH, MaxRH, mtime):
        """calcMaxRH - computes the maximum RH during the specified period

        Args:
            RH: ndarray of relative humidity for a portion of destination period (percent)
            MaxRH: ndarray of current maximum RH for this destination period (percent)
            mtime: time of destination grid in epoch seconds (start time, end time)

        Returns:
            float32 ndarray with the maximum RH
        """

        # First, try the derived method to ensure enough component grids are available.
        # If that method does not work in this situation, continue to regular method.
        try:
            return self.BI_handleDerivedGrids("MaxRH", RH, "RH", mtime, None)
        except BaseException:
            pass

        # If the current maximum RH is missing - return the current RH to start
        if MaxRH is None:
            return RH

        # If we made it this far, return the maximum RH for this period
        return np.maximum(MaxRH, RH)

    # From: ../../methods/calcMaxT/calcMaxT.py
    def calcMaxT(self, T, MaxT, mtime):
        """calcMaxT - computes the maximum temperature using the BI_calcMaxT method.

        Args:
            T: ndarray of surface temperature (degrees F)
            MaxT: ndarray of maximum surface temperature (degrees F)
            mtime: time of destination grid in epoch seconds (start time, end time)

        Returns:
            float32 ndarray with the maximum temperature
        """

        return self.BI_calcMaxT(T, MaxT, mtime)

    # From: ../../methods/calcMinRH/calcMinRH.py
    def calcMinRH(self, RH, MinRH, mtime):
        """calcMinRH - computes the minimum RH during the specified period

        Args:
            RH: ndarray of relative humidity for a portion of destination period (percent)
            MinRH: ndarray of current minimum RH for this destination period (percent)
            mtime: time of destination grid in epoch seconds (start time, end time)

        Returns:
            float32 ndarray with the minimum RH
        """

        # First, try the derived method to ensure enough component grids are available.
        # If that method does not work in this situation, continue to regular method.
        try:
            return self.BI_handleDerivedGrids("MinRH", RH, "RH", mtime, None)
        except BaseException:
            pass

        # If the current minimum RH is missing - return the current RH to start
        if MinRH is None:
            return RH

        # If we made it this far, return the minimum RH for this period
        return np.minimum(MinRH, RH)

    # From: ../../methods/calcMinT/calcMinT.py
    def calcMinT(self, T, MinT, mtime):
        """calcMinT - computes the minimum temperature using the BI_calcMinT method.

        Args:
            T: ndarray of surface temperature (degrees F)
            MinT: ndarray of minimum surface temperature (degrees F)
            mtime: time of destination grid in epoch seconds (start time, end time)

        Returns:
            float32 ndarray with the minimum temperature
        """

        return self.BI_calcMinT(T, MinT, mtime)

    # From: ../../methods/calcPoP/tp1c1.calcPoP.py
    def calcPoP(self, tp1c1_SFC, topo):

        PoP = tp1c1_SFC

        return PoP

    # From: ../../methods/calcQPF/tp1mean.calcQPF.py
    def calcQPF(self, tp1mean_SFC):
        """QPF - change mm to inches and clip greater than 1000mm use tp1mean_SFC"""

        qpf = np.where((tp1mean_SFC > 1000), 0.0, (tp1mean_SFC / 25.4))
        return qpf.clip(0, 10)  # clip at zero and 10 inches

    # From: ../../methods/calcRadar/refcpmmn.calcRadar.py
    def calcRadar(self, refcpmmn_EA):
        return refcpmmn_EA

    # From: ../../methods/calcRH/calcRH.py
    def calcRH(self, T, Td):
        """calcRH - calculate RH based on Temp and Dewpoint (both in degrees F)"""
        Tc = 0.556 * (T - 32.0)
        Tdc = 0.556 * (Td - 32.0)
        satVaporPress = 6.1078 * np.exp((Tc * 17.269) / (Tc + 237.3))
        vaporPress = 6.1078 * np.exp((Tdc * 17.269) / (Tdc + 237.3))
        return np.clip((vaporPress / satVaporPress) * 100, 2, 100)

    # From: ../../methods/calcSky/tccmean_EA.calcSky.py
    def calcSky(self, tccmean_EA):
        return tccmean_EA.clip(0.0, 100.0)  # clip

    # From: ../../methods/calcTdAft/calcTdAft.py
    def calcTdAft(self, MaxT, MinRH):
        """Computes dewpoint from MaxT and MinRH via BI_dewFromTandRH"""
        # Do not create a grid if there is not enough data.
        if MaxT is None or MinRH is None:
            return
        return self.BI_dewFromTandRH(MaxT, MinRH)

    # From: ../../methods/calcTdMrn/calcTdMrn.py
    def calcTdMrn(self, MinT, MaxRH):
        """Computes dewpoint from MinT and MaxRH via BI_dewFromTandRH."""
        # Do not create a grid if there is not enough data.
        if MinT is None or MaxRH is None:
            return
        return self.BI_dewFromTandRH(MinT, MaxRH)

    # From: ../../methods/calcTd/SREF.calcTd.py
    def calcTd(self, dptmean_FHAG2):
        return self.KtoF(dptmean_FHAG2)

    # From: ../../methods/calcT/SREF.calcT.py
    def calcT(self, tmean_FHAG2):
        return self.KtoF(tmean_FHAG2)

    # From: ../../methods/calcVisibility/HREF.calcVisibility.py
    def calcVisibility(self, vismean_SFC):
        """calcVisibility - returns surface visibility

        Args:
            ndarray vismean_SFC: mean surface visibility (m)

        Returns:
            float32 ndarray of surface visibility (statue miles)
        """

        # Fill in any missing data - if needed
        try:
            if "fillMasks" in self.BI_optionsDict:
                # Fill in data-void regions
                vismean_SFC = self.BI_fillScalar(vismean_SFC)
        except AttributeError:
            pass

        # Return a smoothed visibility field after converting units
        return self.BI_smoothpm(vismean_SFC / 1609.344, 2)

    # From: ../../methods/calcVisProb4/visprob4p0_SFC.calcVisProb4.py
    def calcVisProb4(self, visprob4p0_SFC):
        """calcVisProb4 - returns probability of visibility < 1 statue mile

        Args:
            ndarray visprob4p0_SFC: probability of visibility < 1 statue mile (% * 100)

        Returns:
            float32 ndarray with probability of visibility < 1 statue mile (% * 100)
        """

        # Fill in any missing data - if needed
        try:
            if "fillMasks" in self.BI_optionsDict:
                # Fill in data-void regions
                visprob4p0_SFC = self.BI_fillScalar(visprob4p0_SFC)
        except AttributeError:
            pass

        return visprob4p0_SFC

    # From: ../../methods/calcVisProb2/visprob2p0_SFC.calcVisProb2.py
    def calcVisProb2(self, visprob2p0_SFC):
        """calcVisProb2 - returns probability of visibility < 1 statue mile

        Args:
            ndarray visprob2p0_SFC: probability of visibility < 1 statue mile (% * 100)

        Returns:
            float32 ndarray with probability of visibility < 1 statue mile (% * 100)
        """

        # Fill in any missing data - if needed
        try:
            if "fillMasks" in self.BI_optionsDict:
                # Fill in data-void regions
                visprob2p0_SFC = self.BI_fillScalar(visprob2p0_SFC)
        except AttributeError:
            pass

        return visprob2p0_SFC

    # From: ../../methods/calcVisProb1/visprob1p0_SFC.calcVisProb1.py
    def calcVisProb1(self, visprob1p0_SFC):
        """calcVisProb1 - returns probability of visibility < 1 statue mile

        Args:
            ndarray visprob1p0_SFC: probability of visibility < 1 statue mile (% * 100)

        Returns:
            float32 ndarray with probability of visibility < 1 statue mile (% * 100)
        """

        # Fill in any missing data - if needed
        try:
            if "fillMasks" in self.BI_optionsDict:
                # Fill in data-void regions
                visprob1p0_SFC = self.BI_fillScalar(visprob1p0_SFC)
        except AttributeError:
            pass

        return visprob1p0_SFC

    # From: ../../methods/calcVisProb05/visprob0p5_SFC.calcVisProb05.py
    def calcVisProb05(self, visprob0p5_SFC):
        """calcVisProb05 - returns probability of visibility < 1 statue mile

        Args:
            ndarray visprob0p5_SFC: probability of visibility < 1 statue mile (% * 100)

        Returns:
            float32 ndarray with probability of visibility < 1 statue mile (% * 100)
        """

        # Fill in any missing data - if needed
        try:
            if "fillMasks" in self.BI_optionsDict:
                # Fill in data-void regions
                visprob0p5_SFC = self.BI_fillScalar(visprob0p5_SFC)
        except AttributeError:
            pass

        return visprob0p5_SFC

    # From: ../../methods/calcVisProb025/visprob0p25_SFC.calcVisProb025.py
    def calcVisProb025(self, visprob0p25_SFC):
        """calcVisProb025 - returns probability of visibility < 1 statue mile

        Args:
            ndarray visprob0p25_SFC: probability of visibility < 1 statue mile (% * 100)

        Returns:
            float32 ndarray with probability of visibility < 1 statue mile (% * 100)
        """

        # Fill in any missing data - if needed
        try:
            if "fillMasks" in self.BI_optionsDict:
               # Fill in data-void regions
               visprob0p25_SFC = self.BI_fillScalar(visprob0p25_SFC)
        except AttributeError:
            pass

        return visprob0p25_SFC

    # From: ../../methods/BI_calcMaxT/BI_calcMaxT.py
    # ===========================================================================
    # MaxT and MinT - max and min of hourly Ts, optionally can apply an offset
    # to hourly T (higher for MaxT and lower for MinT)
    # ===========================================================================
    def BI_calcMaxT(self, T, MaxT, mtime, modelSfcT=None, modelMaxT=None, offset=0.0):
        """BI_calcMaxT - calculates maximum of all model temperatures, both hourly and longer
            duration max temperatures.

        Args:
            ndarray T: hourly surface temperature (deg F)
            ndarray MaxT: maximum surface temperature (deg F)
            tuple mtime: start and end times (in epoch seconds) of the destination database grid
                         currently being calculated.
            ndarray modelSfcT: optional model time step surface temperature (K)
            ndarray modelMaxT: optional model max surface temperature (K)
            float32 offset: optional value to add to hourly surface T before computing a new max
                    temperature (default = 0.0)

            Note: If modelSfcT is used, modelMaxT must also be used.

        Returns:
            float32 ndarray with the computed maximum surface temperature
        """

        # If we have raw model temperatures
        if modelSfcT is not None and modelMaxT is not None:

            # Compute a difference between model temperatures we can apply to the previously
            # downscaled hourly temperature in degrees F
            #  (don't forget we need to account for differences between degrees C and degrees F)
            #             diff = 0.5 + ((modelMaxT - modelSfcT) * 9.0 / 5.0)
            diff = modelMaxT - modelSfcT
            diff *= 1.8  # 9.0/5.0 - convert to Fahrenheit
            diff += 0.5  # round up max temperatures

        # Otherwise, just start with an empty grid
        else:
            diff = self.empty()

        # First, try the derived method to ensure enough component grids are
        # available. If that method does not work in this situation, continue
        # to regular method.
        try:
            return self.BI_handleDerivedGrids("MaxT", T + diff + offset, "T", mtime, None)
        except ValueError:
            pass

        # If the MaxT grid is missing so far
        if MaxT is None:

            # Use the current hourly T after applying any offset
            return T + diff + offset

        # Otherwise, keep the highest temperature (after applying offset)
        MaxT = np.maximum(MaxT, T + diff + offset)

        # Return completed grid
        return self.BI_enforceNpType(MaxT)

    # From: ../../methods/BI_calcMinT/BI_calcMinT.py
    def BI_calcMinT(self, T, MinT, mtime, modelSfcT=None, modelMinT=None, offset=0):
        """BI_calcMinT - calculates minimum of all model temperatures, both hourly and longer
            duration min temperatures.

        Args:
            ndarray T: hourly surface temperature (deg F)
            ndarray MinT: minimum surface temperature (deg F)
            tuple mtime: start and end times (in epoch seconds) of the destination database grid
                         currently being calculated.
            ndarray modelSfcT: optional model time step surface temperature (K)
            ndarray modelMinT: optional model min surface temperature (K)
            float32 offset: optional value to add to hourly surface T before computing a new min
                    temperature (default = 0.0)

            Note: If modelSfcT is used, modelMinT must also be used.

        Returns:
            float32 ndarray with the computed minimum surface temperature
        """

        # If we have raw model temperatures
        if modelSfcT is not None and modelMinT is not None:

            # Compute a difference between the temperatures we can apply
            # to the previously downscaled temperature
            #  (don't forget we need to account for differences between
            #   degrees C and degrees F)
            #             diff = 0.5 + (modelSfcT - modelMinT) * 9.0 / 5.0
            diff = modelSfcT - modelMinT
            diff *= 1.8  # 9.0/5.0 - convert to Fahrenheit
            diff += 0.5  # round up min temperatures

        # otherwise, just start with an empty grid
        else:
            diff = self.empty()
        #
        # First, try the derived method to ensure enough component grids are
        # available. If that method does not work in this situation, continue
        # to regular method.
        #
        try:
            return self.BI_handleDerivedGrids("MinT", T + diff - offset, "T", mtime, None)
        except ValueError:
            pass

        # If the MinT grid is missing so far
        if MinT is None:

            # Use the current hourly T after applying any offset
            return T + diff - offset

        # Otherwise, keep the lowest temperature (after applying offset)
        MinT = np.minimum(MinT, T + diff - offset)

        # Return completed grid
        return self.BI_enforceNpType(MinT)

    # From: ../../methods/BI_handleDerivedGrids/BI_handleDerivedGrids.py
    def BI_handleDerivedGrids(self, varName, inputGrid, gridType, mtime, minGrids):
        """BI_handleDerivedGrids - ensures we have enough component grids available to calculate
        a derived grid.

        Args:
            string varName: derived parameter to be created (usually
                            MaxT/MinT/MaxRH/MinRH/TdMrn/TdAft/PoP[3/6/12]/QPF[3/6/12])
            ndarray inputGrid: component grid that the init is currently operating on
            string gridType: name of the component grid (generally T/Td/RH/PoP*/QPF*)
            tuple mtime: time of destination grid in epoch seconds (start time, end time)
            integer minGrids: required minimum number of component grids to compute derived grid
                              (can also be None. if None, an attempt to use the built in dictionary
                              is made to grab the correct minGrid value. An error is raised if
                              the model name is not present in the dictionary)

        Returns:
            None, if required # of component grids is unavailable or grid type is unsupported.
            Otherwise, ndarray with the derived grid.
        """

        # This method ensures that there are enough component grids available to calculate a
        # derived grid. For example, we don't want a MaxT to be created when we only have the
        # component T grids through 9 AM.

        # Test if dictionary lookup method is needed
        if minGrids is None:

            # Dictionary of temporal resolution and required grids by model & parm.
            # These entries are selected to eliminate grids at "bad" times as much as
            # possible, while still allowing for differences in time zone. The entries
            # for hourly temporal resolution models are slightly relaxed to allow a
            # few more grids to be created.
            requiredGridsDict = {
                # Name         Res  MaxT  MinT  MaxRH  MinRH
                "ECMWF": [6, 2, 2, 3, 3],
                "CMCnh": [6, 2, 2, 3, 3],
                "CMCreg": [3, 4, 4, 6, 6],
                "GFS": [6, 2, 2, 3, 3],
                "GLAMP25": [1, 11, 12, 15, 15],
                "HIRESWarw": [3, 4, 4, 6, 6],
                "HIRESWFV3": [3, 4, 4, 6, 6],
                "HREF": [1, 11, 12, 15, 15],
                "HRRR": [1, 11, 12, 15, 15],
                "HRRREXP": [1, 11, 12, 15, 15],
                "LAMP25": [1, 11, 12, 15, 15],
                "NAM12": [3, 4, 4, 6, 6],
                "NAMNest": [1, 11, 12, 15, 15],
                "RAP13": [1, 11, 12, 15, 15],
                "SREF": [3, 4, 4, 6, 6],
                "WPCGuide": [6, 2, 2, 3, 3],
                "GFS1hr": [1, 11, 12, 15, 15],
            }

            # Grab model database name
            dbID = self.newdb().getModelIdentifier()
            modelName = dbID[10 : dbID.find("_", 10, len(dbID))]

            # Check if current model is available in dict, otherwise raise error
            if modelName not in requiredGridsDict:
                raise ValueError

            # Grab requirements for current situation
            if varName == "MaxT":
                minGrids = requiredGridsDict[modelName][1]
            elif varName == "MinT":
                minGrids = requiredGridsDict[modelName][2]
            elif varName == "MaxRH":
                minGrids = requiredGridsDict[modelName][3]
            else:
                minGrids = requiredGridsDict[modelName][4]

            # For QPF & PoP, just use the model resolution to determine the number of required
            # component grids
            if "QPF" or "PoP" in varName:
                if "3" in varName:
                    minGrids = 3 / (requiredGridsDict[modelName][0])
                elif "6" in varName:
                    minGrids = 6 / (requiredGridsDict[modelName][0])
                elif "12" in varName:
                    minGrids = 12 / (requiredGridsDict[modelName][0])
                if minGrids < 1:
                    raise ValueError

        # For additive grids, we'll start with zero, otherwise the starting point is just the
        # original input
        if varName in ["QPF12", "QPF6", "QPF3", "QPE06", "QPE03"]:
            finalGrid = np.zeros_like(inputGrid)
        else:
            finalGrid = inputGrid

        # Get an Object with all component grids
        tempobj = self._Forecaster__getNewWE(gridType + "_SFC")
        if tempobj is None:
            return None

        # Get the time range tuples of the existing T grids
        temptrobj = tempobj.getKeys()

        # Loop through existing grid time ranges
        numtrs = temptrobj.size()
        count = 0
        for i in range(numtrs):

            # If this grid time range is inside time range of derived grid, count it
            tr = temptrobj.get(i)
            if mtime.contains(tr):
                count += 1

                # Get the numpy grid from a JEP gridslice
                temp = tempobj.getItem(tr).getNDArray()

                # Calculate the derived grid according to the type
                if varName in ["MaxT", "MaxRH", "TdAft", "PoP12", "PoP6", "PoP3"]:
                    finalGrid = np.maximum(finalGrid, temp)
                elif varName in ["QPF12", "QPF6", "QPF3", "QPE06", "QPE03"]:
                    finalGrid += temp
                elif varName in ["MinT", "MinRH", "TdMrn"]:
                    finalGrid = np.minimum(finalGrid, temp)
                else:
                    return None

        # If the minimum # of grids are present, return the derived grid
        if count >= minGrids:
            return finalGrid

        # Otherwise, we do not yet have enough data to compute derived grid
        return None

   # From: ../../methods/BI_enforceNpType/BI_enforceNpType.py
    def BI_enforceNpType(self, grid, npType=np.float32):
        """BI_enforceNpType - Ensure this is a 32-bit float to keep Jep/Java happy and suppress
        log messages.
        """

        if grid is not None and grid.dtype != npType:
            grid = grid.astype(npType)
        return grid

   # From: ../../methods/BI_smoothpm/BI_smoothpm.py
    def BI_smoothpm(self, grid, k=1, mask=None, onlyMaskedData=1):
        """BI_smoothpm - Smooths grid by averaging over plus and minus k
        gridpoints, which means an average over a square 2k+1 gridpoints on a
        side.

        Args:
            ndarray grid: field to be smoothed
            integer k: number of points around loca to use in smooth (default 1)
            ndarray mask: boolean mask which can limit domain of smoothing operation
            integer onlyMaskedData: flag that indicates how data should be handled
                        if 1 - only uses data within masked area, output clipped to mask (default)
                        if 0 - uses all data in smooth, data clipped to mask
                        if -1 - same as 1, except output will not be clipped at all

        Returns:
            ndarray with the smoothed field, same data type as input field
        """

        # If mask is specified (a boolean grid), only modify points that have mask=True, not any
        # other points.
        #
        # If a mask is specified, the default is for only the points inside the mask to influence
        # the smoothed points. This keeps data from outside the mask "bleeding" into the area
        # being smoothed. If, however, you want the data outside the mask to impact the smoothed
        # data, set onlyMaskedData=0 (it defaults to 1). Setting onlyMaskedData to -1 is the same
        # as 1, but it returns the data without clipping it to the mask. This is used for
        # fillMissingClimo, which takes advantage of the small expanded amount of data that is
        # created along the edges of the masked data.
        #
        # Near the edges of the grid, the average is over fewer points than in the center of the
        # grid - because some of the points in the averaging window would be off the grid. It just
        # averages over the points that it can. For example, on the edge gridpoint - it can only
        # come inside k points - so the average is over only k+1 points in that direction (though
        # over all 2k+1 points in the other direction - if possible)
        #
        # This is much faster than shifting the grid multiple times and adding them up. Instead it
        # uses the cumsum function in numpy - which gives you cumulative sum across a row/column.
        # Total across the 2k+1 points is the cumsum at the last point minus the cumsum at the
        # point before the first point. Only edge points need special handling - and the cumsum is
        # useful there too.
        #
        # This method will maintain the data type it got as input. For example, an input grid of
        # integers will be converted to float32, processed, then converted back to the original
        # integer type at the end.

        # Has to be an integer number of gridpoints
        k = int(k)

        # Has to be a positive number of gridpoints
        if k < 1:
            return grid

        # Must be a 2-D field
        if len(grid.shape) != 2:
            return grid

        (ny, nx) = grid.shape
        k2 = k * 2

        # Get the original grid type
        finalReturnType = grid.dtype

        # Because we want to return float32 to make JEP/Java happy, change
        # the final dtype to float32 if it is float64.
        if finalReturnType == np.float64:
            grid = grid.astype(np.float32)
            finalReturnType = np.float32

        # -----------------------------------------------------------------------
        # If the input grid is an integer type, convert it to a float before
        # any smoothing takes place. It will be converted back to an integer
        # before it is returned

        if finalReturnType != np.float32:
            grid = grid.astype(np.float32)
        #
        # Remove the minimum and divide by the range from the grid so
        # that when cumsum accumulates the sum over a full row or
        # column that it doesn't get so big that precision is lost, or
        # might be lost.  This makes the 'gridmin' grid have all points
        # ranging from 0 to 1.
        #
        fullmin = np.amin(grid)
        fullmax = np.amax(grid)
        fullrange = fullmax - fullmin
        if fullrange < 0.001:
            fullrange = 0.001
        gridmin = (grid - fullmin) / fullrange
        #
        # When there is no mask the code is much simpler
        #
        if (mask is None) or (onlyMaskedData == 0):
            #
            # Average over the first (y) dimension - making the 'mid' grid
            #
            mid = np.zeros(grid.shape, np.float32)
            c = np.cumsum(gridmin, 0)
            nym1 = ny - 1
            midy = int((ny - 1.0) / 2.0)
            ymax = min(k + 1, midy + 1)
            #
            # Handle the edges
            #
            for j in range(ymax):
                jk = min(j + k, nym1)
                jk2 = max(nym1 - j - k - 1, -1)
                mid[j, :] = c[jk, :] / float(jk + 1)
                if jk2 == -1:
                    mid[nym1 - j, :] = c[nym1, :] / float(jk + 1)
                else:
                    mid[nym1 - j, :] = (c[nym1, :] - c[jk2, :]) / float(jk + 1)
            #
            # The really fast part for the middle of the grid
            #
            if (k + 1) <= (ny - k):
                mid[k + 1 : ny - k, :] = (c[k2 + 1 :, :] - c[: -k2 - 1, :]) / float(
                    k2 + 1
                )
            #
            # Average over the second (x) dimension - making the 'out' grid
            #
            c = np.cumsum(mid, 1)
            out = np.zeros(grid.shape, np.float32)
            nxm1 = nx - 1
            midx = int((nx - 1.0) / 2.0)
            xmax = min(k + 1, midx + 1)
            #
            # Handle the edges
            #
            for j in range(xmax):
                jk = min(j + k, nxm1)
                jk2 = max(nxm1 - j - k - 1, -1)
                out[:, j] = c[:, jk] / float(jk + 1)
                if jk2 == -1:
                    out[:, nxm1 - j] = c[:, nxm1] / float(jk + 1)
                else:
                    out[:, nxm1 - j] = (c[:, nxm1] - c[:, jk2]) / float(jk + 1)
            #
            # The really fast part for the middle of the grid
            #
            if (k + 1) <= (nx - k):
                out[:, k + 1 : nx - k] = (c[:, k2 + 1 :] - c[:, : -k2 - 1]) / float(
                    k2 + 1
                )
            #
            # Multiply by the range and add the minimum back in
            #
            out *= fullrange
            out += fullmin
            #
            # If only making changes over a masked area, copy the original
            # data outside the mask into the output grid.
            #
            if (onlyMaskedData == 0) and (mask is not None):
                out[~mask] = grid[~mask]
        #
        # When there is a mask specified, it makes the code a bit more
        # difficult. We have to find out how many points were in each
        # cumsum value, and we have to deal with possible divide-by-zero
        # errors for points where no masked points were in the average
        #
        else:
            #
            # Sum over the first (y) dimension - making the 'mid' grid
            #
            # mask = np.clip(mask,0,1)   # Mask should be a boolean
            gridmin1 = np.where(mask, gridmin, 0.0)
            mid = np.zeros(grid.shape, np.float32)
            midd = np.zeros(grid.shape, np.float32)
            c = np.cumsum(gridmin1, 0)
            d = np.cumsum(mask, 0)
            nym1 = ny - 1
            midy = int((ny - 1.0) / 2.0)
            ymax = min(k + 1, midy + 1)
            #
            # Handle the edges
            #
            for j in range(ymax):
                jk = min(j + k, nym1)
                jk2 = max(nym1 - j - k - 1, -1)
                mid[j, :] = c[jk, :]
                midd[j, :] = d[jk, :]
                if jk2 == -1:
                    mid[nym1 - j, :] = c[nym1, :]
                    midd[nym1 - j, :] = d[nym1]
                else:
                    mid[nym1 - j, :] = c[nym1, :] - c[jk2, :]
                    midd[nym1 - j, :] = d[nym1, :] - d[jk2, :]
            #
            # The really fast part for the middle of the grid
            #
            if (k + 1) <= (ny - k):
                mid[k + 1 : ny - k, :] = c[k2 + 1 :, :] - c[: -k2 - 1, :]
                midd[k + 1 : ny - k, :] = d[k2 + 1 :, :] - d[: -k2 - 1, :]
            #
            # Sum over the second (x) dimension - and divide by
            # the number of points (but make sure number of points
            # is at least 1) - making the 'out' grid
            #
            c = np.cumsum(mid, 1)
            d = np.cumsum(midd, 1)
            out = np.zeros(grid.shape, np.float32)
            nxm1 = nx - 1
            midx = int((nx - 1.0) / 2.0)
            xmax = min(k + 1, midx + 1)
            #
            # Handle the edges
            #
            for j in range(xmax):
                jk = min(j + k, nxm1)
                jk2 = max(nxm1 - j - k - 1, -1)
                out[:, j] = c[:, jk] / np.maximum(d[:, jk], 1)
                if jk2 == -1:
                    out[:, nxm1 - j] = c[:, nxm1] / np.maximum(d[:, nxm1], 1)
                else:
                    out[:, nxm1 - j] = (c[:, nxm1] - c[:, jk2]) / np.maximum(
                        d[:, nxm1] - d[:, jk2], 1
                    )
            #
            # The really fast part for the middle of the grid
            #
            if (k + 1) <= (nx - k):
                out[:, k + 1 : nx - k] = (c[:, k2 + 1 :] - c[:, : -k2 - 1]) / np.maximum(
                    d[:, k2 + 1 :] - d[:, : -k2 - 1], 1
                )
            #
            # Multiply by the range and add the minimum back in
            #
            out *= fullrange
            out += fullmin

            # ===================================================================
            # Handle the final output

            # If we need to copy the original data back, do it
            if onlyMaskedData != -1:

                # Copy original data outside the mask into the output grid
                out[~mask] = grid[~mask]

        # If we need to return this grid as an integer, round to the nearest
        # integer before we do
        if finalReturnType != np.float32:
            out = np.rint(out)

        # Return the grid as either a float or integer
        return out.astype(finalReturnType)

   # From: ../../methods/BI_dewFromTandRH/BI_dewFromTandRH.py
    def BI_dewFromTandRH(self, T, RH):
        """BI_dewFromTandRH - Computes a dew point temperature, provided a temperature and
        relative humidity.

        Args:
            ndarray T: temperature (degrees F)
            ndarray RH: relative humidity (percent * 100)

        Returns:
            float32 ndarray with the computed dew point temperature (degrees F)
        """

        # Convert temperature into Celsius
        tc = (T - 32.0) * 0.556

        # Ensure RH is valid, and convert to a percent (0.0 to 1.0)
        rh = RH.clip(0.001, 99.999, RH) / 100.0

        # Compute common term using RH and Celsius temperature
        x = (np.log(rh) / 17.67) + (tc / (tc + 243.5))

        # Compute the dewpoint temperature in Celsius
        tdc = (243.5 * x) / (1.0 - x)

        # Return the dewpoint temperature in Fahrenheit
        return (tdc * 1.8) + 32.0
