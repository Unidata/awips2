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
# Interpolation_4D
#
# Author:
# ----------------------------------------------------------------------------
MenuItems = ["Edit"]

VariableList = [
#        ("", "Gaps", "radio", ["Gaps", "Based on Edited Data"]),
        ("Algorithm", "Cubic Spline", "radio", ["Cubic Spline", "Tweening"]),
        ("", "Gaps", "radio", ["Gaps"]),
        ("Grid Type", "Scalar", "radio", ["Scalar", "Discrete"]),
        ("Interpolation Interval in Hours", 1, "scale", [1,24], 1),
        ("Duration of Grids in Hours", 1, "scale", [1,24], 1),
        ("Anti-aliasing Supersampling Level", 4, "scale", [1,12], 1),
        ("Anti-aliasing Downsample Mode", "Region-Weighted Averaging", "radio",
         ["Region-Weighted Averaging", "Flat Averaging", "Maximizing"]),
        ("Region Weighting", 5, "scale", [1,10], 1),
        ("Verbose", "no", "radio", ["yes", "no"]),
        ]

from numpy import *
import numpy
import SmartScript
import TimeRange
import types, copy
import random

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        self._dbss = dbss;
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, timeRange, varDict):
        # Interpolates active element over given time range

        # This is necessary so that the "getGrids" command will
        # return numeric grids from a Procedure since the default
        # is point-based.
        # This should be fixed in future versions!!
        self.setToolType("numeric")

        # Get Grids and Times based on timeRange and active element
        # grids : List of Existing Grids to be used in interpolating
        # times : List of times (seconds past the start time) corresponding to grids
        element = self.getActiveElement()
        elementName = element.getParmID().getParmName()
        grids = self.getGrids("Fcst", elementName, "SFC", timeRange,
                               mode="List")
        gridInfos = self.getGridInfo("Fcst", elementName, "SFC", timeRange)

        if varDict["Verbose"] == "yes":
            self._verbose = 1
        else:
            self._verbose = 0
        # gridShape : dimension of the grids
        if varDict["Grid Type"] == "Scalar":
            self._gridShape = shape(grids[0])
        else:
            self._gridShape = 1
        times = [zeros(self._gridShape)]

        firstTime = 1
        for gridInfo in gridInfos:
            tr = gridInfo.gridTime()
            if firstTime:
                beginTime = tr.startTime()
                firstTime = 0
            else:
                times.append(zeros(self._gridShape) + (tr.startTime() - beginTime))
            lastStart = tr.startTime()

        # Determine interpTimes
        mode = varDict[""]
        interval = varDict["Interpolation Interval in Hours"]
        duration = varDict["Duration of Grids in Hours"]
        totalDur = timeRange.endTime() - timeRange.startTime()
        interpTimes = []
        prevStart = beginTime
        if self._verbose:
            print "Figuring interpTimes", interval
        while 1:
            newStart = prevStart + interval * 3600
            prevStart = newStart
            if newStart >= lastStart:
                break
            interpTime = newStart - beginTime
            for time in times:
                if interpTime == time.flat[0]:
                    continue
            interpTimes.append(interpTime)
        if self._verbose:
            print "interpTimes"
            for t in interpTimes:
                print "   ", t

        algorithm = varDict["Algorithm"]
        if algorithm == "Cubic Spline":
            self._cubicSpline(beginTime, duration, elementName, grids, times, interpTimes)
        elif algorithm == "Tweening":
            sampleFactor = varDict["Anti-aliasing Supersampling Level"]
            downsampleMethod = varDict["Anti-aliasing Downsample Mode"]
            regionWeighting = varDict["Region Weighting"]
            if varDict["Grid Type"] == "Scalar":
                self._scalarTween(beginTime, duration, elementName, grids, times, interpTimes,
                             sampleFactor, downsampleMethod, regionWeighting)
            else:
                self._discreteTween(beginTime, duration, elementName, grids, times, interpTimes,
                             sampleFactor)

    # Perform interpolation on scalar grids.
    #
    # Basic options:
    # beginTime        -- start of the time range to be interpolated
    # duration         -- length of the time range to 
    # elementName      -- name of the element to be interpolated
    # grids            -- grids to be interpolated
    # times            -- time grids for provided data grids
    # interpTimes      -- times for each interpolated frame
    #
    # Anti-aliasing options:
    # sampleFactor     -- up-sample grids by this factor in each direction
    #                     TODO: Introduce quincunx anti-aliasing, possibly?
    #                           (This might be patented.)
    # downsampleMethod -- what method to use when bringing the grids
    #                     back to native resolution.
    #                     Options are:
    #                       "Flat Averaging" - Average all the points in a
    #                             point's supersampled area to derive the
    #                             downsampled point.
    #                       "Region-Weighted Averaging" - Like "Flat Averaging,"
    #                                        but weight interpolated points
    #                                        higher than background points.
    #                       "Maximizing" - Select the highest value from a
    #                                      point's supersampled area to generate
    #                                      the downsampled point.
    # regionWeighting  -- Parameter for use with "Region-Weighted Averaging"
    #                     downsample method only. By what degree ought in-region
    #                     points be preferred over out-of-region points?
    def _scalarTween(self, beginTime, duration,
                    elementName, grids, times, interpTimes,
                    sampleFactor, downsampleMethod, regionWeighting):
        if self._verbose:
            print "Interpolating scalar grids..."
        
        # Find all points to be interpolated in the source grid. In this case,
        # we say all points with a value greater than 0 are to be interpolated.
        APoints = []
        for x in range(len(grids[0])):
            for y in range(len(grids[0][0])):
                if(grids[0][x][y] > 0):
                    for tx in range(x * sampleFactor, x * sampleFactor + sampleFactor):
                        for ty in range(y * sampleFactor, y * sampleFactor + sampleFactor):
                           APoints.append( (tx, ty, grids[0][x][y]) )
        
        # Likewise, find all points to be interpolated in the destination grid.
        BPoints = []
        for x in range(len(grids[1])):
            for y in range(len(grids[1][0])):
                if(grids[1][x][y] > 0):
                    for tx in range(x * sampleFactor, x * sampleFactor + sampleFactor):
                        for ty in range(y * sampleFactor, y * sampleFactor + sampleFactor):
                           BPoints.append( (tx, ty, grids[1][x][y]) )
                    
        if self._verbose:
            print "Points in A: " + str(len(APoints))
            print "Points in B: " + str(len(BPoints))

        # We always want to interpolate from more points to less points, to get
        # maximum "coverage". Interpolate in "reverse" if need be, and set a
        # flag so we remember to flip the results around in the end.
        toggle = 0
        if len(APoints) < len(BPoints):
            toggle = 1
            x = APoints
            APoints = BPoints
            BPoints = x

        # Generate a random mapping of points in the source grid to points in
        # the destination grid. This works remarkably well, especially for
        # high supersample values. Interestingly, it looks significantly
        # better than many more intelligent-seeming methods (interpolate to
        # nearest point, interpolate by overlaid region).
        mapping = []
        for i in range(len(APoints)):
            mapping.append(random.randrange(0, len(BPoints)))

        # Supersample and interpolate the grids, blowing them up to many times
        # their original resolution. This is incredibly memory-intensive, but
        # when we produce the downsampled output, it is much smoother.
        sampleGrids = []
        for t in range(len(interpTimes)):
            grid = []
            tfa = 1.0 - float((t + 1.0) / (len(interpTimes) + 1.0))
            tfb = float(t + 1.0) / float(len(interpTimes) + 1.0)
            if self._verbose:
                print "tfa=" + str(tfa)
                print "tfb=" + str(tfb)
            for x in range(len(grids[0]) * sampleFactor):
                row = []
                for y in range(len(grids[0][0]) * sampleFactor):
                    row.append(-30)
                grid.append(row)

            for i in range(len(mapping)):
                grid[int(tfa * APoints[i][0] + tfb * BPoints[mapping[i]][0])][int(tfa * APoints[i][1] + tfb * BPoints[mapping[i]][1])] = tfa * APoints[i][2] + tfb * BPoints[mapping[i]][2]

            sampleGrids.append(grid)

        # Downsample the super-sampled grids using the specified algorithm.
        iGrids = []
        for i in range(len(sampleGrids)):
            igrid = []
            for x in range(len(grids[0])):
                irow = []
                for y in range(len(grids[0][0])):
                    if downsampleMethod == "Flat Averaging":
                        value = 0.0
                        count = 0
                        for tx in range(x * sampleFactor, x * sampleFactor + sampleFactor):
                            for ty in range(y * sampleFactor, y * sampleFactor + sampleFactor):
                                value = value + sampleGrids[i][tx][ty]
                                count = count + 1
                        value = value / float(count)

                    elif downsampleMethod == "Region-Weighted Averaging":
                        value = 0.0
                        count = 0
                        for tx in range(x * sampleFactor, x * sampleFactor + sampleFactor):
                            for ty in range(y * sampleFactor, y * sampleFactor + sampleFactor):
                                if sampleGrids[i][tx][ty] > 0:
                                    value = value + regionWeighting * sampleGrids[i][tx][ty]
                                    count = count + regionWeighting
                                else:
                                    value = value + sampleGrids[i][tx][ty]
                                    count = count + 1
                                    
                        if count > 0:
                            value = value / float(count)
                        else:
                            value = -30
                    
                    elif downsampleMethod == "Maximizing":
                        value = sampleGrids[i][x * sampleFactor][y * sampleFactor]
                        for tx in range(x * sampleFactor, x * sampleFactor + sampleFactor):
                            for ty in range(y * sampleFactor, y * sampleFactor + sampleFactor):
                                if (sampleGrids[i][tx][ty] > value):
                                    value = sampleGrids[i][tx][ty]
                                    
                    irow.append(int(value))
                igrid.append(irow)
            iGrids.append(igrid)

        # If we decided above to flip the order of grids in order to
        # maintain our more-points to fewer-points interpolation order,
        # switch them back at this stage.
        if toggle == 1:
            iGrids.reverse()

        # Drop the grids back into the GFE.
        for i in range(len(interpTimes)):
            absTime = beginTime + interpTimes[i]
            gridTimeRange = TimeRange.TimeRange(absTime, absTime + duration * 360)
            self.createGrid("Fcst", elementName, "SCALAR", numpy.asarray(iGrids[i]),
                            gridTimeRange)
    
    # Perform interpolation on discrete grids.
    #
    # Basic options:
    # beginTime        -- start of the time range to be interpolated
    # duration         -- length of the time range to 
    # elementName      -- name of the element to be interpolated
    # grids            -- grids to be interpolated
    # times            -- time grids for provided data grids
    # interpTimes      -- times for each interpolated frame
    #
    # Anti-aliasing options:
    # sampleFactor     -- up-sample grids by this factor in each direction
    #                     TODO: Introduce quincunx anti-aliasing, possibly?
    #                           (This might be patented.)
    #
    # (A number of smoothing options are not available for discrete grids,
    # because some operations--such as averaging their values--do not
    # make sense.)
    def _discreteTween(self, beginTime, duration,
                      elementName, grids, times, interpTimes,
                      sampleFactor):
        if self._verbose:
            print "Interpolating discrete grids (sample=" + str(sampleFactor) + ")"

        # Find all unique "features" (meterological elements) in the
        # source grid and a list of the points comprising each of them
        # to a set. They will be interpolated independently.
        AFeatures = set()
        for x in range(len(grids[0][0])):
            for y in range(len(grids[0][0][0])):
                if not (grids[0][1][grids[0][0][x][y]] in AFeatures):
                    AFeatures.add(grids[0][1][grids[0][0][x][y]])

        # Likewise, for the destination grid.
        BFeatures = set()
        for x in range(len(grids[1][0])):
            for y in range(len(grids[1][0][0])):
                if not (grids[1][1][grids[1][0][x][y]] in BFeatures):
                    BFeatures.add(grids[1][1][grids[1][0][x][y]])       

        # We're going to put our generated grids into discreteGrids, but we'll
        # have to fill them with suitable null values first, and we need to keep
        # them around for multiple loop passes, so declare discreteGrids outside
        # the loop, here.
        discreteGrids = []

        # We only interpolate on features that exist in both A and B. Currently,
        # this fails to do things like turn a heavy rain into a light rain. When
        # we fix that, this is the line to change.
        features = AFeatures.intersection(BFeatures)

        # We're going to define a dictionary for features that's shared amongst
        # interpolated grids. This reduces headache later.
        featureTuples = []
        featureTuples.append(grids[0][1][0])
        counter = 0

        # For each feature, interpolate across all points representing it...
        for feature in features:
            if feature != "<None>" and feature != "<NoCov>:<NoWx>:<NoInten>:<NoVis>:":
                counter = counter + 1
                
                if self._verbose:
                    print "Interpolating on: " + str(feature) + ", " + str(counter)

                # As in scalarTween, select all the relevant points from the
                # source and destination grids.
                APoints = []
                for x in range(len(grids[0][0])):
                    for y in range(len(grids[0][0][0])):
                        if(grids[0][1][grids[0][0][x][y]] == feature):
                            for tx in range(x * sampleFactor, x * sampleFactor + sampleFactor):
                                for ty in range(y * sampleFactor, y * sampleFactor + sampleFactor):
                                   APoints.append( (tx, ty) ) 

                BPoints = []
                for x in range(len(grids[1][0])):
                    for y in range(len(grids[1][0][0])):
                        if(grids[1][1][grids[1][0][x][y]] == feature):
                            for tx in range(x * sampleFactor, x * sampleFactor + sampleFactor):
                                for ty in range(y * sampleFactor, y * sampleFactor + sampleFactor):
                                   BPoints.append( (tx, ty) )

                
                if self._verbose:
                    print "Points in A: " + str(len(APoints))
                    print "Points in B: " + str(len(BPoints))

                ## We want to interpolate from more points to less points,
                ## to get maximum coverage. Interpolate in reverse if need be.
                toggle = 0
                if len(APoints) < len(BPoints):
                    toggle = 1
                    x = APoints
                    APoints = BPoints
                    BPoints = x

                ## Generate a random mapping of points in the source form
                ## to points in the destination form. This works remarkably
                ## well, especially for high supersample values.
                mapping = []
                for i in range(len(APoints)):
                    mapping.append(random.randrange(0, len(BPoints)))

                # As in scalarTween, upsample and dump the results into sampledGrids.
                # This step also initializes the grids to <None> (0), if they haven't
                # been touched yet.
                sampleGrids = []
                for t in range(len(interpTimes)):
                    grid = []
                    tfa = 1.0 - float((t + 1.0) / (len(interpTimes) + 1.0))
                    tfb = float(t + 1.0) / float(len(interpTimes) + 1.0)
                    if self._verbose:
                        print "tfa=" + str(tfa)
                        print "tfb=" + str(tfb)
                    for x in range(len(grids[0][0]) * sampleFactor):
                        row = []
                        for y in range(len(grids[0][0][0]) * sampleFactor):
                            row.append(0)
                        grid.append(row)

                    for i in range(len(mapping)):
                        grid[int(tfa * APoints[i][0] + tfb * BPoints[mapping[i]][0])][int(tfa * APoints[i][1] + tfb * BPoints[mapping[i]][1])] = counter

                    sampleGrids.append(grid)

                # Downsample the grids using maximizing downsample
                # (neither of the others really make sense, given that
                # you can't really average two discrete points. If
                # they do exist, this will only overwrite the grid-stored
                # values if their values are set to 0.
                iGrids = []               
                for i in range(len(sampleGrids)):
                    igrid = []
                    for x in range(len(grids[0][0])):
                        irow = []
                        for y in range(len(grids[0][0][0])):
                            value = sampleGrids[i][x * sampleFactor][y * sampleFactor]
                            for tx in range(x * sampleFactor, x * sampleFactor + sampleFactor):
                                for ty in range(y * sampleFactor, y * sampleFactor + sampleFactor):
                                    if (sampleGrids[i][tx][ty] > value):
                                        value = sampleGrids[i][tx][ty]
                                            
                            irow.append(int(value))
                        igrid.append(irow)
                    iGrids.append(igrid)

                if toggle == 1:
                    iGrids.reverse()

                if len(discreteGrids) == 0:
                    for x in iGrids:
                        discreteGrids.append(x)
                else:
                    for i in range(len(discreteGrids)):
                        for x in range(len(discreteGrids[i])):
                            for y in range(len(discreteGrids[i][x])):
                                if iGrids[i][x][y] != 0:
                                    discreteGrids[i][x][y] = iGrids[i][x][y]

                # Now, drop the finished grids, one by one, into a finished queue.
                featureTuples.append(feature)

        # ...and add them to the GFE.
        for i in range(len(interpTimes)):
            absTime = beginTime + interpTimes[i]
            gridTimeRange = TimeRange.TimeRange(absTime, absTime + duration * 360)
            self.createGrid("Fcst", elementName, "DISCRETE", (numpy.asarray(discreteGrids[i]), featureTuples),
                            gridTimeRange)
    
    def _cubicSpline(self, beginTime, duration, elementName, grids, times, interpTimes):
            
        # STEP 1: Create coefficients for cubic spline curve
        # zCoefs : List of cubic spline coefficient grids computed to fit the
        #          curve defined by grids and times
        # n      : length of grids - 1.  

        # Determine coefficients
        n = len(grids) - 1
        #print "Calculating coeffs"
        #print "n", n
        #print "times, grids lengths", len(times), len(grids)                        
        zCoefs = self._spline3_coef(n, times, grids)
        #print "Done with coeffs"
        
        # STEP 2: Create interpolated grids using coefficients
        # interpTimes : List of Times for which we want interpolated grids
        # xGrids : List of interpolated Grids
           
        # Create interpolated grids 
        if self._verbose:
            print "Interpolating grids"
        for interpTime in interpTimes:
            x = zeros(self._gridShape) + interpTime
            xGrid = self._spline3_eval(n, times, grids, zCoefs, x)
            absTime = beginTime + interpTime
            gridTimeRange = TimeRange.TimeRange(
                absTime, absTime + duration * 3600)
            self.createGrid("Fcst", elementName, "SCALAR", xGrid,
                            gridTimeRange)
        if self._verbose:
            print "Done creating new grids"

    def _spline3_coef(self, n, t, y):
        gridShape = y[0].shape
        # These will get filled in later with grids as values
        # They are just place holders
        h=[0] * n
        b=[0] * n
        u=[0] * n
        v=[0] * n
        z=[0] * (n+1)                
        # Calculate h and b
        #   range 0 thru n-1
        #print "Calculating h and b"
        for i in range(n):
            #print "i", i
            h[i] = t[i+1] - t[i]
            b[i] = (y[i+1] - y[i])/h[i]
            #print "h, b", h[i][0][0], b[i][0][0]
        # Calculate u and v as functions of h and b
        #   range 1 thru n-1
        #print "Calculating u and v"
        u[1] = (2*(h[0] + h[1]))
        v[1] = (6*(b[1]-b[0]))
        #print "u1, v1", u[1][0][0], v[1][0][0]
        for i in range(2, n):
            #print "i", i
            u[i] = (2*(h[i]+h[i-1]) - h[i-1]**2/u[i-1])
            v[i] = (6*(b[i]-b[i-1]) - h[i-1]*v[i-1]/u[i-1])
            #print "u, v", u[i][0][0], v[i][0][0]
        # Calculate z
        #   range 0 thru n
        z[n] = zeros(gridShape)
        #print "Calculating z"
        for i in range(n-1, 0, -1):
            #print "i", i
            if type(u[i]) is types.IntType:
                print "u", u[i]
            z[i] = (v[i] - h[i]*z[i+1])/u[i]
            #print "z", z[i][0][0]
        z[0] = zeros(gridShape)
        return z

    def _spline3_eval(self, n, t, y, z, x):
        for i in range(n-1, 0, -1):
            #print "x, t", x[0][0], t[i][0][0]
            if x[0][0]-t[i][0][0] >= 0:
                break
        #print "using i", i
        h = t[i+1]-t[i]
        tmp = (z[i]/2) + (x-t[i]) * (z[i+1]-z[i])/(6*h)
        tmp = -(h/6)*(z[i+1]+2*z[i]) + (y[i+1]-y[i])/h + (x-t[i]) * tmp
        return y[i] + (x-t[i]) * tmp

 
