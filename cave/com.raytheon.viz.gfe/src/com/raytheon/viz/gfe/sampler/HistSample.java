/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.gfe.sampler;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.vecmath.Vector2f;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherSubKey;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.types.MutableInteger;

/**
 * Contains a complete histogram for a single grid and parameter
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 5, 2008	1167		mnash	    Initial creation
 * Sep 3, 2008  1283        njensen     Fixed issues
 * Sep 9, 2008  1283        njensen     Implemented sample methods
 * May 29, 2009 2159        rjpeter     Optimized sample methods.
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class HistSample {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(HistSample.class);

    private static final double DEG_TO_RAD = Math.PI / 180;

    private static final double RAD_TO_DEG = 180 / Math.PI;

    private int _numSamplePoints;

    private TimeRange _timeRange;

    private float _resolution;

    public boolean _subkeymode;

    private List<HistPair> _histPairs = new ArrayList<HistPair>();

    private List<HistValue> _rawPoints = new ArrayList<HistValue>();

    // cached values
    private HistValue _average;

    private boolean _averageMagDir;

    private HistValue _stdDev;

    private HistValue _mostCommonValue;

    private HistValue _middleValue;

    private HistValue _absoluteMin;

    private HistValue _absoluteMax;

    private HistValue _moderatedMin;

    private int _moderatedMinPercent;

    private HistValue _moderatedMax;

    private int _moderatedMaxPercent;

    private HistValue _moderatedAverage;

    private int _moderatedAverageMinPercent;

    private int _moderatedAverageMaxPercent;

    public boolean _moderatedAverageMagDir;

    private HistValue _stdDevAvg;

    private float _stdDevAvgMinStdD;

    private float _stdDevAvgMaxStdD;

    public boolean _stdDevMaxDir;

    private HistValue _stdDevMax;

    private float _stdDevMaxD;

    private HistValue _stdDevMin;

    private float _stdDevMinD;

    private HistValue _binnedMostCommonValue;

    private float _binnedMostCommonValueFactor;

    private List<HistPair> _binnedHistPairs;

    private float _binnedHistPairsValue;

    private boolean _stdDevMagDir;

    public TimeRange timeRange;

    public List<HistPair> histPairs = new ArrayList<HistPair>();

    public IGridSlice gridSlice;

    public Grid2DBit sampleArea;

    public IGridSlice grid;

    public Grid2DBit area;

    public boolean cachePoints;

    public Point ll;

    public Point ur;

    /**
     * Description : Default constructor for HistSample
     */
    public HistSample() {
        _numSamplePoints = 0;
        _resolution = 0;
    }

    /**
     * Description : Constructor far HistSample taking a histogram in the form
     * of a time range and a sequence of HistPairs, stores the information in
     * private data. Counts up the samples and stores that in _numSamplePoints.
     * 
     * @param timeRange
     * @param histPairs
     */
    public HistSample(TimeRange timeRange, List<HistPair> histPairs) {
        _timeRange = timeRange;
        _histPairs = histPairs;
        _numSamplePoints = 0;
        _resolution = 1.0f;
        _subkeymode = false;
        _averageMagDir = false;
        _moderatedMinPercent = 0;
        _moderatedMaxPercent = 0;
        _moderatedAverageMinPercent = 0;
        _moderatedAverageMaxPercent = 0;
        _moderatedAverageMagDir = false;
        _stdDevAvgMinStdD = 0.0f;
        _stdDevAvgMaxStdD = 0.0f;
        _stdDevMagDir = false;
        _stdDevMaxD = 0.0f;
        _stdDevMinD = 0.0f;
        _binnedMostCommonValue = new HistValue(0.0f);
        _binnedHistPairsValue = 0.0f;

        // count up the number of sample points
        _numSamplePoints = countSamplePoints();
    }

    /**
     * Description : Constructor taking a GridSlice and Grid2DBit. If the
     * gridslice is not valid, then this constructor behaves like the default
     * constructor. If cachePoints is set, then the raw grid points will be
     * cached. Sets number of sample points to zero. Calls sampleGrid() to
     * sample the grid. If successful, stores the time range and counts up the
     * number of sample points.
     * 
     * @param gridSlice
     * @param sampleArea
     * @param cachePoints
     * @param subkeymode
     */
    public HistSample(IGridSlice gridSlice, Grid2DBit sampleArea,
            boolean cachePoints, boolean subkeymode) {
        _numSamplePoints = 0;
        _resolution = 1;
        _subkeymode = subkeymode;
        _averageMagDir = false;
        _moderatedMinPercent = 0;
        _moderatedMaxPercent = 0;
        _moderatedAverageMinPercent = 0;
        _moderatedAverageMaxPercent = 0;
        _moderatedAverageMagDir = false;
        _stdDevAvgMinStdD = 0.0f;
        _stdDevAvgMaxStdD = 0.0f;
        _stdDevMagDir = false;
        _stdDevMaxD = 0.0f;
        _stdDevMinD = 0.0f;
        _binnedMostCommonValue = new HistValue(0.0f);
        _binnedHistPairsValue = 0.0f;

        _timeRange = gridSlice.getValidTime();
        sampleGrid(gridSlice, sampleArea, cachePoints);
        _numSamplePoints = countSamplePoints();
    }

    /**
     * Description : Returns the average of the samples for SCALAR. Returns the
     * average magnitude/direction for VECTOR. The most common value is provided
     * for WEATHER. The most common value for DISCRETE. For vector, if
     * separateMagDir is true, the magnitude is averaged separately from the
     * direction.
     * 
     * @param separateMagDir
     * @return
     */
    public final HistValue average(boolean separateMagDir) {

        if (separateMagDir == _averageMagDir && _average != null) {
            return _average;
        }

        if (_histPairs.size() == 0) {
            return _average; // HistValue()
        }

        HistSample hs = this;
        hs._averageMagDir = separateMagDir; // save

        switch (_histPairs.get(0).value().dataType()) {
        case SCALAR: {
            float sum = 0.0f;
            int count = 0;
            for (int i = 0; i < _histPairs.size(); i++) {
                sum += _histPairs.get(i).count()
                        * _histPairs.get(i).value().scalar();
                count += _histPairs.get(i).count();
            }
            hs._average = new HistValue(binit(sum / count, _resolution));
            return _average;
        }

        case VECTOR: {
            float sum = 0.0f;
            int count = 0;
            float uSum = 0.0f;
            float vSum = 0.0f;
            if (separateMagDir) {
                for (int i = 0; i < _histPairs.size(); i++) {
                    int tCount = _histPairs.get(i).count();
                    sum += tCount * _histPairs.get(i).value().magnitude();
                    count += tCount;
                    uSum += tCount
                            * Math.sin(_histPairs.get(i).value().direction()
                                    * (Math.PI / 180));
                    vSum += tCount
                            * Math.cos(_histPairs.get(i).value().direction()
                                    * (Math.PI / 180));
                }
                float mag = binit(sum / count, _resolution);
                float dir = binit(
                        (float) (Math.atan2(uSum, vSum) * RAD_TO_DEG), 10.0f);
                while (dir < 0.0) {
                    dir += 360.0;
                }
                while (dir >= 360) {
                    dir -= 360.0;
                }
                if (dir == 360.0) {
                    dir = 0.0f;
                }
                hs._average = new HistValue(mag, dir);
                return _average;
            } else {
                for (int i = 0; i < _histPairs.size(); i++) {
                    int tCount = _histPairs.get(i).count();
                    count += tCount;
                    uSum += tCount
                            * Math.sin(_histPairs.get(i).value().direction()
                                    * (Math.PI / 180))
                            * _histPairs.get(i).value().magnitude();
                    vSum += tCount
                            * Math.cos(_histPairs.get(i).value().direction()
                                    * (Math.PI / 180))
                            * _histPairs.get(i).value().magnitude();
                    float v = uSum / count;
                    float u = vSum / count;
                    float mag = binit((float) Math.sqrt(u * u + v * v),
                            _resolution);
                    float dir = binit((float) (Math.atan2(u, v) * RAD_TO_DEG),
                            10.0f);
                    while (dir < 0.0) {
                        dir += 360.0;
                    }
                    while (dir >= 360.0) {
                        dir -= 360.0;
                    }
                    if (dir == 360.0) {
                        dir = 0.0f;
                    }
                    hs._average = new HistValue(mag, dir);
                    return _average;
                }
            }
        }
        case WEATHER: {
            hs._average = mostCommonValue();
            return _average;
        }

        case DISCRETE: {
            hs._average = mostCommonValue();
            return _average;
        }

        default:
            return _average; // unknown type, default HistValue
        }
    }

    /**
     * Description : the square root function
     * 
     * @param val
     * @return
     */
    public static double squareRoot(double val) {
        Double rval = new Double(Math.sqrt(val));
        if (rval.isNaN()) {
            return 0.0;
        }
        return rval;
    }

    /**
     * Description : Returns the standard deviation of the samples for SCALAR.
     * Returns the standard deviation of each component separately for VECTOR.
     * Should not be called for WEATHER. For vector, if separate MagDir is true,
     * the magnitude is averaged separately from the direction.
     * 
     * @return
     */
    public final HistValue stdDev() {
        if (_histPairs.size() == 0) {
            return _stdDev; // no average possible, empty HistValue()
        }

        // cached?
        if (_stdDev != null) {
            return _stdDev;
        }

        HistSample hs = this;
        switch (_histPairs.get(0).value().dataType()) {
        case SCALAR: {
            double sum = 0.0;
            double sum2 = 0.0;
            long count = 0;

            for (int i = 0; i < _histPairs.size(); i++) {
                double x = _histPairs.get(i).value().scalar();
                long cnt = _histPairs.get(i).count();
                sum += cnt * x;
                sum2 += cnt * x * x;
                count += cnt;
            }

            float dev = 0.0f;
            if (count != 0) {
                dev = (float) squareRoot((sum2 - (sum * sum) / count) / count);
            }
            hs._stdDev = new HistValue(dev);
            return _stdDev;
        }
        case VECTOR: {
            double sum = 0.0;
            double sum2 = 0.0;
            long count = 0;
            double uSum = 0.0;
            double vSum = 0.0;
            double uSum2 = 0.0;
            double vSum2 = 0.0;

            for (int i = 0; i < _histPairs.size(); i++) {
                long tCount = _histPairs.get(i).count();
                double x = _histPairs.get(i).value().magnitude();
                sum += tCount * x;
                sum2 += tCount * x * x;
                count += tCount;
                double u = Math.sin(_histPairs.get(i).value().direction()
                        * (Math.PI / 180));
                double v = Math.cos(_histPairs.get(i).value().direction()
                        * (Math.PI / 180));
                uSum += tCount * u;
                uSum2 = tCount * u * u;
                vSum += tCount * v;
                vSum2 = tCount * v * v;
            }

            float dev = 0.0f;
            float udev = 0.0f;
            @SuppressWarnings("unused")
            float vdev = 0.0f;

            if (count != 0) {
                dev = (float) squareRoot((sum2 - (sum * sum) / count) / count);
                udev = (float) squareRoot((uSum2 - (uSum * uSum) / count)
                        / count);
                vdev = (float) squareRoot((vSum2 - (vSum * vSum) / count)
                        / count);
            }
            hs._stdDev = new HistValue(dev, udev + udev);
            return _stdDev;
        }

        case WEATHER:
            return _stdDev; // empty HistValue

        case DISCRETE:
            return _stdDev; // empty HistValue

        default:
            return _stdDev; // empty HistValue
        }
    }

    /**
     * Description : Returns the most common value in the samples. Looks through
     * the HistPair's for the maximum count value and returns it. In the case
     * where more than one entry shares the maximum count value, then only the
     * highest value (sort order) value will be returned.
     * 
     * @return
     */
    public final HistValue mostCommonValue() {
        if (_histPairs.size() == 0) {
            return _mostCommonValue; // default HistValue()
        }

        // cached?
        if (_mostCommonValue != null) {
            return _mostCommonValue;
        }

        // search through HistPair's for the maximum count
        HistSample hs = this;
        int index = 0;
        for (int i = 1; i < _histPairs.size(); i++) {
            if (_histPairs.get(i).count() >= _histPairs.get(index).count()) {
                index = i;
            }
        }

        // found the last maximum count
        hs._mostCommonValue = _histPairs.get(index).value();
        return _mostCommonValue;
    }

    /**
     * Description : Returns the most common values in the samples, based ona
     * resolution. Bins the HistPairs to the given resolution, then looks
     * through the HistPair's for the maximum count value and returns it. In the
     * case where more than one entyr shares the maximum count value, then only
     * the highest value (sort order) value will be returned. Works only on
     * SCALAR and VECTOR.
     * 
     * @param resolution
     * @return
     */
    public final HistValue mostCommonValue(float resolution) {
        if (_histPairs.size() == 0) {
            return _mostCommonValue; // default HistValue()
        }

        // cached?
        if ((_binnedMostCommonValue != null)
                && resolution == _binnedMostCommonValueFactor) {
            return _binnedMostCommonValue;
        }

        // get the binned hist pairs
        final List<HistPair> bhp = binnedHistogram(resolution);

        // search through binned HistPair's for the maximum count
        HistSample hs = this;
        int index = 0;
        for (int i = 1; i < bhp.size(); i++) {
            if (bhp.get(i).count() >= bhp.get(index).count()) {
                index = i;
            }
        }

        // found the last maximum count
        hs._binnedMostCommonValue = bhp.get(index).value();
        hs._binnedMostCommonValueFactor = resolution;

        return _binnedMostCommonValue;

    }

    /**
     * Finds and returns the middle value associated with the sample. The middle
     * value is that value that is halfway between the lowest and highest in
     * terms of count, and not value. This is a no-op for WEATHER/DISCRETE.
     * 
     * @return
     */
    public final HistValue middleValue() {
        if (_histPairs.size() == 0
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.WEATHER)
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.DISCRETE)) {
            return _middleValue; // default HistValue()
        }

        // cached?
        if (_middleValue != null) {
            return _middleValue;
        }

        HistSample hs = this;
        int runningCount = 0;
        for (int i = 0; i < _histPairs.get(i).count(); i++) {
            runningCount += _histPairs.get(i).count();
            if ((float) runningCount / (float) _numSamplePoints > 0.50) {
                hs._middleValue = _histPairs.get(i).value();
                return _middleValue;
            }
        }
        hs._middleValue = _histPairs.get(_histPairs.size() - 1).value();
        return _middleValue;
    }

    /**
     * Description : Returns the absolute minimum value for the sample points.
     * This is a no-op for WEATHER/DISCRETE. Only the magnitude component for
     * VECTOR is used for comparison.
     * 
     * @return
     */
    public final HistValue absoluteMin() {
        if (_histPairs.size() == 0
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.WEATHER)
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.DISCRETE)) {
            return _absoluteMin; // no minimum possible
        }

        // cached?
        if (_absoluteMin != null) {
            return _absoluteMin;
        }

        // the 1st value will be the minimum value
        _absoluteMin = _histPairs.get(0).value();
        return _absoluteMin;
    }

    /**
     * Description : REturns the absolute maximum value for the sample points.
     * This is a no-op for WEATHER/DISCRETE. Only the magnitude component for
     * VECTOR is used for comparison.
     * 
     * @return
     */
    public final HistValue absoluteMax() {
        if (_histPairs.size() == 0
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.WEATHER)
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.DISCRETE)) {
            return _absoluteMax; // default HistValue
        }

        // cached?
        if (_absoluteMax != null) {
            return _absoluteMax;
        }

        // the last value will be the maximum value
        _absoluteMax = _histPairs.get(_histPairs.size() - 1).value();
        return _absoluteMax;
    }

    /**
     * Description : Returns the representative average of the samples for
     * SCALAR. Returns the representative magnitude/direction for VECTOR. The
     * most common value is provide for WEATERH. Outliers are eliminated.
     * Percent ranges from 0 to 50. For vector, if separateMagDir is true, the
     * magnitude is averaged separately from the direction.
     * 
     * @param minpercent
     * @param maxpercent
     * @param separateMagDir
     * @return
     */
    public final HistValue moderatedAverage(int minpercent, int maxpercent,
            boolean separateMagDir) {
        HistValue bogus = new HistValue();
        HistSample hs = this;
        if (_histPairs.size() == 0 || minpercent < 0 || minpercent > 50
                || maxpercent < 0 || maxpercent > 50) {
            hs._moderatedAverage = new HistValue();
            hs._moderatedAverageMinPercent = hs._moderatedAverageMaxPercent = 0;
            return _moderatedAverage;
        }

        // cached value
        if (minpercent == _moderatedAverageMinPercent
                && maxpercent == _moderatedAverageMaxPercent
                && _moderatedAverage != null) {
            return _moderatedAverage;
        }

        // cache values
        hs._moderatedAverageMinPercent = minpercent;
        hs._moderatedAverageMaxPercent = maxpercent;

        float runningCount = 0;
        int minlimitCount = (int) (minpercent * _numSamplePoints / 100.0);
        int maxlimitCount = (int) ((100 - maxpercent) * _numSamplePoints / 100.0);

        switch (_histPairs.get(0).value().dataType()) {
        case SCALAR: {
            float sum = 0.0f;
            int count = 0;
            for (int i = 0; i < _histPairs.size(); i++) {
                int minVCount = (int) runningCount + 1; // indices for this pair
                int maxVCount = (int) runningCount + _histPairs.get(i).count();

                int numInclude = 0;
                if (minlimitCount > maxVCount || maxlimitCount < minVCount) {
                    numInclude = 0;
                } else {
                    numInclude = Math.max(minlimitCount, minVCount)
                            - Math.min(maxlimitCount, maxVCount) + 1;
                }

                if (numInclude != 0) {
                    sum += numInclude * _histPairs.get(i).value().scalar();
                    count += numInclude;
                }
                runningCount += _histPairs.get(i).count();
            }
            hs._moderatedAverage = new HistValue(
                    binit(sum / count, _resolution));
            return _moderatedAverage;
        }
        case VECTOR: {
            float sum = 0.0f;
            int count = 0;
            float uSum = 0.0f;
            float vSum = 0.0f;
            if (separateMagDir) {
                for (int i = 0; i < _histPairs.size(); i++) {
                    int minVCount = (int) runningCount + 1; // indexes for this
                    // pair
                    int maxVCount = (int) runningCount
                            + _histPairs.get(i).count();

                    int numInclude = 0;
                    if (minlimitCount > maxVCount || maxlimitCount < minVCount) {
                        numInclude = 0;
                    } else {
                        numInclude = Math.max(minlimitCount, minVCount)
                                - Math.min(maxlimitCount, maxVCount) + 1;
                    }
                    if (numInclude != 0) {
                        int tCount = numInclude;
                        sum += tCount * _histPairs.get(i).value().magnitude();
                        count += tCount;
                        uSum += tCount
                                * Math.sin(_histPairs.get(i).value()
                                        .direction()
                                        * (Math.PI / 180));
                        vSum += tCount
                                * Math.cos(_histPairs.get(i).value()
                                        .direction()
                                        * (Math.PI / 180));
                    }
                    runningCount += _histPairs.get(i).count();
                }
                float mag = binit(sum / count, _resolution);
                float dir = binit(
                        (float) (Math.atan2(uSum, vSum) * RAD_TO_DEG), 10.0f);
                while (dir < 0.0) {
                    dir += 360.0;
                }
                while (dir >= 360.0) {
                    dir -= 360.0;
                }
                if (dir == 360.0) {
                    dir = 0.0f;
                }
                hs._moderatedAverage = new HistValue(mag, dir);
                return _moderatedAverage;
            } else {
                for (int i = 0; i < _histPairs.size(); i++) {
                    int minVCount = (int) runningCount + 1; // indexes for this
                    // pair
                    int maxVCount = (int) runningCount
                            + _histPairs.get(i).count();

                    int numInclude = 0;
                    if (minlimitCount > maxVCount || maxlimitCount < minVCount) {
                        numInclude = 0;
                    } else {
                        numInclude = Math.max(minlimitCount, minVCount)
                                - Math.min(maxlimitCount, maxVCount) + 1;
                    }

                    if (numInclude != 0) {
                        int tCount = _histPairs.get(i).count();
                        count += tCount;
                        uSum += tCount
                                * Math.sin(_histPairs.get(i).value()
                                        .direction()
                                        * DEG_TO_RAD)
                                * _histPairs.get(i).value().magnitude();
                        vSum += tCount
                                * Math.cos(_histPairs.get(i).value()
                                        .direction()
                                        * DEG_TO_RAD)
                                * _histPairs.get(i).value().magnitude();
                    }
                    runningCount += _histPairs.get(i).count();
                }
                float u = uSum / count;
                float v = vSum / count;
                float mag = binit((float) Math.sqrt(u * u + v * v), _resolution);
                float dir = binit((float) (Math.atan2(u, v) * RAD_TO_DEG),
                        10.0f);
                while (dir < 0.0) {
                    dir += 360.0;
                }
                while (dir >= 360.0) {
                    dir -= 360.0;
                }
                if (dir == 360.0) {
                    dir = 0.0f;
                }
                hs._moderatedAverage = new HistValue(mag, dir);
                return _moderatedAverage;
            }
        }
        case WEATHER: {
            hs._moderatedAverage = mostCommonValue();
            return _moderatedAverage;
        }
        case DISCRETE: {
            hs._moderatedAverage = mostCommonValue();
            return _moderatedAverage;
        }
        default: {
            return bogus;
        }
        }
    }

    /**
     * Description : Returns the representative minimum value for the sample
     * points. this is a no-op for WEATHER/DISCRETE. Percent should be between 0
     * and 50. This routine eliminates the bottom xx% of sample values and
     * returns that value.
     * 
     * @param percent
     * @return
     */
    public final HistValue moderatedMin(int percent) {
        HistSample hs = this;
        if (_histPairs.size() == 0 || percent < 0 || percent > 50
                || _histPairs.get(0).value().dataType() == GridType.WEATHER
                || _histPairs.get(0).value().dataType() == GridType.DISCRETE) {
            hs._moderatedMin = new HistValue();
            hs._moderatedMinPercent = 0;
            return _moderatedMin; // default histValue()
        }

        // cached?
        if (_moderatedMinPercent == percent && _moderatedMin != null) {
            return _moderatedMin;
        }

        hs._moderatedMinPercent = percent;

        float runningCount = 0;
        float limitCount = (float) (percent * _numSamplePoints / 100.0);

        for (int i = 0; i < _histPairs.size(); i++) {
            runningCount += _histPairs.get(i).count();
            if (runningCount > limitCount) {
                hs._moderatedMin = _histPairs.get(i).value();
                return _moderatedMin;
            }
        }
        hs._moderatedMin = _histPairs.get(_histPairs.size() - 1).value();
        return _moderatedMin;
    }

    /**
     * Description : Returns the representative maximu value for the sample
     * points. This is a no-op for WEATHER/DISCRETE. Percent should be between 0
     * and 50. This routine eliminates the top 15% of sample values and returns
     * that value.
     * 
     * @param percent
     * @return
     */
    public final HistValue moderatedMax(int percent) {
        HistSample hs = this;
        if (_histPairs.size() == 0
                || percent < 0
                || percent > 50
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.WEATHER)
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.DISCRETE)) {
            hs._moderatedMaxPercent = 0;
            hs._moderatedMax = new HistValue();
            return _moderatedMax; // default histValue()
        }

        // cached?
        if (_moderatedMax != null && _moderatedMaxPercent == percent) {
            return _moderatedMax;
        }

        hs._moderatedMaxPercent = percent;

        float runningCount = 0;
        float limitCount = (float) (percent * _numSamplePoints / 100.0);

        for (int i = _histPairs.size() - 1; i >= 0; i--) {
            runningCount += _histPairs.get(i).count();
            if (runningCount > limitCount) {
                hs._moderatedMax = _histPairs.get(i).value();
                return _moderatedMax;
            }
        }
        hs._moderatedMax = _histPairs.get(_histPairs.size() - 1).value();
        return _moderatedMax;
    }

    /**
     * Description : Returns the representative average of the samples for
     * SCALAR. Returns the representative magnitude/direction for VECTOR. The
     * most common value is provided for WEATHER/DISCRETE. Outliers are
     * eliminated based on standard deviation. For vector, if separateMagDir is
     * true, the magnitude is averaged separately from the direction.
     * 
     * @param minStdD
     * @param maxStdD
     * @param separateMagDir
     * @return
     */
    public final HistValue stdDevAvg(float minStdD, float maxStdD,
            boolean separateMagDir) {
        HistValue bogus = new HistValue();
        HistSample hs = this;

        if (_histPairs.size() == 0 || minStdD < 0 || maxStdD < 0) {
            hs._stdDevAvgMinStdD = hs._stdDevAvgMaxStdD = 0;
            hs._stdDevMagDir = false;
            hs._stdDevAvg = new HistValue();
            return _stdDevAvg;
        }

        // use cached value if possible
        if (_stdDevAvg != null && _stdDevAvgMinStdD == minStdD
                && _stdDevAvgMaxStdD == maxStdD
                && separateMagDir == _stdDevMagDir) {
            return _stdDevAvg;
        }

        hs._stdDevAvgMinStdD = minStdD;
        hs._stdDevAvgMaxStdD = maxStdD;
        hs._stdDevMagDir = separateMagDir;

        switch (_histPairs.get(0).value().dataType()) {
        case SCALAR: {
            float sDev = stdDev().scalar();
            float avg = average(true).scalar();
            float minValue = avg - minStdD * sDev;
            float maxValue = avg + maxStdD * sDev;
            float sum = 0.0f;
            int count = 0;
            for (int i = 0; i < _histPairs.size(); i++) {
                float v = _histPairs.get(i).value().scalar();
                if (v >= minValue && v <= maxValue) {
                    sum += _histPairs.get(i).count() * v;
                    count += _histPairs.get(i).count();
                }
            }
            if (count == 0) {
                return average(true);
            }
            hs._stdDevAvg = new HistValue(binit(sum / count, _resolution));
            return _stdDevAvg;
        }
        case VECTOR: {
            float sDev = stdDev().scalar();
            float avg = average(true).scalar();
            float minValue = avg - minStdD * sDev;
            float maxValue = avg + maxStdD * sDev;
            float sum = 0.0f;
            int count = 0;
            float uSum = 0.0f;
            float vSum = 0.0f;
            if (separateMagDir) {
                for (int i = 0; i < _histPairs.size(); i++) {
                    float v = _histPairs.get(i).value().magnitude();
                    if (v >= minValue && v <= maxValue) {
                        int tCount = _histPairs.get(i).count();
                        sum += tCount * _histPairs.get(i).value().magnitude();
                        count += tCount;
                        uSum += tCount
                                * Math.sin(_histPairs.get(i).value()
                                        .direction()
                                        * DEG_TO_RAD);
                        vSum += tCount
                                * Math.cos(_histPairs.get(i).value()
                                        .direction()
                                        * DEG_TO_RAD);
                    }
                }
                if (count == 0) {
                    return average(true);
                }
                float mag = binit(sum / count, _resolution);
                float dir = binit(
                        (float) (Math.atan2(uSum, vSum) * RAD_TO_DEG), 10.0f);
                while (dir < 0.0) {
                    dir += 360.0;
                }
                while (dir >= 360.0) {
                    dir -= 360.0;
                }
                if (dir == 360.0) {
                    dir = 0.0f;
                }
                hs._stdDevAvg = new HistValue(mag, dir);
                return _stdDevAvg;
            } else {
                for (int i = 0; i < _histPairs.size(); i++) {
                    float v = _histPairs.get(i).value().magnitude();
                    if (v >= minValue && v <= maxValue) {
                        int tCount = _histPairs.get(i).count();
                        count += tCount;
                        uSum += tCount
                                * Math.sin(_histPairs.get(i).value()
                                        .direction()
                                        * DEG_TO_RAD)
                                * _histPairs.get(i).value().magnitude();
                        vSum += tCount
                                * Math.cos(_histPairs.get(i).value()
                                        .direction()
                                        * DEG_TO_RAD)
                                * _histPairs.get(i).value().magnitude();
                    }
                }
                if (count == 0) {
                    return average(true);
                }
                float u = uSum / count;
                float v = vSum / count;
                float mag = binit((float) (Math.sqrt(u * u + v * v)),
                        _resolution);
                float dir = binit((float) (Math.atan2(u, v) * RAD_TO_DEG),
                        10.0f);
                while (dir < 0.0) {
                    dir += 360.0;
                }
                while (dir >= 360.0) {
                    dir -= 360.0;
                }
                if (dir == 360.0) {
                    dir = 0.0f;
                }
                hs._stdDevAvg = new HistValue(mag, dir);
                return _stdDevAvg;
            }
        }

        case WEATHER:
            return mostCommonValue();

        case DISCRETE:
            return mostCommonValue();

        default:
            return bogus;
        }
    }

    /**
     * Description : Returns the representative minimum value for the sample
     * points. This is a no-op for WEATHER/DISCRETE. Based on standard
     * deviations.
     * 
     * @param stdD
     * @return
     */
    public final HistValue stdDevMin(float stdD) {
        HistSample hs = this;

        if (_histPairs.size() == 0
                || stdD < 0
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.WEATHER)
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.DISCRETE)) {
            hs._stdDevMinD = 0.0f;
            hs._stdDevMin = new HistValue();
            return _stdDevMin;
        }

        // use cached value if possible
        if (_stdDevMin != null && _stdDevMinD == stdD) {
            return _stdDevMin;
        }

        hs._stdDevMinD = stdD;

        float sDev = stdDev().scalar();
        float avg = average(true).scalar();
        float minValue = avg - stdD * sDev;
        if (_histPairs.get(0).value().scalar() > minValue) {
            minValue = _histPairs.get(0).value().scalar();
        }
        hs._stdDevMin = new HistValue(minValue);
        return _stdDevMin;
    }

    /**
     * Description : Returns the representative maximum value for the sample
     * points. This is a no-op for WEATHER/DISCRETE. Based on standard
     * deviations.
     * 
     * @param stdD
     * @return
     */
    public final HistValue stdDevMax(float stdD) {
        HistSample hs = this;

        if (_histPairs.size() == 0
                || stdD < 0
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.DISCRETE)
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.WEATHER)) {
            hs._stdDevMaxD = 0.0f;
            hs._stdDevMax = new HistValue();
            return _stdDevMax;
        }

        // use cached value if possible
        if (_stdDevMax != null && _stdDevMaxD == stdD) {
            return _stdDevMax;
        }
        hs._stdDevMaxD = stdD;

        float sDev = stdDev().scalar();
        float avg = average(true).scalar();
        float maxValue = avg + stdD * sDev;
        if (_histPairs.get(_histPairs.size() - 1).value().scalar() < maxValue) {
            maxValue = _histPairs.get(_histPairs.size() - 1).value().scalar();
        }

        hs._stdDevMax = new HistValue(maxValue);
        return _stdDevMax;
    }

    /**
     * OUtputs the histogram for this grid, but binned by the specified float
     * value. This only applies to SCALAR and VECTOR data.
     * 
     * @param resolution
     * @return
     */
    public List<HistPair> binnedHistogram(float resolution) {
        HistSample hs = this;
        if (_histPairs.size() == 0
                || resolution <= 0.0
                || _histPairs.get(0).value().dataType().equals(GridType.NONE)
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.WEATHER)
                || _histPairs.get(0).value().dataType()
                        .equals(GridType.DISCRETE)) {
            hs._binnedHistPairs.clear();
            hs._binnedHistPairsValue = 0.0f;
            return _binnedHistPairs;
        }

        // cached value
        if (resolution == _binnedHistPairsValue) {
            return _binnedHistPairs;
        }

        // cache values
        hs._binnedHistPairsValue = resolution;

        // calculate the binned histogram for SCALAR
        if (_histPairs.get(0).value().dataType().equals(GridType.SCALAR)) {
            Map<Float, Integer> hp = new HashMap<Float, Integer>();
            for (int i = 0; i < _histPairs.size(); i++) {
                hp.put(binit(_histPairs.get(i).value().scalar(), resolution),
                        hp.get(binit(_histPairs.get(i).value().scalar(),
                                resolution)) + _histPairs.get(i).count());
            }

            int i = 0;
            Iterator<?> pos = hp.entrySet().iterator();
            Iterator<?> pos1 = hp.entrySet().iterator();
            while (pos.hasNext()) {
                pos1.next();
                hs._binnedHistPairs.set(i++,
                        _histPairs.get(pos.next().hashCode()));
                pos.next();
            }
        }
        // calculate the binned histogram for VECTOR
        else {
            Map<Float, Float> hp = new HashMap<Float, Float>();
            for (int i = 0; i < _histPairs.size(); i++) {
                // Map<Float, Float> pair = new HashMap<Float, Float>();
                // hp.set(hp.put(binit(_histPairs.get(i).value().magnitude(),
                // resolution),_histPairs.get(i).value().direction()),);
                /* TODO */// hp.set(hp.get(hp.put(binit(_histPairs.get(i).value().magnitude(),resolution),
                // _histPairs.get(i).value().direction()),hp.get(hp.put(binit(_histPairs.get(i).value().magnitude(),resolution),
                // _histPairs.get(i).value().direction())+ 1);
            }

            // Now store our histogram in SeqOf form.
            Iterator<?> pos = hp.entrySet().iterator();
            Iterator<?> pos1 = hp.entrySet().iterator();

            while (pos.hasNext()) {
                pos1.next();
                // hs._binnedHistPairs.set(i++, new
                // HistPair((Integer)pos.next(), new
                // HistValue((Float)pos.next())));
            }

        }
        return _binnedHistPairs;
    }

    /**
     * Description : Bins the data sample based on the resolution
     * 
     * @param v
     * @param resolution
     * @return
     */
    private float binit(float v, float resolution) {

        // convert the value into an integer using the resolution
        float i = v / resolution;

        // round to the nearest int
        i = (int) ((i > -.5) ? i + 0.5 : i - 0.5);

        // restore the resolution
        return i * resolution;
    }

    /**
     * Samples the grid and creates HistPairs. Stores the HistPairs in private
     * data. Returns false if a problem. Caches the raw grid points if the
     * cachesPoints flag is set. Samples are rounded based on the precision of
     * the grid. Wind direction is rounded to the nearest 10 degrees. Ensures
     * that the grid is valid and grid and Grid2DBit sizes match. Ensures there
     * are points in the sample area. Switch cases on data type and then
     * extracts out the data for each sample point.
     * 
     * @param grid
     * @param area
     * @param cachePoints
     */
    private void sampleGrid(IGridSlice grid, Grid2DBit area, boolean cachePoints) {
        _histPairs.clear();

        // ensure grid sizes match
        Point saSize = new Point(area.getXdim(), area.getYdim());
        if (!grid.getGridInfo().getGridLoc().gridSize().equals(saSize)) {
            statusHandler.handle(Priority.PROBLEM, "Grid size ["
                            + grid.getGridInfo().getGridLoc().gridSize()
                            + "] and Grid2DBit size [" + saSize
                            + "] not the same");
            return;
        }

        // ensure there are points in the sample area
        Point ll = new Point();
        Point ur = new Point();
        if (!area.extremaOfSetBits(ll, ur)) {
            return;
        }

        // precision and resolution
        int prec = grid.getGridInfo().getPrecision();

        _resolution = (float) Math.pow(10.000, -(double) prec);

        // switch/case based on the data type, then gather the entries

        switch (grid.getGridInfo().getGridType()) {
        case SCALAR:
            sampleScalar(grid, area, cachePoints, ll, ur);
            break;
        case VECTOR:
            sampleVector(grid, area, cachePoints, ll, ur);
            break;
        case WEATHER:
            sampleWx(grid, area, cachePoints, ll, ur);
            break;
        case DISCRETE:
            sampleDiscrete(grid, area, cachePoints, ll, ur);
            break;
        default:
            statusHandler.handle(Priority.PROBLEM, "Unknown grid format");
            break;
        }

        // sort the samples
        Collections.sort(_histPairs);
    }

    /**
     * If the sample was of scalars this function is called
     * 
     * @param grid
     * @param area
     * @param cachePoints
     * @param ll
     * @param ur
     */
    private void sampleScalar(IGridSlice grid, Grid2DBit area,
            boolean cachePoints, Point ll, Point ur) {
        Map<Float, MutableInteger> hp = new HashMap<Float, MutableInteger>();
        Grid2DFloat gs = ((ScalarGridSlice) grid).getScalarGrid();
        for (int x = ll.x; x <= ur.x; x++) {
            for (int y = ll.y; y <= ur.y; y++) {
                if (area.get(x, y) != 0) {
                    float val = (binit(gs.get(x, y), _resolution));
                    MutableInteger count = hp.get(val);
                    if (count == null) {
                        count = new MutableInteger(0);
                        hp.put(val, count);
                    }
                    count.add(1);
                    if (cachePoints) {
                        _rawPoints.add(new HistValue(gs.get(x, y)));
                    }
                }
            }
        }
        // Now store our histogram in ArrayList form
        for (Entry<Float, MutableInteger> entry : hp.entrySet()) {
            HistPair histPair = new HistPair(entry.getValue().getValue(),
                    new HistValue(entry.getKey()));
            _histPairs.add(histPair);
        }
    }

    private void sampleVector(IGridSlice grid, final Grid2DBit area,
            boolean cachePoints, final Point ll, final Point ur) {

        Map<Vector2f, MutableInteger> hp = new HashMap<Vector2f, MutableInteger>();

        Grid2DFloat mag = ((VectorGridSlice) grid).getMagGrid();
        Grid2DFloat dir = ((VectorGridSlice) grid).getDirGrid();

        for (int x = ll.x; x <= ur.x; x++) {
            for (int y = ll.y; y <= ur.y; y++) {
                if (area.get(x, y) != 0) {
                    float fdir = binit(dir.get(x, y), 10.0f);
                    float fmag = binit(mag.get(x, y), _resolution);

                    // normalize the direction
                    while (fdir < 0) {
                        fdir += 360.0f;
                    }
                    while (fdir >= 360.0) {
                        fdir -= 360.0f;
                    }
                    if (fdir == 360) {
                        fdir = 0.0f;
                    }
                    Vector2f val = new Vector2f(fmag, fdir);

                    MutableInteger count = hp.get(val);
                    if (count == null) {
                        count = new MutableInteger(0);
                        hp.put(val, count);
                    }
                    count.add(1);

                    if (cachePoints) {
                        _rawPoints.add(new HistValue(mag.get(x, y), dir.get(x,
                                y)));
                    }
                }
            }
        }

        // Now store our histogram in ArrayList form
        for (Entry<Vector2f, MutableInteger> entry : hp.entrySet()) {
            Vector2f first = entry.getKey();
            HistPair histPair = new HistPair(entry.getValue().getValue(),
                    new HistValue(first.x, first.y));
            _histPairs.add(histPair);
        }
    }

    // -- private
    // ----------------------------------------------------------------
    // HistSample::sampleWx()
    //
    // Products _histPairs list
    //
    // ---------------------------------------------------------------------------
    private void sampleWx(final IGridSlice grid, final Grid2DBit area,
            boolean cachePoints, final Point ll, final Point ur) {
        Map<WeatherKey, MutableInteger> hp = new HashMap<WeatherKey, MutableInteger>();
        Map<WeatherKey, MutableInteger> skhp = new HashMap<WeatherKey, MutableInteger>();
        Grid2DByte gs = ((WeatherGridSlice) grid).getWeatherGrid();
        String siteId = grid.getGridInfo().getGridLoc().getSiteId();
        WeatherKey[] key = ((WeatherGridSlice) grid).getKeys();
        for (int x = ll.x; x <= ur.x; x++) {
            for (int y = ll.y; y <= ur.y; y++) {
                if (area.get(x, y) != 0) {
                    WeatherKey k = key[gs.get(x, y)];
                    if (_subkeymode) {
                        List<WeatherSubKey> subkeys = k.getSubKeys();
                        for (int z = 0; z < subkeys.size(); z++) {
                            WeatherKey wk = new WeatherKey(siteId,
                                    Arrays.asList(subkeys.get(z)));
                            MutableInteger count = skhp.get(wk);
                            if (count == null) {
                                count = new MutableInteger(0);
                                skhp.put(wk, count);
                            }
                            count.add(1);

                            if (cachePoints) {
                                _rawPoints.add(new HistValue(wk));
                            }
                        }
                    } else {
                        MutableInteger count = hp.get(k);
                        if (count == null) {
                            count = new MutableInteger(0);
                            hp.put(k, count);
                        }
                        count.add(1);

                        if (cachePoints) {
                            _rawPoints.add(new HistValue(k));
                        }
                    }
                }
            }
        }

        // Now store our histogram in ArrayList form
        if (_subkeymode) {
            for (Entry<WeatherKey, MutableInteger> entry : skhp.entrySet()) {
                HistPair histPair = new HistPair(entry.getValue().getValue(),
                        new HistValue(entry.getKey()));
                _histPairs.add(histPair);
            }
        } else {
            for (Entry<WeatherKey, MutableInteger> entry : hp.entrySet()) {
                HistPair histPair = new HistPair(entry.getValue().getValue(),
                        new HistValue(entry.getKey()));
                _histPairs.add(histPair);
            }
        }

    }

    private void sampleDiscrete(final IGridSlice grid, final Grid2DBit area,
            boolean cachePoints, final Point ll, final Point ur) {
        Map<DiscreteKey, MutableInteger> hp = new HashMap<DiscreteKey, MutableInteger>();
        Map<DiscreteKey, MutableInteger> skhp = new HashMap<DiscreteKey, MutableInteger>();
        ParmID parmId = grid.getGridInfo().getParmID();
        Grid2DByte gs = ((DiscreteGridSlice) grid).getDiscreteGrid();
        String siteId = parmId.getDbId().getSiteId();
        DiscreteKey[] key = ((DiscreteGridSlice) grid).getKey();
        for (int x = ll.x; x <= ur.x; x++) {
            for (int y = ll.y; y <= ur.y; y++) {
                if (area.get(x, y) != 0) {
                    DiscreteKey k = key[gs.get(x, y)];
                    if (_subkeymode) {
                        List<String> subkeys = k.getSubKeys();
                        for (int z = 0; z < subkeys.size(); z++) {
                            DiscreteKey dk = new DiscreteKey(siteId,
                                    Arrays.asList(subkeys.get(z)), parmId);
                            MutableInteger count = skhp.get(dk);
                            if (count == null) {
                                count = new MutableInteger(0);
                                skhp.put(dk, count);
                            }
                            count.add(1);

                            if (cachePoints) {
                                _rawPoints.add(new HistValue(dk));
                            }
                        }
                    } else {
                        MutableInteger count = hp.get(k);
                        if (count == null) {
                            count = new MutableInteger(0);
                            hp.put(k, count);
                        }
                        count.add(1);

                        if (cachePoints) {
                            _rawPoints.add(new HistValue(k));
                        }
                    }
                }
            }
        }

        // Now store our histogram in ArrayList form
        if (_subkeymode) {
            for (Entry<DiscreteKey, MutableInteger> entry : skhp.entrySet()) {
                HistPair histPair = new HistPair(entry.getValue().getValue(),
                        new HistValue(entry.getKey()));
                _histPairs.add(histPair);
            }
        } else {
            for (Entry<DiscreteKey, MutableInteger> entry : hp.entrySet()) {
                HistPair histPair = new HistPair(entry.getValue().getValue(),
                        new HistValue(entry.getKey()));
                _histPairs.add(histPair);
            }
        }

    }

    /**
     * Description : counts the number of sample points and returns the number
     * 
     * @return
     */
    private int countSamplePoints() {
        int number = 0;
        for (int i = 0; i < _histPairs.size(); i++) {
            number += _histPairs.get(i).count();
        }
        return number;
    }

    /**
     * Description : Returns the sample's raw points. May not be available if
     * the cachePoints flag was not set in the constructor.
     */
    public final List<HistValue> rawPoints() {
        return _rawPoints;
    }

    /**
     * Description : Resets the raw points data
     */
    public void purgeCache() {
        _rawPoints.clear();
    }

    /**
     * Description : Returns the sample's valid time
     * 
     * @return
     */
    public final TimeRange validTime() {
        return _timeRange;
    }

    /**
     * Description : Returns the histogram associated with this sample
     * 
     * @return
     */
    public final HistPair[] histogram() {
        return _histPairs.toArray(new HistPair[_histPairs.size()]);
    }

    /**
     * Description : Returns the number of points associated with this sample
     * 
     * @return
     */
    public int numOfPoints() {
        return _numSamplePoints;
    }

    public float getResolution() {
        return _resolution;
    }

    @Override
    public String toString() {
        return "HistSample " + timeRange.toString();
    }
}
