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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Contains the histogram for one parm and one reference area
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 21, 2008  1167       mnash       Initial creation
 * Sep 3, 2008   1283       njensen     Fixed issues
 * Jun 15, 2012  14994      ryu         Fixed NPE on _minMaxSumHP
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class ParmHisto {

    private ParmID _parmID;

    private List<HistSample> _samples = null;

    private ReferenceData _area;

    private TimeRange _timeRange;

    private int _numGridPoints;

    private float _minV;

    private float _maxV;

    private float _sumV;

    private List<HistPair> _minMaxSumHP;

    private int _minModPercent;

    private int _maxModPercent;

    private float _minModV;

    private float _maxModV;

    private float _sumModV;

    private float _minStds;

    private float _maxStds;

    private float _minStdV;

    private float _maxStdV;

    private float _sumStdV;

    private List<ReferenceData> _iscRefAreas;

    private float _resolution;

    public ParmHisto() {
        _samples = new ArrayList<HistSample>();
    }

    /**
     * Constructor taking a series of GridSlices for a single parm, and a
     * ReferenceData area and timeRange. A list of isc edit areas is provided
     * for the isc data case.
     * 
     * @param parmID
     * @param grids
     * @param area
     * @param timeRange
     * @param iscRefAreas
     */
    public ParmHisto(ParmID parmID, List<IGridSlice> grids, ReferenceData area,
            TimeRange timeRange, List<ReferenceData> iscRefAreas) {
        _parmID = parmID;
        _area = area;
        _timeRange = timeRange;
        _numGridPoints = 0;
        _minV = 0.0f;
        _maxV = 0.0f;
        _sumV = 0.0f;
        _minModPercent = 0;
        _maxModPercent = 0;
        _minModV = 0.0f;
        _maxModV = 0.0f;
        _sumModV = 0.0f;
        _minStds = 0.0f;
        _maxStds = 0.0f;
        _minStdV = 0.0f;
        _maxStdV = 0.0f;
        _sumStdV = 0.0f;
        _iscRefAreas = iscRefAreas;
        _samples = new ArrayList<HistSample>(grids.size());

        if (grids.size() == 0) {
            return; // nothing to do
        }

        // extract info from the 1st grid
        boolean rateParm = grids.get(0).getGridInfo().isRateParm();

        // precision and resolution
        int prec = grids.get(0).getGridInfo().getPrecision();
        _resolution = (float) Math.pow(10.000, -(double) prec);

        // make a Grid2DBit from the reference area
        Grid2DBit bits = area.getGrid();
        _numGridPoints = bits.numberOfBitsSet();

        sample(grids, bits, rateParm);

        // if rate parm, calculate the timeweighted summation and stats
        if (rateParm) {
            calcRateStatistics(grids.get(0).getGridInfo());
            for (int i = 0; i < _samples.size(); i++) {
                _samples.get(i).purgeCache();
            }
        }
    }

    /**
     * Samples the grids, over the given Grid2DBit
     * 
     * @param grids
     * @param bits
     * @param rateParm
     * @SuppressWarnings("unchecked")
     */
    private void sample(List<IGridSlice> grids, Grid2DBit bits, boolean rateParm) {
        // performance shortcut - non-ISC case
        if (!grids.get(0).getGridInfo().getParmID().getDbId().getModelName()
                .equals("ISC")) {
            for (int i = 0; i < grids.size(); i++) {
                _samples.add(new HistSample(grids.get(i), bits, rateParm, true));
            }
        } else {
            // ISC case - process each grid
            for (int i = 0; i < grids.size(); i++) {
                // get the valid data points for this grid
                Grid2DBit validPts = validPoints(grids.get(i));
                // only if all points for the edit area are valid, do we
                // consider this grid valid
                if (validPts.and(bits).equals(bits)) {
                    _samples.add(new HistSample(grids.get(i), bits, rateParm,
                            true));
                }
            }
        }
    }

    /**
     * Returns the minimum, maximum and summation values for a rate-dependent
     * weather element.
     * 
     */
    public float[] minMaxSum() {
        return new float[] { _minV, _maxV, _sumV };
    }

    /**
     * Returns the representative average of the maxMinSum() for SCALAR/VECTOR.
     * Outliers are eliminated. Percent ranges from 0 to 50.
     * 
     * @param minpercent
     * @param maxpercent
     */
    public float[] moderatedMinMaxSum(int minpercent, int maxpercent) {

        // if not cached, then calculate
        if (((minpercent != _minModPercent) || (maxpercent != _maxModPercent))
                && (_minMaxSumHP != null) && (_minMaxSumHP.size() >= 1)) {
            ParmHisto ph = this;

            // calculate the number of sample points
            int numSamplePoints = 0;
            for (int i = 0; i < _minMaxSumHP.size(); i++) {
                numSamplePoints += _minMaxSumHP.get(i).count();
            }

            // calc limits
            float runningCount = 0;
            int minlimitCount = (int) (minpercent * numSamplePoints / 100.0);
            int maxlimitCount = (int) ((100 - maxpercent) * numSamplePoints / 100.0);

            // find the limits
            float sum = 0.0f;
            int count = 0;
            boolean minFound = false;
            boolean maxFound = false;

            for (int i = 0; i < _minMaxSumHP.size(); i++) {
                int minVCount = (int) runningCount + 1; // indexes for this pair
                int maxVCount = (int) runningCount
                        + _minMaxSumHP.get(i).count();

                int numInclude = 0;
                if ((minlimitCount > maxVCount) || (maxlimitCount < minVCount)) {
                    numInclude = 0;
                } else {
                    numInclude = Math.max(minlimitCount, minVCount)
                            - Math.min(maxlimitCount, maxVCount) + 1;
                }

                if (numInclude != 0) {
                    sum += numInclude * _minMaxSumHP.get(i).value().scalar();
                    count += numInclude;
                }

                runningCount += _minMaxSumHP.get(i).count();

                if ((runningCount > minlimitCount) && !minFound) {
                    ph._minModV = _minMaxSumHP.get(i).value().scalar();
                    minFound = true;
                }
                if ((runningCount > maxlimitCount) && !maxFound) {
                    ph._maxModV = _minMaxSumHP.get(i).value().scalar();
                    maxFound = true;
                }
            }

            ph._sumModV = new HistValue(binit(sum / count, _resolution))
                    .scalar();
            ph._minModPercent = minpercent;
            ph._maxModPercent = maxpercent;
        }

        return new float[] { _minModV, _maxModV, _sumModV };
    }

    /**
     * Bins the data sample based on the resolution
     * 
     * @param v
     * @param resolution
     * @return i*resolution
     */
    private float binit(float v, float resolution) {
        // convert the value into an integer using the resolution
        float i = v / resolution;

        // round to the nearest int
        i = (int) (i > -0.5 ? i + 0.5 : i - 0.5);

        // restore the resolution
        return i * resolution;
    }

    /**
     * Finds the standard deviation of the min and max sum.
     * 
     * @param minStds
     * @param maxStds
     */
    public float[] stdDevMinMaxSum(float minStds, float maxStds) {

        // if not cached, then calculate
        if (((minStds != _minStds) || (maxStds != _maxStds))
                && (_minMaxSumHP != null) && (_minMaxSumHP.size() >= 1)) {
            ParmHisto ph = this;
            float sDev = 0.0f;
            float avg = 0.0f;
            int count = 0;
            stdDevAndAverage(_minMaxSumHP, sDev, avg, count);

            float minValue = avg - minStds * sDev;
            float maxValue = avg + maxStds * sDev;

            // calculate the standard deviation average
            float sum = 0.0f;
            int cnt = 0;

            for (int i = 0; i < _minMaxSumHP.size(); i++) {
                float v = _minMaxSumHP.get(i).value().scalar();
                if ((v >= minValue) && (v <= maxValue)) {
                    sum += _minMaxSumHP.get(i).count() * v;
                    cnt += _minMaxSumHP.get(i).count();
                }
            }

            if (cnt > 0) {
                ph._sumStdV = new HistValue(binit(sum / cnt, _resolution))
                        .scalar();
            } else {
                ph._sumStdV = 0.0f;
            }

            // calculate the min stddev
            ph._minStdV = avg - minStds * sDev;
            if (_minMaxSumHP.get(0).value().scalar() > minValue) {
                ph._minStdV = _minMaxSumHP.get(0).value().scalar();
            }

            // calculate the max stddev
            ph._maxStdV = avg + maxStds * sDev;
            if (_minMaxSumHP.get(_minMaxSumHP.size() - 1).value().scalar() < maxValue) {
                ph._maxStdV = _minMaxSumHP.get(_minMaxSumHP.size() - 1).value()
                        .scalar();
            }
        }
        return new float[] { _minStdV, _maxStdV, _sumStdV };
    }

    /**
     * Average and standard deviations, plus count.
     * 
     * @param histPairs
     * @param stdDev
     * @param average
     * @param pointCount
     */
    private void stdDevAndAverage(final List<HistPair> histPairs, float stdDev,
            float average, int pointCount) {
        if (histPairs.size() == 0) {
            stdDev = 0.0f;
            average = 0.0f;
            pointCount = 0;
            return;
        }

        double sum = 0.0;
        double sum2 = 0.0;
        long count = 0;

        for (HistPair hp : histPairs) {
            double x = hp.value().scalar();
            long cnt = hp.count();
            sum += cnt * x;
            sum2 = (float) (cnt * x * x);
            count += cnt;
        }

        stdDev = (float) Math.sqrt((sum2 - sum * sum / count / count));
        average = (float) (sum / count);
        pointCount = (int) count;

    }

    /**
     * Calculates the minimum, maximum, and summation values for a
     * rate-dependent weather element. Given the grid parm info. Only works with
     * scalar and vector.
     * 
     * @param info
     */
    private void calcRateStatistics(final GridParmInfo info) {
        if ((_samples.size() == 0) || (_samples.get(0).rawPoints().size() == 0)) {
            return;
        }

        // set up summation List to proper length
        List<Float> sums = new ArrayList<Float>();
        for (int i = 0; i < _samples.get(0).rawPoints().size(); i++) {
            sums.add(i, 0.0f);
        }

        // process each sample
        for (int i = 0; i < _samples.size(); i++) {
            HistSample hs = _samples.get(i); // for convenience

            // calc multiplier for time
            TimeRange intersect = _timeRange.intersection(hs.validTime());
            float timeRatio = (float) intersect.getDuration()
                    / (float) hs.validTime().getDuration();

            List<HistValue> samp = hs.rawPoints();

            for (int j = 0; j < samp.size(); j++) {
                sums.set(j, sums.get(j) + samp.get(j).scalar() * timeRatio);
                // works for mag too
            }
        }

        // now figure out the min, max, and sum stats from the sums array
        _minV = _maxV = sums.get(0);

        float ave = sums.get(0);
        for (int i = 1; i < sums.size(); i++) {
            if (sums.get(i) > _maxV) {
                _maxV = sums.get(i);
            }
            if (sums.get(i) < _minV) {
                _minV = sums.get(i);
            }
            ave += sums.get(i);
        }
        _sumV = ave / sums.size();

        // now make a histogram in order for the moderated/stddev maxMinSum()
        _minMaxSumHP = makeHistogramFromPoints(sums, info);
    }

    /**
     * Description :Calculates the histogram from points. Special use for the
     * maxMinSum() moderated and std.dev versions. Given the parm info. Returns
     * the histogram. Only works with scalar and vector. Similar code to
     * sampleScalar() an binit() in the HistSample class.
     * 
     * @param points
     * @param info
     * @return histPairs
     */
    private List<HistPair> makeHistogramFromPoints(List<Float> points,
            GridParmInfo info) {
        Map<Float, Integer> hp = new HashMap<Float, Integer>();
        for (int i = 0; i < points.size(); i++) {
            Float val = binit(points.get(i), _resolution);
            Integer count = hp.get(val);
            if (count == null) {
                count = 0;
            }
            count += 1;
            hp.put(val, count);
        }

        List<HistPair> histPairs = new ArrayList<HistPair>(hp.size());
        Iterator<Float> itr = hp.keySet().iterator();
        while (itr.hasNext()) {
            Float val = itr.next();
            histPairs.add(new HistPair(hp.get(val), new HistValue(val)));
        }

        return histPairs;
    }

    /**
     * Returns a Grid2DBit showing the valid grid points
     * 
     * @param gs
     * @return validBits
     */
    private Grid2DBit validPoints(final IGridSlice gs) {
        Point size = gs.getGridInfo().getGridLoc().gridSize();
        if (!gs.getGridInfo().getParmID().getDbId().getModelName()
                .equals("ISC")) {
            return new Grid2DBit(size.x, size.y, true);
        }

        // ISC grid
        // Calculate the sites in the grid that are used
        GridDataHistory[] h = gs.getHistory();
        Set<String> sites = new HashSet<String>();
        for (int i = 0; i < h.length; i++) {
            String site = h[i].getOriginParm().getDbId().getSiteId();
            sites.add(site);
        }

        // get Grid2DBits for each of the edit areas, OR them together
        Grid2DBit validBits = new Grid2DBit(size.x, size.y);
        if (_iscRefAreas != null && !_iscRefAreas.isEmpty()) {
            for (String site : sites) {
                for (ReferenceData refArea : _iscRefAreas) {
                    if (refArea.getId().getName().equals("ISC_" + site)) {
                        validBits = validBits.orEquals(refArea.getGrid());
                        break;
                    }
                }
            }
        }
        return validBits;
    }

    /**
     * Constructor taking the parmID, plus a series of samples, and an area and
     * time range, which simply stores the data
     * 
     * @param parmID
     * @param samples
     * @param area
     * @param timeRange
     * @param numGridPoints
     * @param minV
     * @param maxV
     * @param sumV
     */
    public ParmHisto(final ParmID parmID, final List<HistSample> samples,
            final ReferenceData area, final TimeRange timeRange,
            final int numGridPoints, float minV, float maxV, float sumV) {
        _parmID = parmID;
        _samples = samples;
        _area = area;
        _timeRange = timeRange;
        _numGridPoints = numGridPoints;
        _minV = minV;
        _maxV = maxV;
        _sumV = sumV;
    }

    /**
     * Returns the reference data associated with this histogram
     * 
     * @return _area
     */
    public final ReferenceData area() {
        return _area;
    }

    /**
     * Returns the time range associated with this histogram
     * 
     * @return _timeRange
     */
    public final TimeRange timeRange() {
        return _timeRange;
    }

    /**
     * Returns the histogram for this ParmHisto.
     * 
     * @return _samples
     */
    public final HistSample[] histoSamples() {
        return _samples.toArray(new HistSample[_samples.size()]);
    }

    /**
     * Returns the length of _samples
     * 
     * @return _samples.size()
     */
    public final int getSampleLen() {
        return _samples.size();
    }

    /**
     * Returns the HistSample for the given index
     * 
     * @param index
     * @return _samples.get(index)
     */
    public final HistSample getSample(int index) {
        return _samples.get(index);
    }

    /**
     * Returns the parameter identifier for this ParmHisto.
     * 
     * @return _parmID
     */
    public final ParmID parmID() {
        return _parmID;
    }

    /**
     * Returns the number of grid points for this ParmHisto.
     * 
     * @return _numGridPoints
     */
    public final int numberOfGridPoints() {
        return _numGridPoints;
    }

    public String getCompositeNameUI() {
        return this._parmID.compositeNameUI();
    }
}
