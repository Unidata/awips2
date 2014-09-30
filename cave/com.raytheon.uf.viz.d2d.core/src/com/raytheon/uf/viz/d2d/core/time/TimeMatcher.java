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

package com.raytheon.uf.viz.d2d.core.time;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.TimeZone;

import org.apache.commons.collections.MultiMap;
import org.apache.commons.collections.map.MultiValueMap;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.DataTimeComparator;

/**
 * 
 * Partial port of AWIPS I D2D TimeMatchFunctions class
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2007            chammack    Initial Creation.
 * May 31, 2013 15908      dhuffman    Removed a null from a method call to
 *                                     cease a null pointer exception.
 * May  5, 2014 DR 17201   D. Friedman Make same-radar time matching work more like A1.
 * Aug 08, 2013 2245       bsteffen    Make all DataTime comparisons consistent.
 * Jul 18, 2014 ASM #15049 D. Friedman Fix LAPS problem introduced by DR 17201
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class TimeMatcher {

    // 1 minutes in milliseconds
    private static final long ONE_MINUTE_MS = 60000;

    // 2 minutes in milliseconds
    private static final long TWO_MINUTES_MS = 2 * ONE_MINUTE_MS;

    // 5 minutes in milliseconds
    private static final long FIVE_MINUTES_MS = 5 * ONE_MINUTE_MS;;

    // 11 minutes in milliseconds
    private static final long ELEVEN_MINUTES_MS = 11 * ONE_MINUTE_MS;;

    // 1 hour in milliseconds
    private static final long ONE_HOUR_MS = 60 * ONE_MINUTE_MS;

    // 1 day in milliseconds
    private static final long ONE_DAY_MS = 24 * ONE_HOUR_MS;

    // half minute in seconds
    private static final long HALF_MINUTE_S = 30;

    // 1 minute in seconds
    private static final long ONE_MINUTE_S = 60;

    // half hour in seconds
    private static final long HALF_HOUR_S = ONE_MINUTE_S * 30;

    // 1 hour in seconds
    private static final long ONE_HOUR_S = ONE_MINUTE_S * 60;

    // 3 hours in seconds
    private static final long THREE_HOURS_S = ONE_HOUR_S * 3;

    // 6 hours in seconds
    private static final long SIX_HOURS_S = ONE_HOUR_S * 6;

    public static final float DEFAULT_TOLERANCE_FACTOR = 0.6f;

    private static long autoIntervals[] = { 300, 900, 1800, 3600, 10800, 21600,
            43200, 86400 };

    private boolean radarOnRadarYes = false;

    // Package access
    TimeMatcher() {

    }

    // This routine filters out any DataTimes from a sequence that have a
    // forecast time other than zero and a reference time earlier than
    // "latest". Returns position in the sequence of the first DataTime with
    // a reference time equal to "latest". Caller allocates and owns "times".
    // ---------------------------------------------------------------------------
    static int filterOldForecasts(List<DataTime> times, Date latest,
            boolean noBackfill) {
        int next, check;
        int lastRun = -1;
        long curRef;

        // Determine if there is more than one forecast time in this list, if
        // not
        // then there is nothing to filter.
        if (latest.getTime() == 0) {
            return times.size() - 1;
        }
        long fcstOK = (times).get(0).getMatchFcst();
        for (check = times.size() - 1; check > 0; check--) {
            if (times.get(check).getMatchFcst() != fcstOK) {
                break;
            }
        }
        if (check <= 0) {
            return times.size() - 1;
        }
        fcstOK = noBackfill ? -99999 : 0;

        // Remove any items with a reference less than latest and a forecast
        // time
        // not equal to fcstOK.
        for (next = check = 0; check < times.size(); check++) {
            curRef = (times).get(check).getMatchRef();
            if (curRef < latest.getTime()
                    && (times).get(check).getMatchFcst() != fcstOK) {
                continue;
            }
            if (curRef == latest.getTime() && lastRun < 0) {
                lastRun = next;
            }
            if (next < check) {
                times.set(next, times.get(check));
            }
            next++;
        }

        if (next < check) {
            for (int i = next + 1; i < check; i++) {
                times.remove(next);
            }
        }

        return lastRun;
    }

    // This routine sorts a sequence of DataTimes in ascending order with
    // valid time as the primary key and forecast time as the secondary
    // key. Caller allocates and owns "times" and "majorIndex". Routine
    // expects "majorIndex" to be an empty sequence when routine is called.
    // On output, "majorIndex" contains index of first element in "times" for
    // each unique value of valid time. Sort of allows user to then
    // treat "times" as a 2d array.
    // Will not construct majorIndex if pointer is null.
    // ---------------------------------------------------------------------------
    static void validTimeSort(List<DataTime> times, List<Integer> majorIndex,
            boolean ignoreSpatial) {
        int i, j;
        if (times.size() == 0) {
            return;
        }

        Collections.sort(times, new DataTimeComparator(
                DataTimeComparator.SortKey.VALID_TIME,
                DataTimeComparator.SortKey.FORECAST_TIME, true));

        if (majorIndex == null) {
            return;
        }

        majorIndex.clear();
        for (i = 0; i < times.size(); i++) {
            if (!(times.get(i).isNull())) {
                break;
            }
        }
        if (i >= times.size()) {
            return;
        }
        majorIndex.add(i);

        if (times.get(i).isSpatial() && !ignoreSpatial) {
            long ref0 = times.get(i).getMatchRef();
            for (j = i + 1; j < times.size(); i++, j++) {
                if (times.get(j).getMatchValid() > times.get(i).getMatchValid()) {
                    ref0 = times.get(j).getMatchRef();
                } else if (ref0 != times.get(j).getMatchRef()) {
                    continue;
                }
                majorIndex.add(j);
            }
        } else {
            for (j = i + 1; j < times.size(); i++, j++) {
                if (times.get(j).getMatchValid() > times.get(i).getMatchValid()) {
                    majorIndex.add(j);
                }
            }
        }

    }

    // Scans a sorted Sequence of DataTimes and determines the minimum period
    // of time separating the individual items. Considers separation in both
    // initial time and forecast time space. Separation cannot be zero.
    // ---------------------------------------------------------------------------
    IntrinsicReturnVal intrinsicPeriod(DataTime[] times,
            boolean haveForecasts) {
        int i0, i, j, m, nn, n0;
        long dt, dt2, d, df;
        boolean haveFcst = false;

        // Check for some simple cases where there is no meaningful period.
        m = times.length - 1;
        for (i0 = 0; i0 < m; i0++) {
            if (!(times[i0].isNull())) {
                break;
            }
        }
        for (; m > i0; m--) {
            if (!times[m].isNull()) {
                break;
            }
        }
        if (i0 >= m) {
            if (haveForecasts) {
                haveForecasts = haveFcst;
            }
            if (radarOnRadarYes) {
                return new IntrinsicReturnVal(haveForecasts, FIVE_MINUTES_MS);
            }
            return new IntrinsicReturnVal(haveForecasts, ONE_DAY_MS);
        }

        // Initialize smallest and 2nd smallest dt, counts for null and 0
        // deltas.
        dt = dt2 = 0x7FFFFFFF;
        nn = n0 = 0;

        // Loop to inspect each pair of consecutive non-null times.
        for (i = i0, j = i + 1; j <= m; j++) {

            // Pull out delta in ref and fcst space, check for zero.
            if (times[j].isNull()) {
                n0++;
                continue;
            }
            d = (times[j].getMatchRef() - (times[i].getMatchRef()));
            df = 1000 * (times[j].getMatchFcst() - (times)[i].getMatchFcst());
            i = j;
            if (d == 0 && df == 0) {
                n0++;
                continue;
            }

            // Note if this can be smallest or 2nd smallest delta.
            if (d < 0) {
                d = -d;
            }
            if (df < 0) {
                df = -df;
            }
            if (d == 0 || df > 0 && df < d) {
                d = df;
            }
            if (df > 0) {
                haveFcst = true;
            }
            if (d >= dt2) {
                ;
            } else if (d == dt) {
                nn++;
            } else if (d < dt) {
                dt2 = dt;
                dt = d;
                nn = 1;
            } else {
                dt2 = d;
            }
        }

        // Checking for presence of bimodal distribution that implies
        // artifical
        // times being present.
        if (dt == 0x7FFFFFFF) {
            return new IntrinsicReturnVal(haveForecasts, ONE_DAY_MS);
        }
        if (dt2 < 0x7FFFFFFF && dt <= TWO_MINUTES_MS && dt2 > 50 * dt
                && !radarOnRadarYes) {
            dt = dt2;
        } else {
            nn = 0;
        }
        nn += n0 + i0;

        // dont check for variable separation if we have forecast data
        if (haveForecasts) {
            haveForecasts = haveFcst;
        }
        if (haveFcst || radarOnRadarYes) {
            return new IntrinsicReturnVal(haveForecasts, dt);
        }

        // if mean separation is reasonably close to min, use it
        nn = m - nn;
        if (nn < 1) {
            nn = 1;
        }
        d = ((times[m].getMatchValid() - times[i0].getMatchValid()) / nn);
        if (d < 0) {
            d = -d;
        }
        if (d >= dt && d < dt * 1.05) {
            return new IntrinsicReturnVal(haveForecasts, d);
        }

        // check for variable separation
        dt2 = dt;
        for (i = i0, j = i0 + 1; j <= m; i++, j++) {
            d = times[j].getMatchValid() - times[i].getMatchValid();
            if (d < 0) {
                d = -d;
            }
            if (d < dt * 1.95 && d > dt2) {
                dt2 = d;
            }
        }

        return new IntrinsicReturnVal(haveForecasts, dt2);
    }

    private static class IntrinsicReturnVal {

        public IntrinsicReturnVal(boolean haveForecasts, long intrinsicPeriod) {
            this.haveForecasts = haveForecasts;
            this.intrinsicPeriod = intrinsicPeriod;
        }

        public boolean haveForecasts;

        public long intrinsicPeriod;
    }

    // Scans a matched Sequence of DataTimes and majorIndex that came from a
    // call to validTimeSort and determines the minimum length of valid
    // time separating the individual items. Separation cannot be zero.
    // ---------------------------------------------------------------------------
    IntrinsicReturnVal intrinsicPeriod(List<DataTime> times,
            List<Integer> majorIndex, boolean haveForecasts) {
        int i, j, k, nn, n0;
        long dt, dt2, d;
        boolean haveFcst = false;

        if (majorIndex.size() < 2) {
            if (haveForecasts) {
                haveForecasts = haveFcst;
            }
            if (radarOnRadarYes) {
                return new IntrinsicReturnVal(haveForecasts, FIVE_MINUTES_MS);
            }
            return new IntrinsicReturnVal(haveForecasts, ONE_DAY_MS);
        }
        n0 = majorIndex.get(0);
        int m = majorIndex.size() - 1;
        nn = majorIndex.get(m);

        dt = dt2 = 0x7FFFFFFF;
        n0 = nn = 0;

        j = majorIndex.get(0);
        for (k = 1; k <= m; k++) {
            i = j;
            j = (majorIndex).get(k);
            if ((times).get(i).getMatchFcst() > 0) {
                haveFcst = true;
            }
            d = (times).get(j).getMatchValid() - (times).get(i).getMatchValid();
            if (d < 0) {
                d = -d;
            }
            if (d == 0) {
                n0++;
            } else if (d == dt) {
                nn++;
            } else if (d >= dt2) {
                ;
            } else if (d < dt) {
                dt2 = dt;
                dt = d;
                nn = 1;
            } else {
                dt2 = d;
            }
        }
        if ((times).get(j).getMatchFcst() > 0) {
            haveFcst = true;
        }

        // Checking for presence of bimodal distribution that implies artifical
        // times being present.
        if (dt == 0x7FFFFFFF) {
            return new IntrinsicReturnVal(haveForecasts, ONE_DAY_MS);
        }
        if (dt2 < 0x7FFFFFFF && dt <= TWO_MINUTES_MS && dt2 > 50 * dt
                && !radarOnRadarYes) {
            dt = dt2;
        } else {
            nn = 0;
        }
        nn += n0;

        // dont check for variable separation if we have forecast data
        if (haveForecasts) {
            haveForecasts = haveFcst;
        }
        if (haveFcst || radarOnRadarYes) {
            return new IntrinsicReturnVal(haveForecasts, dt);
        }

        // if mean separation is reasonably close to min, use it
        nn = m - nn;
        if (nn < 1) {
            nn = 1;
        }
        d = ((times).get((majorIndex).get(m)).getMatchValid() - (times).get(
                (majorIndex).get(0)).getMatchValid())
                / nn;
        if (d < 0) {
            d = -d;
        }
        if (d >= dt && d < dt * 1.05) {
            return new IntrinsicReturnVal(haveForecasts, d);
        }

        // check for variable separation
        dt2 = dt;
        j = (majorIndex).get(0);
        for (k = 1; k <= m; k++) {
            i = j;
            j = majorIndex.get(k);
            d = (times).get(j).getMatchValid() - (times).get(i).getMatchValid();
            if (d < 0) {
                d = -d;
            }
            if (d < dt * 1.95 && d > dt2) {
                dt2 = d;
            }
        }

        return new IntrinsicReturnVal(haveForecasts, dt2);
    }

    static void makeFinalLoadList(List<DataTime> loadTimes, int nFrames,
            Date prefered, boolean preferedLast) {
        if (loadTimes.size() <= nFrames) {
            return;
        }

        // Find the prefered index
        int i;
        int prefIdx = loadTimes.size() - 1;
        if (prefered.getTime() > 0) {
            long dt = 0x7FFFFFFF;
            boolean byRef = (loadTimes).get(prefIdx).getMatchValid() == (loadTimes)
                    .get(0).getMatchValid();
            long thisTime;
            for (i = loadTimes.size() - 1; i >= 0 && dt > 0; i--) {
                if (byRef) {
                    thisTime = (loadTimes).get(i).getMatchRef();
                } else {
                    thisTime = (loadTimes).get(i).getMatchValid();
                }
                long thisdt = thisTime - prefered.getTime();
                if (thisdt < 0) {
                    thisdt = -thisdt;
                }
                if (thisdt >= dt) {
                    continue;
                }
                dt = thisdt;
                prefIdx = i;
            }
        }

        // Remove any from the list that make it too long.
        if (preferedLast) {
            prefIdx -= nFrames - 1;
            if (prefIdx < 0) {
                prefIdx = 0;
            }
        }
        if (loadTimes.size() > prefIdx + nFrames) {
            int sz = loadTimes.size();
            for (int k = prefIdx + nFrames; k < sz; k++) {
                loadTimes.remove(prefIdx + nFrames);
            }
        }
        while (loadTimes.size() > nFrames) {
            int sz = loadTimes.size();
            for (int k = 0; k < sz - (nFrames); k++) {
                loadTimes.remove(0);
            }
        }
    }

    //
    // Given a sequence of available DataTimes and a sequence of existing
    // frame times, this routine fills up the sequence "loadTimes" with the
    // the proper list of times for doing a valid time overlay. For any given
    // valid time available in depictTimes, this will only use the one with
    // the latest reference time. From the allowable DataTimes, routine will
    // match to each frame that DataTime which most closely matches its valid
    // time. Caller creates and owns depictTimes, frameTimes, and
    // loadTimes. loadTimes is assumed to be an empty sequence when call is
    // made.
    // After completion, loadTimes will always have the same length as
    // frameTimes,
    // with null DataTimes indicating unmatchable frames.
    // Null entries result when a given frame has no item in depictTimes
    // available that has a valid time within a certain tolerance, that
    // tolerance being half the intrinsic period the existing frames or the
    // data being overlaid, whichever is greater.
    // ---------------------------------------------------------------------------
    public DataTime[] doValTimOverlay(DataTime[] depictTimeArr,
            DataTime[] frameTimes, long deltaTime, LoadMode mode, Date latest,
            float tolerance) {

        List<DataTime> depictTimes = new ArrayList<DataTime>();
        for (DataTime dt : depictTimeArr) {
            depictTimes.add(dt);
        }

        int f, f0;
        int p, pp, m, n, q;
        int best;
        long dtf, dt, dtd, fd;
        double fo;
        long vf, v1, v2, vd;

        if (depictTimes.size() == 0) {
            return new DataTime[0];
        }

        DataTime[] loadTimes = new DataTime[frameTimes.length];

        for (f0 = 0; f0 < frameTimes.length; f0++) {
            if ((frameTimes)[f0].getRefTime().getTime() != 0) {
                break;
            }
        }
        if (f0 >= frameTimes.length) {
            return loadTimes;
        }

        if (mode == LoadMode.RANGE_MATCH) {
            boolean allFramesAreRangeBased = true;
            for (DataTime depictTime : depictTimeArr) {
                if (depictTime != null) {
                    if (depictTime.getValidPeriod() == null
                            || depictTime.getValidPeriod().getDuration() < (1000 * 60)) {
                        allFramesAreRangeBased = false;
                        break;
                    }
                }
            }
            if (allFramesAreRangeBased) {
                // Code added by chammack to support gfe time matching
                for (int i = 0; i < loadTimes.length; i++) {

                    for (int j = 0; j < depictTimeArr.length; j++) {
                        if (depictTimeArr[j].getValidPeriod().contains(
                                frameTimes[i].getValidTime().getTime())) {
                            loadTimes[i] = depictTimeArr[j];
                            break;
                        }
                    }

                }

                return loadTimes;
            }
            // otherwise act like valid time seq
            mode = LoadMode.VALID_TIME_SEQ;
        }

        if (latest != null) {
            if (mode == LoadMode.NO_BACKFILL) {
                filterOldForecasts(depictTimes, latest, true);
            } else if (mode == LoadMode.LATEST
                    || mode == LoadMode.PREVIOUS_MODEL_RUN) {
                filterOldForecasts(depictTimes, latest, false);
            }

        }

        // If this is a spatial FCST_TIME_MATCH, do forecast time filtering now.
        boolean byFcst = false;
        boolean fspatial = frameTimes[f0].isSpatial();
        boolean dspatial = depictTimes.get(0).isSpatial();
        boolean spatial = fspatial && dspatial;
        if (mode != LoadMode.DPROG_DT && mode != LoadMode.FCST_TIME_MATCH) {
            ;
        } else if (spatial) {
            depictTimes = filterByForecast(
                    depictTimes.toArray(new DataTime[] {}),
                    frameTimes[f0].getMatchFcst());
        } else {
            byFcst = true;
        }

        // Sort data times and record length of majorIndex, which is how many
        // different valid times we have.
        // validTimeSort sorts primarily on valid time and secondarily on
        // forecast time. Because valid = reference + forecast, having forecast
        // time increase within each valid time group means reference time
        // decreases. Thus the first items in each valid time group, which
        // are pointed to by majorIndex, are the most recent data for each
        // valid time.
        List<Integer> majorIndex = new ArrayList<Integer>();
        validTimeSort(depictTimes, majorIndex, false);
        m = majorIndex.size() - 1;
        n = depictTimes.size();

        // Compute time separation of data, frames, calculate time tolerance
        int ef = frameTimes.length - 1;
        boolean dataFcsts = false, frameFcsts = false;

        IntrinsicReturnVal rv = intrinsicPeriod(depictTimes, majorIndex,
                dataFcsts);
        dt = rv.intrinsicPeriod;
        dtd = (rv.intrinsicPeriod * 3) / 2;
        dataFcsts = rv.haveForecasts;

        rv = intrinsicPeriod(frameTimes, frameFcsts);
        dtf = rv.intrinsicPeriod;
        frameFcsts = rv.haveForecasts;

        if (fspatial) {
            frameFcsts = dataFcsts;
        } else if (dtf > dt) {
            dt = dtf;
        }

        // A1 TimeMatchFunctions.C ~ line 952
        if (dt > ONE_MINUTE_MS && dt <= ELEVEN_MINUTES_MS
                && dtf > ONE_MINUTE_MS && dtf <= ELEVEN_MINUTES_MS
                && radarOnRadarYes) {
            if (dtf<dt) {
                dt = dtf;
            }
        } else if (dtf>dt) {
            dt = dtf;
        }

        /* A1 TimeMatchingFunctions.C ~ line 960
         *  For 88D radar, dt is usually 300 seconds or larger
         *  For TDWR radar, dt is usually 180 seconds or less
         *  To allow 3 minutes overlay for TDWR products, dt is set to 300 seconds
         */
        if (radarOnRadarYes && dt < FIVE_MINUTES_MS) {
            dt = FIVE_MINUTES_MS;
        }

        if (tolerance > 99) {
            dt = 0x7FFFFFl * 1000l;
        } else {
            dt = (int) (dt * tolerance);
        }
        if (dt == 0) {
            ;
        } else if (dataFcsts && frameFcsts) {
            dt--;
        } else {
            dt++;
        }

        if (dt > dtd || dt < dtf
                || frameTimes[ef].getMatchValid() - latest.getTime() > dtd) {
            // Three conditions here:
            // 1) dtd is supposed to provide extra padding compared to dt, so if
            // dt is bigger then set them equal to avoid reducing the padding.
            // 2) Clear the extra padding if the acceptable depict time spacing
            // is not bigger than the frame time spacing
            // 3) Clear the extra padding if the latest frame time is not within
            // dtd of the latest depict time

            // setting dtd to dt makes any use of dtd a noop.
            dtd = dt;
        }

        // Try to find match for each frame. Dependent on valid times increasing
        // monotonically in depictTimes.
        for (f = f0; f <= ef; f++) {
            // vf is frame valid time, vd is data valid time
            if ((frameTimes)[f].getRefTime().getTime() == 0) {
                continue;
            }
            vf = (frameTimes)[f].getMatchValid() + deltaTime;
            v1 = vf - dt; // first usable valid time
            v2 = vf + dt; // last usable valid time
            if (!radarOnRadarYes && !dataFcsts && !frameFcsts && vf > latest.getTime()) {
                // if we are dealing with live data(without forecast times) then
                // we want to allow extra time on the latest frame. For example
                // LAPS data arrives hourly, and radar arrives every 6 minutes,
                // in this scenario dt is around 36 minutes so 36 minutes after
                // the hour when radar updates LAPS disappears. This code
                // changes that so for the latest frame LAPS will be visible for
                // 90 minutes which is enough time for the next LAPS frame to
                // come in, this means that the latest frame always has data.
                v1 = vf - dtd;
            }
            fo = frameTimes[f].getLevelValue();
            spatial = fo >= 0.0 && dspatial;
            best = -1; // have no best match yet
            for (p = 0; p <= m; p++) {
                pp = majorIndex.get(p);
                vd = (depictTimes).get(pp).getMatchValid();
                if (vd > v2) {
                    break; // past last valid, no more matches possible
                }
                if (vd < v1) {
                    continue; // before first valid time, no match
                }
                if (spatial && depictTimes.get(pp).getLevelValue() != fo) {
                    continue;
                }
                if (byFcst) {
                    q = p == m ? n : majorIndex.get(p + 1);
                    fd = frameTimes[f].getMatchFcst();
                    while (pp < q && (depictTimes).get(pp).getMatchFcst() != fd) {
                        pp++;
                    }
                    if (pp >= q) {
                        continue;
                    }
                }
                best = pp; // record this match
                if (vd >= vf) {
                    break; // past frame time, new matches no better
                }
                v2 = vf + (vf - vd); // new matches must be at least as this
                // good
            }
            if (best >= 0) { // put best match into sequence

                loadTimes[f] = depictTimes.get(best);
            }

        }

        return loadTimes;
    }

    // This routine filters out any DataTimes from a sequence that have a
    // forecast time different than "forecast". Caller allocates and own
    // "times".
    // if forecast is not a multiple of 60, then we remove all forecasts without
    // the same offset of a multiple of 60. This is how we now deal with
    // ensemble perturbations. The check against 8640000 allows us to send in
    // and absolute time that is treated the same way vs perturbations except
    // that it does not actually filter forecasts.
    // ---------------------------------------------------------------------------
    static List<DataTime> filterByForecast(DataTime[] incomingTimes,
            long forecast) {
        int next, check;

        ArrayList<DataTime> times = new ArrayList<DataTime>();
        for (DataTime t : incomingTimes) {
            times.add(t);
        }

        if (times.isEmpty()) {
            return times;
        }

        long p = times.get(0).getFcstTime() % 60;
        if (forecast >= 8640000 || forecast < 0) {
            if (p == 0) {
                return times;
            }
            if (forecast > 0) {
                forecast = -forecast % 60;
            } else if (forecast != -60) {
                forecast = forecast % 60;
            }
        }

        int sz = times.size();
        if (forecast < 0) {
            for (next = check = 0; check < sz; check++) {
                if ((times).get(check).getFcstTime() % 60 != -forecast) {
                    continue;
                }
                if (next < check) {
                    times.set(next, (times).get(check));
                }
                next++;
            }
        } else if (forecast % 60 != 0) {
            for (next = check = 0; check < sz; check++) {
                if ((times).get(check).getFcstTime() != forecast) {
                    continue;
                }
                if (next < check) {
                    times.set(next, (times).get(check));
                }
                next++;
            }
        } else {
            for (next = check = 0; check < sz; check++) {
                if ((times).get(check).getMatchFcst() != forecast) {
                    continue;
                }
                if (next < check) {
                    times.set(next, (times).get(check));
                }
                next++;
            }
        }

        if (next < check) {
            for (int i = next; i < check; i++) {
                times.remove(next);
            }
        }

        return times;
    }

    // This routine filters out any DataTimes from a sequence that have a
    // reference time later than "clock". If the flag "prevRun" is true then
    // it also filters out any DataTimes with the latest reference time
    // that passes the "clock" test. This allows the caller to see the
    // previous run of some model. Returns the latest reference time remaining
    // in the sequence. Caller allocates and own "times".
    //
    // -- implementation
    // ---------------------------------------------------------
    // Correct return of latest reference time dependent on default construction
    // of AbsTime being a time_t of zero.
    // "prevRun" feature is implemented by making a recursive call with "clock"
    // set to the latest reference time in the sequence minus one.
    // ---------------------------------------------------------------------------
    private static Date filterByClock(List<DataTime> times, Date clock,
            boolean prevRun) {
        int next, check;
        long latest = 0L;
        long curRef;
        if (times == null || times.size() == 0 || clock == null) {
            return clock;
        }

        for (next = check = 0; check < times.size(); check++) {
            curRef = (times).get(check).getMatchRef();
            if (curRef > clock.getTime()) {
                continue;
            }
            if (latest < curRef) {
                latest = curRef;
            }
            if (next < check) {
                times.set(next, times.get(check));
            }
            next++;
        }

        if (next < check) {
            for (int i = next; i < check; i++) {
                times.remove(next);
            }
        }

        if (prevRun) {
            return filterByClock(times, new Date(latest - 1), false);
        } else {
            return new Date(latest);
        }
    }

    // This routine sorts a sequence of DataTimes in ascending order with
    // reference time as the primary key and forecast time as the secondary
    // key. Caller allocates and owns "times" and "majorIndex". Routine
    // expects "majorIndex" to be an empty sequence when routine is called.
    // On output, "majorIndex" contains index of first element in "times" for
    // each unique value of reference time. Sort of allows user to then
    // treat "times" as a 2d array.
    // Will not construct majorIndex if pointer is null.
    // Will return true if there are any non-zero forecast time values.
    // ---------------------------------------------------------------------------
    static void refTimeSort(List<DataTime> times, List<Integer> majorIndex,
            boolean ignoreSpatial) {
        int i, j;

        if (times.size() == 0) {
            return;
        }

        Collections.sort(times, new DataTimeComparator(
                DataTimeComparator.SortKey.INITIAL_TIME,
                DataTimeComparator.SortKey.FORECAST_TIME, true));

        if (majorIndex == null) {
            return;
        }
        majorIndex.clear();

        for (i = 0; i < times.size(); i++) {
            if (!(times).get(i).isNull()) {
                break;
            }
        }
        if (i >= times.size()) {
            return;
        }
        majorIndex.add(i);
        if (times.get(i).isSpatial() && !ignoreSpatial) {
            long val0 = times.get(i).getMatchValid();
            for (j = i + 1; j < times.size(); i++, j++) {
                if (times.get(j).getMatchRef() > times.get(i).getMatchRef()) {
                    val0 = times.get(j).getMatchValid();
                } else if (val0 != times.get(j).getMatchValid()) {
                    continue;
                }
                majorIndex.add(j);
            }
        } else {
            for (j = i + 1; j < times.size(); i++, j++) {
                if ((times).get(j).getMatchRef() > (times).get(i).getMatchRef()) {
                    majorIndex.add(j);
                }
            }
        }
    }

    // Given a sequence of available DataTimes, the maximum number of frames
    // which can be displayed, and the latest available reference time in the
    // sequence, this routine fills up the sequence "loadTimes" with the
    // the proper list of times for a row load to an empty display. By
    // definition a row load contains analysis plus forecasts from only the
    // latest run. Caller creates and owns both depictTimes and loadTimes.
    // loadTimes is assumed to be an empty sequence when call is made.
    // ---------------------------------------------------------------------------
    static Date makeRowloadList(List<DataTime> depictTimes, Date latest,
            List<DataTime> loadTimes, boolean noBackfill) {
        int p, q;

        // check for case of no data available and then sort.
        if (depictTimes.size() == 0) {
            return new Date(0);
        }
        refTimeSort(depictTimes, null, false);

        // filterOldForecasts removes any DataTimes that do not belong in a row
        // load and returns index of first item with latest reference time,
        // which
        // is the most preferred time to load.
        p = filterOldForecasts(depictTimes, latest, noBackfill);

        // Add all unfiltered times to the sequence of potential load times.
        for (q = 0; q < depictTimes.size(); q++) {
            loadTimes.add((depictTimes).get(q));
        }

        // Return valid time of preferred index.
        return new Date((depictTimes).get(p).getMatchValid());
    }

    // This routine filters out any DataTimes from a sequence that have valid
    // times that are not roughly multiples of deltaTime.
    // ---------------------------------------------------------------------------
    @SuppressWarnings("unchecked")
    static void filterByDeltaTime(List<DataTime> times, long deltaTime,
            Date preferred) {

        // cases where we know there is nothing to do
        if (times.size() < 2) {
            return;
        }
        if (deltaTime <= 1) {
            return;
        }

        // Find the preferred index.
        long thisTime = 0;
        int n = times.size() - 1;
        int prefIdx = n;
        boolean byRef = (times).get(n).getMatchValid() == (times).get(0)
                .getMatchValid();

        long dt = 0x7FFFFFFF;
        if (preferred.getTime() > 0) {
            for (int i = n; i >= 0 && dt > 0; i--) {
                if (byRef) {
                    thisTime = (times).get(i).getMatchRef();
                } else {
                    thisTime = (times).get(i).getMatchValid();
                }
                long thisdt = thisTime - preferred.getTime();
                if (thisdt < 0) {
                    thisdt = -thisdt;
                }
                if (thisdt >= dt) {
                    continue;
                }
                dt = thisdt;
                prefIdx = i;
            }
        }

        // Reset preferred value to an exact match of something in the list,
        if (byRef) {
            preferred = new Date((times).get(prefIdx).getMatchRef());
        } else {
            preferred = new Date((times).get(prefIdx).getMatchValid());
        }
        Calendar preferredCal = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        preferredCal.setTime(preferred);
        long preferredMillis = preferredCal.getTimeInMillis();

        // Make of map of expected times and times nearest to the expected times
        MultiMap expectedTimesMap = new MultiValueMap();
        long earliestMatchedTime = Long.MAX_VALUE;
        long latestMatchedTime = Long.MIN_VALUE;
        Calendar thisTimeCal = Calendar
                .getInstance(TimeZone.getTimeZone("GMT"));

        // Maximum allowable delta is 15 minutes.
        long allowableDelta = Math.min(deltaTime / 2, 900000);

        for (ListIterator<DataTime> lIter = times.listIterator(); lIter
                .hasNext();) {
            DataTime thisDataTime = lIter.next();

            if (byRef) {
                thisTime = thisDataTime.getMatchRef();
            } else {
                thisTime = thisDataTime.getMatchValid();
            }
            thisTimeCal.setTimeInMillis(thisTime);

            long diff = preferredMillis - thisTime;
            long quotient = Math.round((double) diff / (double) deltaTime);
            long expectedTime = preferredMillis - (quotient * deltaTime);
            long deltaFromExpected = thisTime - expectedTime;
            deltaFromExpected = deltaFromExpected < 0 ? -deltaFromExpected
                    : deltaFromExpected;

            if (deltaFromExpected < allowableDelta) {
                expectedTimesMap.put(expectedTime, thisDataTime);
                if (thisTime < earliestMatchedTime) {
                    earliestMatchedTime = thisTime;
                }
                if (thisTime > latestMatchedTime) {
                    latestMatchedTime = thisTime;
                }
            }
        }

        // We only keep one of any with the same timeList value. Don't keep
        // any with a time outside the range of matched data.
        times.clear();
        for (Iterator<Long> keyIter = expectedTimesMap.keySet().iterator(); keyIter
                .hasNext();) {
            Long expectedTime = keyIter.next();
            Collection<DataTime> associatedTimes = (Collection<DataTime>) expectedTimesMap
                    .get(expectedTime);
            // find the associated dataTime nearest to the expectedTime
            DataTime nearest = null;
            Long nearestDiff = Long.MAX_VALUE;
            for (DataTime aTime : associatedTimes) {
                long aTimeMillis;
                if (byRef) {
                    aTimeMillis = aTime.getMatchRef();
                } else {
                    aTimeMillis = aTime.getMatchValid();
                }
                if (aTimeMillis >= earliestMatchedTime
                        && aTimeMillis <= latestMatchedTime) {
                    long aTimeDiff = Math.abs(expectedTime - aTimeMillis);
                    if (aTimeDiff < nearestDiff) {
                        nearestDiff = aTimeDiff;
                        nearest = aTime;
                    }
                }
            }
            if (nearest != null) {
                times.add(nearest);
            }
        }
        List<Integer> majorIndex = new ArrayList<Integer>();
        validTimeSort(times, majorIndex, false);
    }

    // Given a sequence of available DataTimes and the latest available
    // reference
    // time in the sequence, this routine fills up the sequence "loadTimes" with
    // the proper list of times for loading a valid time sequence to an
    // empty display. A valid time sequence attempts to load the most recent
    // data for each available valid time. Returns valid time of preferred frame.
    // Caller creates and owns both depictTimes and loadTimes.
    // loadTimes is assumed to be an empty sequence when call is made.
    // ---------------------------------------------------------------------------
    static Date makeValTimSeqList(List<DataTime> depictTimes, Date latest,
            List<DataTime> loadTimes) {
        int p, q;

        // check for case of no data available.
        if (depictTimes.size() == 0) {
            return new Date(0);
        }

        // validTimeSort sorts primarily on valid time and secondarily on
        // forecast time. Because valid = reference + forecast, having forecast
        // time increase within each valid time group means reference time
        // decreases. Thus the first items in each valid time group, which
        // are pointed to by majorIndex, are the most recent data for each
        // valid time.
        List<Integer> majorIndex = new ArrayList<Integer>();
        validTimeSort(depictTimes, majorIndex, false);

        // identify the first item in the sequence with the latest reference
        // time, which is the most preferred time to load. Store as m.
        int m = depictTimes.size() - 1;
        for (p = 0; p < majorIndex.size(); p++) {
            if ((depictTimes).get(majorIndex.get(p)).getMatchRef() < latest
                    .getTime()) {
                continue;
            }
            m = majorIndex.get(p);
            break;
        }

        // Compile list of all potential load times.
        for (q = 0; q < majorIndex.size(); q++) {
            loadTimes.add((depictTimes).get(majorIndex.get(q)));
        }

        // Return valid time of preferred index.
        return new Date((depictTimes).get(m).getMatchValid());
    }

    // This routine, creates the appropriate list of
    // DataTimes for doing a DprogDt Load to an empty display. This selects
    // DataTimes with all the same valid time and increasing initial times.
    // Valid time used is defined as the DataTime with the most recent reference
    // time and a forecast time of "fcstTime".
    // ---------------------------------------------------------------------------
    static Date makeDprogDtList(List<DataTime> depictTimes, long fcstTime,
            List<DataTime> loadTimes) {
        int p, q, n;
        long v = 0L;

        // check for case of no data available.
        if (depictTimes.size() == 0) {
            return new Date(0);
        }

        // validTimeSort sorts primarily on valid time and secondarily on
        // forecast time. Because valid = reference + forecast, having forecast
        // time increase within each valid time group means reference time
        // decreases.
        validTimeSort(depictTimes, null, false);

        // By searching backwards thru sorted list for first occurrence of
        // specified forecast time, we find item with desired valid time that
        // has latest reference time.
        n = depictTimes.size();
        for (p = n - 1; p >= 0; p--) {
            if ((depictTimes).get(p).getMatchFcst() == fcstTime) {
                break;
            }
        }
        if (p < 0) {
            return new Date(0);
        }
        v = (depictTimes).get(p).getMatchValid(); // base valid time of the
        // dProg/dt

        // Step back again recording all matches to this valid time, return
        // last one in the list as the preferred ref time.
        for (q = n - 1; q >= 0; q--) {
            if ((depictTimes).get(q).getMatchValid() == v) {
                loadTimes.add((depictTimes).get(q));
            }
        }

        return (loadTimes).get(loadTimes.size() - 1).getRefTime();
    }

    // Given a sequence of available DataTimes for an able to depict, the clock time,
    // the desired number of frames, and a picture load mode, this routine
    // returns the sequence "loadTimes" with the proper list of times for
    // loading data to an empty display. Caller creates and owns depictTimes
    // User should check documentation on routines that
    // handle individual load modes for information on how they work.
    // Optional argument "forecast" controls how modes PROG_LOOP and
    // DPROG_DT work.
    // ---------------------------------------------------------------------------
    public static DataTime[] makeEmptyLoadList(DataTime[] depictTimes,
            Date clock, int nFrames, LoadMode mode, long forecast,
            long deltaTime) {
        List<DataTime> loadTimes = new ArrayList<DataTime>();

        Date latest;
        Date prefered = new Date(0);
        boolean preferedLast = deltaTime > 0 && forecast > 8640000;
        long filterTime = forecast < 0 || forecast >= 8640000 ? forecast
                : 8640000 + forecast;
        loadTimes.clear();
        if (depictTimes == null || depictTimes.length == 0) {
            return loadTimes.toArray(new DataTime[loadTimes.size()]);
        }

        if (mode == LoadMode.RANGE_MATCH) {
            // if in range match mode, use valid times for initial load
            mode = LoadMode.VALID_TIME_SEQ;
        }

        if (mode == LoadMode.STD) {
            List<DataTime> filteredTimes = filterByForecast(depictTimes,
                    filterTime);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = filterByClock(filteredTimes, clock, false);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            ;
            latest = makeValTimSeqList(filteredTimes, latest, loadTimes);
            if (preferedLast) {
                prefered = new Date(forecast);
            }
            if (prefered.getTime() == 0) {
                prefered = latest;
            }
            filterByDeltaTime(loadTimes, deltaTime, prefered);
            makeFinalLoadList(loadTimes, nFrames, prefered, preferedLast);
        } else if (mode == LoadMode.LATEST) {
            List<DataTime> filteredTimes = filterByForecast(depictTimes,
                    filterTime);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = filterByClock(filteredTimes, clock, false);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = makeRowloadList(filteredTimes, latest, loadTimes, false);
            if (preferedLast) {
                prefered = new Date(forecast);
            }
            if (prefered.getTime() == 0) {
                prefered = latest;
            }
            filterByDeltaTime(loadTimes, deltaTime, prefered);
            makeFinalLoadList(loadTimes, nFrames, prefered, preferedLast);
        } else if (mode == LoadMode.NO_BACKFILL) {
            List<DataTime> filteredTimes = filterByForecast(depictTimes,
                    filterTime);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = filterByClock(filteredTimes, clock, false);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = makeRowloadList(filteredTimes, latest, loadTimes, true);
            if (preferedLast) {
                prefered = new Date(forecast);
            }
            if (prefered.getTime() == 0) {
                prefered = latest;
            }
            filterByDeltaTime(loadTimes, deltaTime, prefered);
            makeFinalLoadList(loadTimes, nFrames, prefered, preferedLast);
        } else if (mode == LoadMode.PREVIOUS_MODEL_RUN) {
            List<DataTime> filteredTimes = filterByForecast(depictTimes,
                    filterTime);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = filterByClock(filteredTimes, clock, true);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = makeRowloadList(filteredTimes, latest, loadTimes, false);
            if (preferedLast) {
                prefered = new Date(forecast);
            }
            if (prefered.getTime() == 0) {
                prefered = latest;
            }
            filterByDeltaTime(loadTimes, deltaTime, prefered);
            makeFinalLoadList(loadTimes, nFrames, prefered, preferedLast);
        } else if (mode == LoadMode.VALID_TIME_SEQ) {
            List<DataTime> filteredTimes = filterByForecast(depictTimes,
                    filterTime);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }

            latest = filterByClock(filteredTimes, clock, false);

            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = makeValTimSeqList(filteredTimes, latest, loadTimes);
            if (preferedLast) {
                prefered = new Date(forecast);
            }
            if (prefered.getTime() == 0) {
                prefered = latest;
            }
            filterByDeltaTime(loadTimes, deltaTime, prefered);
            makeFinalLoadList(loadTimes, nFrames, prefered, preferedLast);
        } else if (mode == LoadMode.PREVIOUS_VALID_TIME_SEQ) {
            List<DataTime> filteredTimes = filterByForecast(depictTimes,
                    filterTime);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = filterByClock(filteredTimes, clock, true);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = makeValTimSeqList(filteredTimes, latest, loadTimes);
            if (preferedLast) {
                prefered = new Date(forecast);
            }
            if (prefered.getTime() == 0) {
                prefered = latest;
            }
            filterByDeltaTime(loadTimes, deltaTime, prefered);
            makeFinalLoadList(loadTimes, nFrames, prefered, preferedLast);
        } else if (mode == LoadMode.ANALYSIS_LOOP) {
            if (preferedLast) {
                prefered = new Date(forecast);
            }
            forecast = 0; // intentional fall thru to prog loop
        }
        if (mode == LoadMode.PROG_LOOP || mode == LoadMode.ANALYSIS_LOOP) {
            List<DataTime> filteredTimes = filterByForecast(depictTimes,
                    forecast);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = filterByClock(filteredTimes, clock, false);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = makeValTimSeqList(filteredTimes, latest, loadTimes);

            if (prefered.getTime() == 0) {
                prefered = latest;
            }
            filterByDeltaTime(loadTimes, deltaTime, prefered);
            makeFinalLoadList(loadTimes, nFrames, prefered, true);
        } else if (mode == LoadMode.SLOT || mode == LoadMode.INVENTORY
                || mode == LoadMode.FCST_TIME_MATCH) {
            List<DataTime> filteredTimes = filterByForecast(depictTimes,
                    forecast);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = filterByClock(filteredTimes, clock, false);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            makeValTimSeqList(filteredTimes, latest, loadTimes);
            if (loadTimes.size() > nFrames) {
                int sz = loadTimes.size() - (nFrames + 1);
                for (int i = 0; i < sz; i++) {
                    loadTimes.remove(0);
                }
            }
        } else if (mode == LoadMode.DPROG_DT) {
            List<DataTime> filteredTimes = filterByForecast(depictTimes,
                    filterTime);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            latest = filterByClock(filteredTimes, clock, false);
            if (filteredTimes.size() == 0) {
                return loadTimes.toArray(new DataTime[loadTimes.size()]);
            }
            forecast -= forecast % 60;
            latest = makeDprogDtList(filteredTimes, forecast, loadTimes);
            if (prefered.getTime() == 0) {
                prefered = latest;
            }
            filterByDeltaTime(loadTimes, deltaTime, prefered);
            makeFinalLoadList(loadTimes, nFrames, prefered, true);
        } else if (mode == LoadMode.FORCED) {
            // invalid clear display load modes
        }
        return loadTimes.toArray(new DataTime[loadTimes.size()]);

    }

    // ::makeOverlayList()
    //
    // Given a sequence of available DataTimes for a depictable, the clock time,
    // the current list of frame times, and a picture load mode, this routine
    // fills up the sequence "loadTimes" with the proper list of DataTimes for
    // overlaying depictables on existing data. Caller creates and owns
    // depictTimes, frameTimes, and loadTimes. loadTimes is assumed to be
    // an empty sequence when call is made. User should check documentation
    // on routines that handle individual load modes for information on how
    // they work.
    // Optional argument "forecast" controls how modes PROG_LOOP,
    // FORCED, FCST_TIME_MATCH and DPROG_DT work.
    // ---------------------------------------------------------------------------
    public DataTime[] makeOverlayList(DataTime[] depictTimes,
            Date clock, DataTime[] frameTimes, LoadMode mode, long forecast,
            long deltaTime, float tolerance) {
        // The levelvalue check has been added to allow resources on a single
        // level to avoid using spatial time matching(This must work for Radar 4
        // panels)
        Double levelvalue = null;
        for (DataTime time : depictTimes) {
            if (!time.isSpatial()
                    || (levelvalue != null && levelvalue.doubleValue() != time
                            .getLevelValue())) {
                levelvalue = null;
                break;
            }
            levelvalue = time.getLevelValue();
        }
        if (levelvalue != null) {
            for (int i = 0; i < depictTimes.length; i++) {
                depictTimes[i] = depictTimes[i].clone();
                depictTimes[i].setLevelValue(null);
            }
        }

        if (depictTimes.length == 0) {
            return new DataTime[0];
        }
        long filterTime = forecast < 0 || forecast >= 8640000 ? forecast
                : 8640000 + forecast;

        // handle case of static data
        DataTime[] loadTimes = new DataTime[0];
        // if (depictTimes.length == 1 && depictTimes[0].isStatic()) {
        // loadTimes = new DataTime[frameTimes.length];
        // for (int i = 0; i < frameTimes.length; i++)
        // loadTimes[i] = depictTimes[0];
        // radarOnRadarYes = false;
        // vbOnNonvbYes = false;
        // return loadTimes;
        // }

        // Check for case of special matching for the latest overlay data set.
        // Need default time options and non-forecast VB prod overlaid on
        // non-forecast non-VB prod.
        // while (vbOnNonvbYes) {
        // vbOnNonvbYes = false;
        // float dtol = tolerance - DEFAULT_TOLERANCE_FACTOR;
        // if (dtol > 0.0001 || dtol < -0.0001 || deltaTime != 0)
        // break;
        // if (mode != LoadMode.NO_BACKFILL && mode != LoadMode.LATEST
        // && mode != LoadMode.VALID_TIME_SEQ && mode != LoadMode.STD
        // && mode != LoadMode.PROG_LOOP)
        // break;
        // int i;
        // for (i = depictTimes.length - 1; i >= 0; i--)
        // if (depictTimes[i].getMatchFcst() != 0
        // || depictTimes[i].getRefTime().getTime() > clock
        // .getTime())
        // break;
        // if (i >= 0)
        // break;
        // for (i = frameTimes.length - 1; i >= 0; i--)
        // if (frameTimes[i].getMatchFcst() != 0
        // || frameTimes[i].getRefTime().getTime() > clock
        // .getTime())
        // break;
        // if (i < 0)
        // vbOnNonvbYes = true;
        // break;
        // }

        // filter the able to be depicted times by the clock setting.
        Date latest;
        List<DataTime> depictTimesList = new ArrayList<DataTime>(
                Arrays.asList(depictTimes));
        if (mode == LoadMode.PREVIOUS_MODEL_RUN
                || mode == LoadMode.PREVIOUS_VALID_TIME_SEQ) {
            latest = filterByClock(depictTimesList, clock, true);
        } else {
            latest = filterByClock(depictTimesList, clock, false);
        }
        depictTimes = depictTimesList.toArray(new DataTime[0]);
        List<DataTime> filteredTimes;
        switch (mode) {
        case NO_BACKFILL:
        case LATEST:
        case PREVIOUS_MODEL_RUN:
            filteredTimes = filterByForecast(depictTimes, filterTime);
            if (filteredTimes.size() == 0) {
                return new DataTime[0];
            }
            loadTimes = doValTimOverlay(filteredTimes.toArray(new DataTime[0]),
                    frameTimes, deltaTime, mode, latest, tolerance);
            break;
        case STD:
        case VALID_TIME_SEQ:
        case PREVIOUS_VALID_TIME_SEQ:
            filteredTimes = filterByForecast(depictTimes, filterTime);
            if (filteredTimes.size() == 0) {
                return new DataTime[0];
            }
            loadTimes = doValTimOverlay(filteredTimes.toArray(new DataTime[0]),
                    frameTimes, deltaTime, mode, latest, tolerance);
            break;
        case SLOT:
            filteredTimes = filterByForecast(depictTimes, forecast);
            if (!filteredTimes.isEmpty()) {
                validTimeSort(filteredTimes, null, false);
                loadTimes = new DataTime[frameTimes.length];
                Arrays.fill(loadTimes,
                        filteredTimes.get(filteredTimes.size() - 1));
            }
            break;
        case ANALYSIS_LOOP:
            forecast = 0; // intentional fall through
        case INVENTORY:
        case PROG_LOOP:
            filteredTimes = filterByForecast(depictTimes, forecast);
            loadTimes = doValTimOverlay(filteredTimes.toArray(new DataTime[0]),
                    frameTimes, deltaTime, mode, latest, tolerance);
            break;
        case DPROG_DT:
        case FCST_TIME_MATCH:
            filteredTimes = filterByForecast(depictTimes, filterTime);
            if (filteredTimes.size() == 0) {
                return new DataTime[0];
            }
            loadTimes = doValTimOverlay(filteredTimes.toArray(new DataTime[0]),
                    frameTimes, deltaTime, mode, latest, tolerance);
            break;
        case FORCED:
            loadTimes = new DataTime[frameTimes.length];
            filteredTimes = filterByForecast(depictTimes, forecast);
            refTimeSort(filteredTimes, null, false);
            {
                DataTime DunnTime = filteredTimes.get(filteredTimes.size() - 1);
                for (int i = 0; i < frameTimes.length; i++) {
                    loadTimes[i] = DunnTime;
                }
            }
            break;
        default:
            break;
        }
        // radarOnRadarYes = false; // A2 uses setRadarOnRadar().
        // If we stripped the levelvalue, restore it.
        if (levelvalue != null) {
            for (DataTime time : loadTimes) {
                if (time != null) {
                    time.setLevelValue(levelvalue);
                }
            }
        }
        return loadTimes;
    }

    public static boolean haveForecasts(DataTime[] times) {
        boolean haveForecasts = false;
        for (DataTime dataTime : times) {
            haveForecasts = (dataTime.getUtilityFlags()
                    .contains(FLAG.FCST_USED));
            if (haveForecasts) {
                break;
            }
        }
        return haveForecasts;
    }

    // Compute the possible time resolutions for a given list of DataTime
    // objects and a forecast time, based on the intrinsic period between the
    // given DataTimes. This code was ported from TclIGC.c computeTimeRes.
    // intervals returned are in seconds.
    public static List<Long> computeTimeRes(int fcstTime, DataTime[] times,
            boolean hasOffset) {
        // boolean haveForecasts = (fcstTime > 0); // original code
        boolean haveForecasts = haveForecasts(times);
        if (haveForecasts) {
            // With forecast time, filter
            filterByForecast(times, fcstTime);
        } else {
            // sort data times
            Arrays.sort(times);
        }

        long minInterval = (new TimeMatcher()).intrinsicPeriod(times, haveForecasts).intrinsicPeriod;
        // the intrinsic period interval is in milliseconds
        minInterval /= 1000;

        // Round the interval to an appropriate time step.
        boolean oneMin = minInterval < 120;
        if (minInterval >= 40000) {
            minInterval = (((minInterval + THREE_HOURS_S) / SIX_HOURS_S) * SIX_HOURS_S);
        } else if (minInterval >= 3000) {
            minInterval = (((minInterval + HALF_HOUR_S) / ONE_HOUR_S) * ONE_HOUR_S);
        } else {
            minInterval = (((minInterval + HALF_MINUTE_S) / ONE_MINUTE_S) * ONE_MINUTE_S);
        }

        // Set up the default intervals based on the data interval.
        AbstractList<Long> intervals = new LinkedList<Long>();
        intervals.add(minInterval);
        intervals.add(2 * minInterval);
        intervals.add(3 * minInterval);
        if (minInterval < 600) {
            intervals.add(5 * minInterval);
            intervals.add(10 * minInterval);
        } else {
            intervals.add(6 * minInterval);
            intervals.add(12 * minInterval);
        }

        // Add any of the auto intervals that fall within the range of
        // data intervals.
        int m = intervals.size() - 1;
        for (int a = 0; a < autoIntervals.length; a++) {
            if (autoIntervals[a] <= minInterval
                    || autoIntervals[a] > minInterval * 60) {
                continue;
            }
            int aa;
            for (aa = 0; aa <= m; aa++) {
                if (autoIntervals[a] <= intervals.get(aa)) {
                    break;
                }
            }
            if (aa > m) {
                if (autoIntervals[a] < 1.5 * intervals.get(m)) {
                    continue;
                }
                intervals.add(autoIntervals[a]);
            } else if (autoIntervals[a] == intervals.get(aa)) {
                continue;
            } else {
                intervals.add(aa, autoIntervals[a]);
            }
            m++;
        }

        // Handle case of whether this is an interval or offset.
        if (hasOffset) {
            if (intervals.get(m) < 86400) {
                intervals.add(86400L);
            }
            intervals.add(0, 0L);
            m = intervals.size() - 1;
            int a = 0;
            while (intervals.get(m) > 0) {
                intervals.add(a++, -intervals.get(m));
            }
        } else {
            if (oneMin) {
                intervals.add(1, 60L);
            }
        }
        return intervals;
    }

    public boolean isRadarOnRadar() {
        return radarOnRadarYes;
    }

    public void setRadarOnRadar(boolean radarOnRadar) {
        this.radarOnRadarYes = radarOnRadar;
    }
}
