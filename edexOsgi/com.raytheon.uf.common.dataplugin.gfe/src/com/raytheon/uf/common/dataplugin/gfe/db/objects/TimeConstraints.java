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
package com.raytheon.uf.common.dataplugin.gfe.db.objects;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.serialize.TimeConstraintsAdapter;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * A TimeConstraint represents a parm's quantum and time block alignments.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/19/2008               chammack    Ported from AWIPS I
 * 03/20/2013     #1774    randerso    Added isValid method, use TimeUtil constants,
 *                                     added serialization adapter, removed setters.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@DynamicSerialize
@DynamicSerializeTypeAdapter(factory = TimeConstraintsAdapter.class)
public class TimeConstraints implements ISerializableObject {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TimeConstraints.class);

    @DynamicSerializeElement
    private int duration;

    @DynamicSerializeElement
    private int repeatInterval;

    @DynamicSerializeElement
    private int startTime;

    boolean valid;

    /**
     * Default Constructor
     */
    public TimeConstraints() {
        duration = 0;
        repeatInterval = 0;
        startTime = 0;
        valid = false;
    }

    public TimeConstraints(int duration, int repeatInterval, int startTime) {
        this.duration = duration;
        this.repeatInterval = repeatInterval;
        this.startTime = startTime;

        if (this.duration == 0 && this.repeatInterval == 0
                && this.startTime == 0) {
            valid = true;
        } else {
            if (repeatInterval <= 0
                    || repeatInterval > TimeUtil.SECONDS_PER_DAY
                    || TimeUtil.SECONDS_PER_DAY % repeatInterval != 0
                    || repeatInterval < duration || startTime < 0
                    || startTime > TimeUtil.SECONDS_PER_DAY || duration < 0
                    || duration > TimeUtil.SECONDS_PER_DAY) {
                statusHandler.warn("Bad init values for TimeConstraints: "
                        + this);
                valid = false;
                this.duration = 0;
                this.repeatInterval = 0;
                this.startTime = 0;
            } else {
                valid = true;
            }
        }

    }

    /**
     * TimeConstraints::constraintTime() Returns the constraint time range that
     * contains the given abs time. Returns a null TimeRange if the constraint
     * parameters are invalid or if the time is not within a constraint time.
     * 
     * @param absTime
     *            the time that the range should contain
     */
    public TimeRange constraintTime(Date absTime) {
        if (!valid) {
            return new TimeRange();
        } else if (!anyConstraints()) {
            return TimeRange.allTimes();
        }

        long secSinceMidnight = absTime.getTime() % TimeUtil.MILLIS_PER_DAY;

        long midnight = (absTime.getTime() / TimeUtil.MILLIS_PER_DAY)
                * TimeUtil.MILLIS_PER_DAY;

        int tStart = startTime - repeatInterval;

        while (tStart + duration >= 0) {
            // tstart+duration is ending time
            tStart -= repeatInterval; // keep going until below 0
        }

        while (tStart < TimeUtil.SECONDS_PER_DAY) {
            int tEnd = tStart + duration;
            if ((tStart * TimeUtil.MILLIS_PER_SECOND) <= secSinceMidnight
                    && secSinceMidnight < (tEnd * TimeUtil.MILLIS_PER_SECOND)) {
                return new TimeRange(midnight + TimeUtil.MILLIS_PER_SECOND
                        * tStart, midnight + TimeUtil.MILLIS_PER_SECOND * tEnd);
            }
            tStart += repeatInterval;
        }
        return new TimeRange(); // no match
    }

    /**
     * Returns true if there are any time constraints
     * 
     * NOTE: This has the legacy implementation, even though it seems
     * incomplete.
     * 
     * @return true if any constraints are present
     */
    public boolean anyConstraints() {
        return (duration != 0);
    }

    /**
     * @return the duration
     */
    public int getDuration() {
        return duration;
    }

    /**
     * @return the repeatInterval
     */
    public int getRepeatInterval() {
        return repeatInterval;
    }

    /**
     * @return the startTime
     */
    public int getStartTime() {
        return startTime;
    }

    /**
     * @return true if valid
     */
    public boolean isValid() {
        return valid;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof TimeConstraints)) {
            return false;
        }

        TimeConstraints rhs = (TimeConstraints) obj;

        return (valid == rhs.valid && duration == rhs.duration
                && repeatInterval == rhs.repeatInterval && startTime == rhs.startTime);

    }

    /**
     * Returns true if the input time range matches the time constraints.
     * 
     * @param tr
     *            the time range
     * @return
     */
    public boolean validTR(final TimeRange tr) {
        if (!anyConstraints()) {
            return true;
        }

        // get the constraint times for the given time range
        TimeRange tr1 = constraintTime(tr.getStart());
        TimeRange tr2 = constraintTime(new Date(tr.getEnd().getTime()
                - TimeUtil.MILLIS_PER_SECOND));

        // checking
        if (!tr1.isValid() || !tr2.isValid()) {
            return false;
        } else if ((tr.getStart().equals(tr1.getStart()))
                && (tr.getEnd().equals(tr2.getEnd()))) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Returns a sequence of time ranges that represent the possible time ranges
     * that cover the given time range. This function should not be used for
     * long time periods since a very large sequence will be returned.
     * 
     * @param timeRange
     *            the range
     * @return possible time ranges
     */
    public TimeRange[] constraintTimes(final TimeRange timeRange) {
        if (!valid || !timeRange.isValid()) {
            return new TimeRange[0]; // return empty sequence
        } else if (!anyConstraints()) {
            TimeRange maxTR = TimeRange.allTimes();
            return new TimeRange[] { maxTR };
        }

        // loop through each possible start time until the ending time
        // is beyond the time range given
        List<TimeRange> sbs = new ArrayList<TimeRange>(); // returned value
        TimeRange tr = firstSB(timeRange.getStart());
        while (timeRange.getEnd().getTime()
                + (duration * TimeUtil.MILLIS_PER_SECOND) > tr.getEnd()
                .getTime()) {
            if (tr.overlaps(timeRange)) {
                sbs.add(tr);
            }
            tr = nextSB(tr); // calculate next one
        }
        return sbs.toArray(new TimeRange[sbs.size()]);
    }

    /**
     * Returns the next calculated time constraint based on the input time
     * constraint.
     * 
     * @param timeRange
     *            input time constraint
     * @return next calculated time constraint
     */
    private TimeRange nextSB(final TimeRange timeRange) {
        long nextStart = timeRange.getStart().getTime()
                + (repeatInterval * TimeUtil.MILLIS_PER_SECOND);
        long nextEnd = timeRange.getEnd().getTime()
                + (repeatInterval * TimeUtil.MILLIS_PER_SECOND);
        return new TimeRange(nextStart, nextEnd);
    }

    /**
     * Returns the first time constraint to use for searching based on the
     * starting search time.
     * 
     * @param searchTime
     *            starting search time
     * @return first time constraint
     */
    private TimeRange firstSB(Date searchTime) {
        long midnightMilliSeconds = (searchTime.getTime() / TimeUtil.MILLIS_PER_DAY)
                * TimeUtil.MILLIS_PER_DAY;

        // to catch overlap
        long ystdMidnight = midnightMilliSeconds - TimeUtil.MILLIS_PER_DAY;

        // calculate the first time range
        Date startT = new Date(ystdMidnight
                + (startTime * TimeUtil.MILLIS_PER_SECOND));
        Date endT = new Date(startT.getTime()
                + (duration * TimeUtil.MILLIS_PER_SECOND));
        return new TimeRange(startT, endT);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        if (!valid) {
            return "<Invalid>";
        } else if (!anyConstraints()) {
            return "<NoConstraints>";
        } else {
            StringBuilder sb = new StringBuilder();
            sb.append("[s=");
            sb.append(startTime / TimeUtil.SECONDS_PER_HOUR);
            sb.append("h, i=");
            sb.append(repeatInterval / TimeUtil.SECONDS_PER_HOUR);
            sb.append("h, d=");
            sb.append(duration / TimeUtil.SECONDS_PER_HOUR);
            sb.append("h]");

            return sb.toString();
        }

    }

    /**
     * expandTRToQuantum() Returns a single time range that is the input time
     * range expanded to the time constraints.
     * 
     * @param timeRange
     *            the time range
     * @return the time range
     */
    public TimeRange expandTRToQuantum(final TimeRange timeRange) {

        if (!valid || !timeRange.isValid()) {
            return new TimeRange();
        }

        if (!anyConstraints()) {
            return timeRange;
        }

        // this calculation works as long as there aren't any gaps
        if (duration == repeatInterval) {
            // calculate constraint time of startTime and endTime-1
            // The -1 is so that the last included time is checked (<, not
            // <=)
            TimeRange tr1 = constraintTime(timeRange.getStart());
            TimeRange tr2 = constraintTime(new Date(timeRange.getEnd()
                    .getTime() - TimeUtil.MILLIS_PER_SECOND));
            if (!tr1.isValid() || !tr2.isValid()) {
                return new TimeRange();
            }
            return new TimeRange(tr1.getStart(), tr2.getEnd());
        }

        // special situation -- gaps exist in the time constraint
        else {
            TimeRange[] times = constraintTimes(timeRange);
            if (times.length == 0) {
                return new TimeRange();
            } else {
                return new TimeRange(times[0].getStart(),
                        times[times.length - 1].getEnd());
            }
        }
    }

}
