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
package com.raytheon.uf.common.datadelivery.bandwidth.data;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Time Window Data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 25, 2012   1269     lvenable    Initial creation.
 * Dec 06, 2012   1397     djohnson    Add dynamic serialize class annotation.
 * Jan 07, 2013   1451     djohnson    Use TimeUtil.newGmtCalendar().
 * Nov 25, 2013   2545     mpduff      Add Network.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
@DynamicSerialize
public class TimeWindowData implements Comparable<TimeWindowData> {
    /** Latency start time in milliseconds. Rounded to minutes. */
    @DynamicSerializeElement
    private long timeWindowStartTime = 0L;

    /** Latency start time in milliseconds. Rounded to minutes. */
    @DynamicSerializeElement
    private long timeWindowEndTime = 0L;

    /** Array of bin start times for this time window. */
    @DynamicSerializeElement
    private List<Long> binStartTimes;

    /** The network for the data */
    @DynamicSerializeElement
    private Network network;

    /**
     * Constructor.
     * 
     * @deprecated Required by dynamic serialization, use
     *             {@link #TimeWindowData(long, long)} instead.
     */
    @Deprecated
    public TimeWindowData() {
        this(0, 0);
    }

    /**
     * Constructor
     * 
     * @param windowStartTime
     *            Latency start time in milliseconds.
     * @param windowEndTime
     *            Latency end time in milliseconds.
     */
    public TimeWindowData(long windowStartTime, long windowEndTime) {

        /*
         * Truncate the start and end time to the minute. The time should
         * already be at the minute so this maybe redundant but we need to
         * remove the seconds and millisecond to graph properly.
         */
        this.timeWindowStartTime = (windowStartTime / TimeUtil.MILLIS_PER_MINUTE)
                * TimeUtil.MILLIS_PER_MINUTE;
        this.timeWindowEndTime = (windowEndTime / TimeUtil.MILLIS_PER_MINUTE)
                * TimeUtil.MILLIS_PER_MINUTE;
        binStartTimes = new ArrayList<Long>();
    }

    /**
     * Set the bin times.
     * 
     * @param binTimesArray
     */
    public void setBinTimes(List<Long> binTimesArray) {
        binStartTimes = binTimesArray;
        sortBinStartTimes();
    }

    /**
     * Add a bin time.
     * 
     * @param binStartTime
     */
    public void addBinTime(Long binStartTime) {
        if (validBinStartTime(binStartTime)) {
            long roundedBinTime = (binStartTime / TimeUtil.MILLIS_PER_MINUTE)
                    * TimeUtil.MILLIS_PER_MINUTE;
            binStartTimes.add(roundedBinTime);
            sortBinStartTimes();
            return;
        }
    }

    /**
     * Validate the bin time.
     * 
     * @param binStartTime
     * @return true if bin time is within the time window
     */
    private boolean validBinStartTime(Long binStartTime) {
        return binStartTime >= timeWindowStartTime
                && binStartTime <= timeWindowEndTime;
    }

    /**
     * Sort the bin times.
     */
    public void sortBinStartTimes() {
        Collections.sort(binStartTimes);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(TimeWindowData otherBGD) {
        if (timeWindowStartTime < otherBGD.timeWindowStartTime) {
            return -1;
        } else if (timeWindowStartTime > otherBGD.timeWindowStartTime) {
            return 1;
        }

        return 0;
    }

    /**
     * Get the time window start time.
     * 
     * @return
     */
    public long getTimeWindowStartTime() {
        return timeWindowStartTime;
    }

    /**
     * Get the time window end time.
     * 
     * @return
     */
    public long getTimeWindowEndTime() {
        return timeWindowEndTime;
    }

    /**
     * Get the time window end time.
     * 
     * @return
     */
    public List<Long> getBinStartTimes() {
        return binStartTimes;
    }

    /**
     * @param timeWindowStartTime
     *            the timeWindowStartTime to set
     */
    public void setTimeWindowStartTime(long timeWindowStartTime) {
        this.timeWindowStartTime = timeWindowStartTime;
    }

    /**
     * @param timeWindowEndTime
     *            the timeWindowEndTime to set
     */
    public void setTimeWindowEndTime(long timeWindowEndTime) {
        this.timeWindowEndTime = timeWindowEndTime;
    }

    /**
     * @param binStartTimes
     *            the binStartTimes to set
     */
    public void setBinStartTimes(List<Long> binStartTimes) {
        this.binStartTimes = binStartTimes;
    }

    /**
     * @return the network
     */
    public Network getNetwork() {
        return network;
    }

    /**
     * @param network
     *            the network to set
     */
    public void setNetwork(Network network) {
        this.network = network;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        Calendar cal = TimeUtil.newGmtCalendar();
        cal.setTimeInMillis(this.timeWindowStartTime);

        StringBuilder sb = new StringBuilder();
        sb.append("Start Time:\t").append(sdf.format(cal.getTime()))
                .append(" Z");
        sb.append("\n");
        cal.setTimeInMillis(this.timeWindowEndTime);
        sb.append("End Time:\t").append(sdf.format(cal.getTime())).append(" Z");
        return sb.toString();
    }
}
