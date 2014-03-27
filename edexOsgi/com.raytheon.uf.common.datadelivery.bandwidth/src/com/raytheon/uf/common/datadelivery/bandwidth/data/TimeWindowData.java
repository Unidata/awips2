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
import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.StringUtil;

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
 * Jan 23, 2014   2636     mpduff      Removed binStartTimes, add base time and offset.
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

    /** The network for the data */
    @DynamicSerializeElement
    private Network network;

    @DynamicSerializeElement
    private long baseTime;

    @DynamicSerializeElement
    private int offset;

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
     * @return the offset
     */
    public int getOffset() {
        return offset;
    }

    /**
     * @param offset
     *            the offset to set
     */
    public void setOffset(int offset) {
        this.offset = offset;
    }

    /**
     * @return the baseTime
     */
    public long getBaseTime() {
        return baseTime;
    }

    /**
     * @param baseTime
     *            the baseTime to set
     */
    public void setBaseTime(long baseTime) {
        this.baseTime = baseTime;
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
        sb.append(StringUtil.NEWLINE);
        cal.setTimeInMillis(this.timeWindowEndTime);
        sb.append("End Time:\t").append(sdf.format(cal.getTime())).append(" Z");
        cal.setTimeInMillis(this.baseTime);
        sb.append(StringUtil.NEWLINE);
        sb.append("Base Time:\t").append(sdf.format(cal.getTime()))
                .append(" Z");
        sb.append(StringUtil.NEWLINE).append("Availability Offset: ")
                .append(offset).append(" minutes");
        return sb.toString();
    }
}
