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

import java.util.Calendar;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.StringUtil;

/**
 * Subscription status summary.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2013  1653      mpduff      Initial creation
 * Aug 28, 2013  2290      mpduff      Added default missing values.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class SubscriptionStatusSummary {
    /**
     * Missing data value.
     */
    public static final long MISSING_VALUE = -9999;

    @DynamicSerializeElement
    private long dataSize = MISSING_VALUE;

    @DynamicSerializeElement
    private long startTime = MISSING_VALUE;

    @DynamicSerializeElement
    private long endTime = MISSING_VALUE;

    @DynamicSerializeElement
    private long latency = MISSING_VALUE;

    /**
     * @return the dataSize
     */
    public long getDataSize() {
        return dataSize;
    }

    /**
     * @param dataSize
     *            the dataSize to set
     */
    public void setDataSize(long dataSize) {
        this.dataSize = dataSize;
    }

    /**
     * @return the startTime
     */
    public long getStartTime() {
        return startTime;
    }

    /**
     * @param startTime
     *            the startTime to set
     */
    public void setStartTime(long startTime) {
        this.startTime = startTime;
    }

    /**
     * @return the latency
     */
    public long getLatency() {
        return latency;
    }

    /**
     * @param latency
     *            the latency to set
     */
    public void setLatency(long latency) {
        this.latency = latency;
    }

    /**
     * @return the endTime
     */
    public long getEndTime() {
        return endTime;
    }

    /**
     * @param endTime
     *            the endTime to set
     */
    public void setEndTime(long endTime) {
        this.endTime = endTime;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Data size:  ").append(this.dataSize)
                .append(StringUtil.NEWLINE);
        sb.append("Latency:    ").append(this.latency)
                .append(StringUtil.NEWLINE);

        Calendar cal = TimeUtil.newGmtCalendar();
        cal.setTimeInMillis(this.startTime);

        sb.append("Start Time: ").append(cal.getTime())
                .append(StringUtil.NEWLINE);

        cal.setTimeInMillis(this.endTime);
        sb.append("End Time:   ").append(cal.getTime())
                .append(StringUtil.NEWLINE);
        return sb.toString();
    }
}
