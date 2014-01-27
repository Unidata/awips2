package com.raytheon.uf.common.datadelivery.registry;

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

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.SortedSet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.google.common.collect.Sets;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * PointTime
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012    754      dhladky     Initial creation
 * Sept 11, 2013  2351      dhladky     Added more point intervals
 * Sept 17, 2013  2383      bgonzale    Use end or start time when times are
 *                                      null because times are not always set.
 * Sept 30, 2013  1797      dhladky     separation of gridded time from time
 * Oct 10, 2013   1797      bgonzale    Refactored registry Time objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PointTime extends Time implements Serializable {

    private static final long serialVersionUID = 234624356321L;

    @XmlElement
    @DynamicSerializeElement
    private int interval;

    @XmlElements({ @XmlElement(name = "times", type = Date.class) })
    @DynamicSerializeElement
    private List<Date> times;
    
    /**
     * Intervals for point request
     */
    public static final SortedSet<Integer> INTERVALS = Sets.newTreeSet(Arrays.asList(5, 10, 15, 20, 30, 60));

    /**
     * Default Constructor.
     */
    public PointTime() {

    }
    
    /**
     * Clone constructor.
     * 
     * @param the
     *            {@link PointTime} to clone
     */
    public PointTime(PointTime toCopy) {
        super(toCopy);
        this.times = toCopy.times;
        this.interval = toCopy.interval;
    }

    public void setTimes(List<Date> times) {
        this.times = times;
    }

    public List<Date> getTimes() {
        return times;
    }

    /**
     * gets the most recent date
     * 
     */
    @Override
    public Date getEnd() {
        List<Date> timesList = getTimes();
        if (timesList != null) {
            for (Date time : timesList) {
                if (end == null) {
                    end = time;
                } else if (end.before(time)) {
                    end = time;
                }
            }
        }
        return super.getEnd();
    }

    /**
     * gets the earliest date
     * 
     */
    @Override
    public Date getStart() {
        List<Date> timesList = getTimes();
        if (timesList != null) {
            for (Date time : timesList) {
                if (start == null) {
                    start = time;
                } else if (start.after(time)) {
                    start = time;
                }
            }
        }
        return super.getStart();
    }

    /**
     * @return the interval
     */
    public int getInterval() {
        return interval;
    }

    /**
     * @param interval
     *            the interval to set
     */
    public void setInterval(int interval) {
        this.interval = interval;
    }

    /**
     * Get the allowed refresh intervals. This should be a configurable value at
     * some point.
     * 
     * @return the allowed refresh intervals
     */
    public static SortedSet<Integer> getAllowedRefreshIntervals() {
        return INTERVALS;
    }
}
