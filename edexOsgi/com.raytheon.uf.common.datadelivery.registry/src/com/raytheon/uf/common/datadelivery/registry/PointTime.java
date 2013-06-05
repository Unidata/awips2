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
import java.util.Date;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
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
 * Aug 21, 2012    754         dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PointTime extends Time implements ISerializableObject,
        Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = 234624356321L;

    @XmlElement
    @DynamicSerializeElement
    private int interval;

    @XmlElements({ @XmlElement(name = "times", type = Date.class) })
    @DynamicSerializeElement
    private List<Date> times;

    /**
     * Default Constructor.
     */
    public PointTime() {

    }

    public void setTimes(List<Date> times) {
        this.times = times;
    }

    public List<Date> getTimes() {
        return times;
    }

    /**
     * gets the most recent date
     */
    @Override
    public Date getEndDate() {
        for (Date time : getTimes()) {
            if (endDate == null) {
                endDate = time;
            } else if (endDate.before(time)) {
                endDate = time;
            }
        }
        return endDate;
    }

    /**
     * gets the earliest date
     */
    @Override
    public Date getStartDate() {
        for (Date time : getTimes()) {
            if (startDate == null) {
                startDate = time;
            } else if (startDate.after(time)) {
                startDate = time;
            }
        }
        return startDate;
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
}
