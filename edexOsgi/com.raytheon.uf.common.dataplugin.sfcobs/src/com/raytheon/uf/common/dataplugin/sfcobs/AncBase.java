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
package com.raytheon.uf.common.dataplugin.sfcobs;

import java.util.Calendar;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Base class for the ancillary table data that require alternate observation
 * times and/or time periods over which the data was observed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 25, 2007  391      jkorman     Initial Coding.
 * Dec 03, 2013  2537     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AncBase {

    // Type of the observation.
    @DynamicSerializeElement
    @XmlAttribute
    protected Integer obsType;

    // Observation time if given.
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar obsTime;

    // Base time for the time period if different from the observation time
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar baseTime;

    // Time period (in seconds) over which the data was observed.
    @DynamicSerializeElement
    @XmlAttribute
    private Integer timePeriod;

    /**
     * Construct an empty base.
     */
    public AncBase() {
    }

    /**
     * Get the base time for the time period.
     * 
     * @return The base time.
     */
    public Calendar getBaseTime() {
        return baseTime;
    }

    /**
     * Get the time this observation is valid.
     * 
     * @return
     */
    public Calendar getObsTime() {
        return obsTime;
    }

    /**
     * Set the time this observation is valid.
     * 
     * @param oTime
     *            The observation time.
     */
    public void setObsTime(Calendar oTime) {
        obsTime = oTime;
    }

    /**
     * Get the time period (in seconds) over which the value was observed.
     * 
     * @return The observation time period.
     */
    public Integer getTimePeriod() {
        return timePeriod;
    }

    /**
     * Set the time period (in seconds) over which the value was observed. The
     * time period for point-in-time observations should be set to zero.
     * 
     * @param period
     *            Time period in seconds.
     */
    public void setTimePeriod(Integer period) {
        timePeriod = period;
    }

    /**
     * Set the base time for the time period if different from the observation
     * time.
     * 
     * @param baseTime
     *            The base time.
     */
    public void setBaseTime(Calendar baseTime) {
        this.baseTime = baseTime;
    }

    /**
     * Get the observation type for the data. Each subclass must define values
     * specific to it's data.
     * 
     * @return The observation type.
     */
    public Integer getObsType() {
        return obsType;
    }

    /**
     * Get the observation type for the data. Each subclass must define values
     * specific to it's data.
     * 
     * @param type
     *            The observation type.
     */
    public void setObsType(Integer type) {
        obsType = type;
    }
}
