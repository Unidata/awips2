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
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 5, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class InterWinds {

    
    // Observation time if given.
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar obsTime;

    // Wind direction in degrees
    @DynamicSerializeElement
    @XmlAttribute
    private Double windDir;
    
    // Wind speed in meters per second
    @DynamicSerializeElement
    @XmlAttribute
    private Double windSpeed;

    /**
     * Empty constructor.
     */
    public InterWinds() {
    }
    
    /**
     * 
     * @param time Time the wind data was observed.
     * @param dir The observed wind direction in degrees.
     * @param speed The observed wind speed in meters/second.
     */
    public InterWinds(Calendar time, Double dir, Double speed) {
        obsTime = time;
        windDir = dir;
        windSpeed = speed;
    }

    /**
     * Get the wind data observation time.
     * @return The wind data observation time.
     */
    public Calendar getObsTime() {
        return obsTime;
    }

    /**
     * Set the wind data observation time.
     * @param time The wind data observation time.
     */
    public void setObsTime(Calendar time) {
        obsTime = time;
    }
    
    /**
     * Get the observed wind direction. 
     * @return The observed wind direction in degrees.
     */
    public Double getWindDir() {
        return windDir;
    }

    /**
     * Set the observed wind direction.
     * @param dir The observed wind direction in degrees.
     */
    public void setWindDir(Double dir) {
        windDir = dir;
    }

    /**
     * Get the observed wind speed.
     * @return The observed wind speed in meters/second.
     */
    public Double getWindSpeed() {
        return windSpeed;
    }

    /**
     * Set the observed wind speed.
     * @param speed The observed wind speed in meters/second.
     */
    public void setWindSpeed(Double speed) {
        windSpeed = speed;
    }
}
