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

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Various wind speed and direction information.
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
public class AncWind extends AncBase implements Serializable {

    private static final long serialVersionUID = 1L;

    // peak wind data.
    public static final int PEAK_WIND = 101;

    // max wind data.
    public static final int MAX_WIND = 102;

    // min wind data.
    public static final int MIN_WIND = 103;

    // mean wind data.
    public static final int MEAN_WIND = 104;

    // Coastal intermediate wind data.
    public static final int INTER_WIND_OBS = 1000;

    // Coastal extrapolated 10 meter speed.
    public static final int WIND_10M_SPD = 1002;

    // Coastal extrapolated 20 meter speed.
    public static final int WIND_20M_SPD = 1003;

    // wind direction in degrees.
    @DynamicSerializeElement
    @XmlAttribute
    private Integer windDirection;

    // Wind speed in meters per second
    @DynamicSerializeElement
    @XmlAttribute
    private Double windSpeed;

    /**
     * Construct an empty base.
     */
    public AncWind() {
    }

    /**
     * Constructor with known parent and observation type.
     * 
     * @param parent
     *            The parent of this class.
     * @param type
     *            The observation type for this data.
     */
    public AncWind(Integer type) {
        this.obsType = type;
    }

    /**
     * Get the wind direction in degrees.
     * 
     * @return The wind direction in degrees.
     */
    public Integer getWindDirection() {
        return windDirection;
    }

    /**
     * Set the wind direction in degrees.
     * 
     * @param windDirection
     *            The wind direction in degrees.
     */
    public void setWindDirection(Integer windDirection) {
        this.windDirection = windDirection;
    }

    /**
     * Get the wind speed in meters per second.
     * 
     * @return The observed wind speed in meters per second.
     */
    public Double getWindSpeed() {
        return windSpeed;
    }

    /**
     * Set the wind speed in meters per second.
     * 
     * @param windspeed
     *            The observed wind speed in meters per second.
     */
    public void setWindSpeed(Double windSpeed) {
        this.windSpeed = windSpeed;
    }

}
