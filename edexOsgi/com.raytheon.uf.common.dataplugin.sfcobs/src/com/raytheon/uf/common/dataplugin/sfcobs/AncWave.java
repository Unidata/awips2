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
 * Ancillary sfcobs table for ocean/lake wave data.
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
public class AncWave implements Serializable {

    public static final Integer T_WIND_WAVE = 1000;

    public static final Integer T_SWELL_WAVE = 1001;

    public static final Integer T_WAVE_HGT = 1002;

    private static final long serialVersionUID = 1L;

    // 
    @DynamicSerializeElement
    @XmlAttribute
    private Integer obsType;

    // Direction from which the swell is coming in degrees true.
    @DynamicSerializeElement
    @XmlAttribute
    private Integer wavedirection;

    // Height of the wave in meters
    @DynamicSerializeElement
    @XmlAttribute
    private Double waveheight;

    // Period of the wave in seconds
    @DynamicSerializeElement
    @XmlAttribute
    private Integer waveperiod;

    /**
     * Construct an empty instance of this class.
     */
    public AncWave() {
    }

    /**
     * Construct an instance with a specified parent and observation type.
     * 
     * @param parent
     *            The ObsCommon parent.
     * @param type
     *            The observation type.
     */
    public AncWave(Integer type) {
        obsType = type;
    }

    /**
     * Get the observation type for this data.
     * 
     * @return The observation type.
     */
    public Integer getObsType() {
        return obsType;
    }

    /**
     * Set the observation type for this data.
     * 
     * @param obsType
     *            The observation type.
     */
    public void setObsType(Integer obsType) {
        this.obsType = obsType;
    }

    /**
     * Get the wave direction in degrees.
     * 
     * @return The wavedirection in degrees.
     */
    public Integer getWavedirection() {
        return wavedirection;
    }

    /**
     * Get the wave direction in degrees.
     * 
     * @param wavedirection
     *            The wavedirection to set
     */
    public void setWavedirection(Integer wavedirection) {
        this.wavedirection = wavedirection;
    }

    /**
     * Get the wave height in meters.
     * 
     * @return The wave height in meters.
     */
    public Double getWaveheight() {
        return waveheight;
    }

    /**
     * Set the wave height in meters.
     * 
     * @param waveheight
     *            the waveheight to set
     */
    public void setWaveheight(Double waveheight) {
        this.waveheight = waveheight;
    }

    /**
     * Get the wave period in seconds.
     * 
     * @return The wave period in seconds.
     */
    public Integer getWaveperiod() {
        return waveperiod;
    }

    /**
     * Set the wave period in seconds.
     * 
     * @param waveperiod
     *            The wave period in seconds.
     */
    public void setWaveperiod(Integer waveperiod) {
        this.waveperiod = waveperiod;
    }

}
