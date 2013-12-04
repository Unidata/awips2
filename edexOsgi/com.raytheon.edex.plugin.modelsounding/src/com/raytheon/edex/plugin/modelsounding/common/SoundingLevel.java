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
package com.raytheon.edex.plugin.modelsounding.common;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * SoundingLevel contains the data for a single vertical level forecast.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 03, 2008  1026     jkorman     Initial implementation.
 * Dec 02, 2013  2537     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class SoundingLevel implements Serializable {

    private static final long serialVersionUID = 1L;

    @DynamicSerializeElement
    @XmlAttribute
    private Integer pressure;

    @DynamicSerializeElement
    @XmlAttribute
    private Double temperature;

    @DynamicSerializeElement
    @XmlAttribute
    private Double ucWind;

    @DynamicSerializeElement
    @XmlAttribute
    private Double vcWind;

    @DynamicSerializeElement
    @XmlAttribute
    private Double specificHumidity;

    @DynamicSerializeElement
    @XmlAttribute
    private Double omega;

    @DynamicSerializeElement
    @XmlAttribute
    private Double cldH2OMxRatio;

    @DynamicSerializeElement
    @XmlAttribute
    private Double iceMxRatio;

    @DynamicSerializeElement
    @XmlAttribute
    private Integer lyrCldCvr;

    @DynamicSerializeElement
    @XmlAttribute
    private Double lyrTurbKE;

    @DynamicSerializeElement
    @XmlAttribute
    private Double convLatHeat;

    @DynamicSerializeElement
    @XmlAttribute
    private Double staLatHeat;

    @DynamicSerializeElement
    @XmlAttribute
    private Double swHeatRate;

    @DynamicSerializeElement
    @XmlAttribute
    private Double lwHeatRate;

    /**
     * Construct an empty instance.
     */
    public SoundingLevel() {
    }

    /**
     * @return the pressure
     */
    public Integer getPressure() {
        return pressure;
    }

    /**
     * @param pressure
     *            the pressure to set
     */
    public void setPressure(Integer pressure) {
        this.pressure = pressure;
    }

    /**
     * @return the temperature
     */
    public Double getTemperature() {
        return temperature;
    }

    /**
     * @param temperature
     *            the temperature to set
     */
    public void setTemperature(Double temperature) {
        this.temperature = temperature;
    }

    /**
     * Get the u wind component of the horizontal wind.
     * 
     * @return the uWind
     */
    public Double getUcWind() {
        return ucWind;
    }

    /**
     * Set the u wind component of the horizontal wind.
     * 
     * @param wind
     *            the uWind to set
     */
    public void setUcWind(Double wind) {
        ucWind = wind;
    }

    /**
     * Get the v wind component of the horizontal wind.
     * 
     * @return the vWind
     */
    public Double getVcWind() {
        return vcWind;
    }

    /**
     * Set the v wind component of the horizontal wind.
     * 
     * @param wind
     *            the vWind to set
     */
    public void setVcWind(Double wind) {
        vcWind = wind;
    }

    /**
     * @return the specificHumidity
     */
    public Double getSpecificHumidity() {
        return specificHumidity;
    }

    /**
     * @param specificHumidity
     *            the specificHumidity to set
     */
    public void setSpecificHumidity(Double specificHumidity) {
        this.specificHumidity = specificHumidity;
    }

    /**
     * @return the omega
     */
    public Double getOmega() {
        return omega;
    }

    /**
     * @param omega
     *            the omega to set
     */
    public void setOmega(Double omega) {
        this.omega = omega;
    }

    /**
     * @return the cldH2OMxRatio
     */
    public Double getCldH2OMxRatio() {
        return cldH2OMxRatio;
    }

    /**
     * @param cldH2OMxRatio
     *            the cldH2OMxRatio to set
     */
    public void setCldH2OMxRatio(Double cldH2OMxRatio) {
        this.cldH2OMxRatio = cldH2OMxRatio;
    }

    /**
     * @return the iceMxRatio
     */
    public Double getIceMxRatio() {
        return iceMxRatio;
    }

    /**
     * @param iceMxRatio
     *            the iceMxRatio to set
     */
    public void setIceMxRatio(Double iceMxRatio) {
        this.iceMxRatio = iceMxRatio;
    }

    /**
     * @return the lyrCldCvr
     */
    public Integer getLyrCldCvr() {
        return lyrCldCvr;
    }

    /**
     * @param lyrCldCvr
     *            the lyrCldCvr to set
     */
    public void setLyrCldCvr(Integer lyrCldCvr) {
        this.lyrCldCvr = lyrCldCvr;
    }

    /**
     * @return the convLatHeat
     */
    public Double getConvLatHeat() {
        return convLatHeat;
    }

    /**
     * @param convLatHeat
     *            the convLatHeat to set
     */
    public void setConvLatHeat(Double convLatHeat) {
        this.convLatHeat = convLatHeat;
    }

    /**
     * @return the staLatHeat
     */
    public Double getStaLatHeat() {
        return staLatHeat;
    }

    /**
     * @param staLatHeat
     *            the staLatHeat to set
     */
    public void setStaLatHeat(Double staLatHeat) {
        this.staLatHeat = staLatHeat;
    }

    /**
     * @return the swHeatRate
     */
    public Double getSwHeatRate() {
        return swHeatRate;
    }

    /**
     * @param swHeatRate
     *            the swHeatRate to set
     */
    public void setSwHeatRate(Double swHeatRate) {
        this.swHeatRate = swHeatRate;
    }

    /**
     * @return the lwHeatRate
     */
    public Double getLwHeatRate() {
        return lwHeatRate;
    }

    /**
     * @param lwHeatRate
     *            the lwHeatRate to set
     */
    public void setLwHeatRate(Double lwHeatRate) {
        this.lwHeatRate = lwHeatRate;
    }

    /**
     * @return the lyrTurbKE
     */
    public Double getLyrTurbKE() {
        return lyrTurbKE;
    }

    /**
     * @param lyrTurbKE
     *            the lyrTurbKE to set
     */
    public void setLyrTurbKE(Double lyrTurbKE) {
        this.lyrTurbKE = lyrTurbKE;
    }

}
