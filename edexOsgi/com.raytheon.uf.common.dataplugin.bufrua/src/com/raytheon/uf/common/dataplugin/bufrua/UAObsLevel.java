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
package com.raytheon.uf.common.dataplugin.bufrua;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Describes one level of data in a vertical sounding. The vertical ordinate may
 * be one or both of pressure (in Pascals) or height (in meters). At least one
 * vertical datum must be present to describe the point.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080630           1215 jkorman     Implemented Serializable.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class UAObsLevel implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    // Vertical sounding significance.
    // BUFR Code tables Version 13-07/11/2007 - 0 08 001
    // Bit No.
    // 1 Surface
    // 2 Standard level
    // 3 Tropopause level
    // 4 Maximum wind level
    // 5 Significant level, temperature and/or relative humidity
    // 6 Significant level, wind
    // All 7 Missing value
    @XmlAttribute
    @DynamicSerializeElement
    private Integer vertSig = null;

    // Observation pressure in Pascals
    @XmlAttribute
    @DynamicSerializeElement
    private Integer pressure = null;

    // Observation geopotential height in meters.
    @XmlAttribute
    @DynamicSerializeElement
    private Integer geoHeight = null;

    // Observation dry air temperature in degrees Kelvin.
    @XmlAttribute
    @DynamicSerializeElement
    private Double temp = null;

    // Observation dewpoint temperature in degrees Kelvin.
    @XmlAttribute
    @DynamicSerializeElement
    private Double dwpt = null;

    // Observation wind direction in angular degrees. Integer
    @XmlAttribute
    @DynamicSerializeElement
    private Integer windDirection = null;

    // Observation wind speed in meters per second.
    // Decimal(5,2)
    @XmlAttribute
    @DynamicSerializeElement
    private Double windSpeed = null;

    // Observation 1 KM below level wind shear in meters per second.
    // Decimal(5,2)
    @XmlAttribute
    @DynamicSerializeElement
    private Double loShear = null;

    // Observation 1 KM above level wind shear in meters per second.
    // Decimal(5,2)
    @XmlAttribute
    @DynamicSerializeElement
    private Double hiShear = null;

    /**
     * Construct an empty base.
     */
    public UAObsLevel() {
    }

    /**
     * Get the level vertical significance (level type).
     * 
     * @return the vertSig
     */
    public Integer getVertSig() {
        return vertSig;
    }

    /**
     * Set the level vertical significance (level type).
     * 
     * @param vertSig
     *            the vertSig to set
     */
    public void setVertSig(Integer vertSig) {
        this.vertSig = vertSig;
    }

    /**
     * Get the level pressure in Pascals.
     * 
     * @return the pressure
     */
    public Integer getPressure() {
        return pressure;
    }

    /**
     * Set the level pressure in Pascals.
     * 
     * @param pressure
     *            the pressure to set
     */
    public void setPressure(Integer pressure) {
        this.pressure = pressure;
    }

    /**
     * Get the level geopotential height in meters.
     * 
     * @return the geoHeight
     */
    public Integer getGeoHeight() {
        return geoHeight;
    }

    /**
     * Set the level geopotential height in meters.
     * 
     * @param geoHeight
     *            the geoHeight to set
     */
    public void setGeoHeight(Integer geoHeight) {
        this.geoHeight = geoHeight;
    }

    /**
     * Get the level dry air temperature in degrees Kelvin.
     * 
     * @return the temp
     */
    public Double getTemp() {
        return temp;
    }

    /**
     * Set the level dry air temperature in degrees Kelvin.
     * 
     * @param temp
     *            the temp to set
     */
    public void setTemp(Double temp) {
        this.temp = temp;
    }

    /**
     * Get the level dewpoint temperature in degrees Kelvin.
     * 
     * @return the dwpt
     */
    public Double getDwpt() {
        return dwpt;
    }

    /**
     * Set the level dewpoint temperature in degrees Kelvin.
     * 
     * @param dwpt
     *            the dwpt to set
     */
    public void setDwpt(Double dwpt) {
        this.dwpt = dwpt;
    }

    /**
     * Get the level wind direction in angular degrees.
     * 
     * @return the windDirection
     */
    public Integer getWindDirection() {
        return windDirection;
    }

    /**
     * Set the level wind direction in angular degrees.
     * 
     * @param windDirection
     *            the windDirection to set
     */
    public void setWindDirection(Integer windDirection) {
        this.windDirection = windDirection;
    }

    /**
     * Get the level wind speed in meters per second.
     * 
     * @return the windSpeed
     */
    public Double getWindSpeed() {
        return windSpeed;
    }

    /**
     * Set the level wind speed in meters per second.
     * 
     * @param windSpeed
     *            the windSpeed to set
     */
    public void setWindSpeed(Double windSpeed) {
        this.windSpeed = windSpeed;
    }

    /**
     * Get the level low level wind shear.
     * 
     * @return the loShear
     */
    public Double getLoShear() {
        return loShear;
    }

    /**
     * Set the level low level wind shear.
     * 
     * @param loShear
     *            the loShear to set
     */
    public void setLoShear(Double loShear) {
        this.loShear = loShear;
    }

    /**
     * Get the level high level wind shear.
     * 
     * @return the hiShear
     */
    public Double getHiShear() {
        return hiShear;
    }

    /**
     * Set the level high level wind shear.
     * 
     * @param hiShear
     *            the hiShear to set
     */
    public void setHiShear(Double hiShear) {
        this.hiShear = hiShear;
    }

    /**
     * 
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Type=");
        sb.append(vertSig);
        sb.append(" Pr=");
        sb.append(pressure);
        sb.append(" Pa:Ht=");
        sb.append(geoHeight);
        sb.append(" m:Tp=");
        sb.append(temp);
        sb.append(" K:Td=");
        sb.append(dwpt);
        sb.append(" K:Wd=");
        sb.append(windDirection);
        sb.append(" deg:Ws=");
        sb.append(windSpeed);
        sb.append(" m/s");

        return sb.toString();
    }
}
