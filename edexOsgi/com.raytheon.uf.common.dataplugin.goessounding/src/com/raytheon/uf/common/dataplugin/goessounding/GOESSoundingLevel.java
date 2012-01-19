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
package com.raytheon.uf.common.dataplugin.goessounding;

import java.io.Serializable;

import javax.persistence.Column;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * GOESSoundingLevel contains the data for a single vertical level observation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080414           1077 jkorman     Initial implementation.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class GOESSoundingLevel implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer pressure;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Double temperature;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Double dewpoint;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer height;

    /**
     * Construct an empty instance.
     */
    public GOESSoundingLevel() {
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
     * @return the dewpoint
     */
    public Double getDewpoint() {
        return dewpoint;
    }

    /**
     * @param dewpoint
     *            the dewpoint to set
     */
    public void setDewpoint(Double dewpoint) {
        this.dewpoint = dewpoint;
    }

    /**
     * @return the height
     */
    public Integer getHeight() {
        return height;
    }

    /**
     * @param height
     *            the height to set
     */
    public void setHeight(Integer height) {
        this.height = height;
    }

}
