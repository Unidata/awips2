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
 * Ancillary sfcobs table for temperature data.
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
public class AncTemp extends AncBase implements Serializable {

    private static final long serialVersionUID = 1L;

    public static final Integer T_AIR_TEMP = 10;

    public static final Integer T_MAX_AIR_TEMP = 11;

    public static final Integer T_MIN_AIR_TEMP = 12;

    public static final Integer T_MEAN_AIR_TEMP = 13;

    public static final Integer T_US_CITY_TEMP = 1010;

    public static final Integer T_US_CITY_MAX = 1011;

    public static final Integer T_US_CITY_MIN = 1012;

    /**
     * Air temperature in degrees Kelvin
     */
    @DynamicSerializeElement
    @XmlAttribute
    private Double value;

    /**
     * Construct an empty instance of this class.
     */
    public AncTemp() {

    }

    /**
     * Get the temperature value in degrees Kelvin.
     * 
     * @return The value in degrees Kelvin.
     */
    public Double getValue() {
        return value;
    }

    /**
     * Set the temperature value in degrees Kelvin.
     * 
     * @param value
     *            The value in degrees Kelvin.
     */
    public void setValue(Double value) {
        this.value = value;
    }

}
