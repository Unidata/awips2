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

package com.raytheon.viz.core.style.graph;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

/**
 * Style preferences related to a data axis
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2007             njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AxisScale {

    public static enum Type {
        LINEAR, LOG, SUM
    };

    private Type scaleType;

    @XmlElement(name = "min")
    private double minValue;

    @XmlElement(name = "max")
    private double maxValue;

    @XmlElement
    private boolean exactMinValue = false;

    @XmlElement
    private boolean exactMaxValue = false;

    @XmlElement
    private Double minimumRange;

    @XmlElement
    private Double baseVal;

    /**
     * @return the scaleType
     */
    public Type getScaleType() {
        return scaleType;
    }

    /**
     * @return the scaleType as string
     */
    @XmlAttribute(name = "scale")
    public String getScaleTypeAsString() {
        if (scaleType == null)
            return null;
        return scaleType.toString();
    }

    /**
     * @param scaleType
     *            the scaleType to set as string
     */
    public void setScaleTypeAsString(String scaleType) {
        this.scaleType = Type.valueOf(scaleType);
    }

    /**
     * @param scaleType
     *            the scaleType to set
     */
    public void setScaleType(Type scaleType) {
        this.scaleType = scaleType;
    }

    /**
     * @return the minValue
     */
    public double getMinValue() {
        return minValue;
    }

    /**
     * @param minValue
     *            the minValue to set
     */
    public void setMinValue(double minValue) {
        this.minValue = minValue;
    }

    /**
     * @return the maxValue
     */
    public double getMaxValue() {
        return maxValue;
    }

    /**
     * @param maxValue
     *            the maxValue to set
     */
    public void setMaxValue(double maxValue) {
        this.maxValue = maxValue;
    }

    /**
     * @return the exactMinValue
     */
    public boolean isExactMinValue() {
        return exactMinValue;
    }

    /**
     * @param exactMinValue
     *            the exactMinValue to set
     */
    public void setExactMinValue(boolean exactMinValue) {
        this.exactMinValue = exactMinValue;
    }

    /**
     * @return the exactMaxValue
     */
    public boolean isExactMaxValue() {
        return exactMaxValue;
    }

    /**
     * @param exactMaxValue
     *            the exactMaxValue to set
     */
    public void setExactMaxValue(boolean exactMaxValue) {
        this.exactMaxValue = exactMaxValue;
    }

    /**
     * @return the minimumRange
     */
    public Double getMinimumRange() {
        return minimumRange;
    }

    /**
     * @param minimumRange
     *            the minimumRange to set
     */
    public void setMinimumRange(Double minimumRange) {
        this.minimumRange = minimumRange;
    }

    /**
     * @return the baseVal
     */
    public Double getBaseVal() {
        return baseVal;
    }

    /**
     * @param baseVal
     *            the baseVal to set
     */
    public void setBaseVal(Double baseVal) {
        this.baseVal = baseVal;
    }

}
