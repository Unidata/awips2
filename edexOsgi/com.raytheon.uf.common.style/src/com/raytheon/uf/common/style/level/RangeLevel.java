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

package com.raytheon.uf.common.style.level;

import javax.measure.Measure;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;


/**
 * A level that is represented by a range
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "rangeLevel")
public class RangeLevel extends Level {

    protected Measure<Double, ?> lowerMeasure;

    protected Measure<Double, ?> upperMeasure;

    public RangeLevel() {
        super();
    }

    /**
     * Constructor
     * 
     * @param aType
     *            the type of level
     */
    public RangeLevel(LevelType type) {
        super(type);
    }

    /**
     * Constructor
     * 
     * @param aType
     *            the type of level
     */
    public RangeLevel(String aType) {
        super(aType);
    }

    /**
     * Sets the lower value of the level
     * 
     * @param value
     *            the lower value of the level
     */
    @XmlElement(name = "lower")
    public void setLowerValue(double value) {
        this.lowerMeasure = Measure.valueOf(value, units);
    }

    /**
     * Get the lower value of the level
     * 
     * @return the lower value of the level
     */
    public double getLowerValue() {
        return this.lowerMeasure.getValue();
    }

    /**
     * Get the lower value of the level, decorated with units
     * 
     * @return the measure
     */
    public Measure<?, ?> getLowerMeasure() {
        return this.lowerMeasure;
    }

    /**
     * Sets the upper value of the level
     * 
     * @param value
     *            the upper value of the level
     */
    @XmlElement(name = "upper")
    public void setUpperValue(double value) {
        this.upperMeasure = Measure.valueOf(value, units);
    }

    /**
     * Get the upper value of the level
     * 
     * @return the upper value of the level
     */
    public double getUpperValue() {
        return this.upperMeasure.getValue();
    }

    /**
     * Get the upper value of the level, decorated with units
     * 
     * @return the measure
     */
    public Measure<?, ?> getUpperMeasure() {
        return this.upperMeasure;
    }

}
