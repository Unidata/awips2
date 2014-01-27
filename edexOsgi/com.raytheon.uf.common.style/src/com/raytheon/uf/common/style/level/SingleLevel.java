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
 * Represents a vertical level
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2007            chammack    Initial Creation.	
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "singleLevel")
public class SingleLevel extends Level implements Comparable<SingleLevel> {

    protected Measure<Double, ?> measure;

    public SingleLevel() {
        super();
    }

    /**
     * Constructor
     * 
     * Create a level of the specified type
     * 
     * @param type
     *            the type
     */
    public SingleLevel(LevelType type) {
        super(type);
    }

    /**
     * Constructor
     * 
     * Create a level of the specified type
     * 
     * @param type
     *            the type
     */
    public SingleLevel(String aType) {
        super(aType);
    }

    /**
     * Set the value of the level
     * 
     * @param the
     *            value of the level
     */
    @XmlElement
    public void setValue(double value) {
        this.measure = Measure.valueOf(value, units);
    }

    /**
     * Gets the value of the level
     * 
     * @return the value of the level
     */
    public double getValue() {
        return this.measure.getValue();
    }

    /**
     * Get the value of the level, decorated with units
     * 
     * @return the measure
     */
    public Measure<?, ?> getMeasure() {
        return this.measure;
    }

    @Override
    public int compareTo(SingleLevel o) {
        int flip = 1;
        // pressure levels are in the opposite order
        if (o.getType() == Level.LevelType.PRESSURE) {
            flip = -1;
        }
        int retVal = 0;
        if (o.getValue() > this.getValue()) {
            retVal = -1;
        } else if (o.getValue() < this.getValue()) {
            retVal = 1;
        }

        return retVal * flip;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((measure == null) ? 0 : measure.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        final SingleLevel other = (SingleLevel) obj;
        if (measure == null) {
            if (other.measure != null)
                return false;
        } else if (!measure.equals(other.measure))
            return false;
        return true;
    }

}
