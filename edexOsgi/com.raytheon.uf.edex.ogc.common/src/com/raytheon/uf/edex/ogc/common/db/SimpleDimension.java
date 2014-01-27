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
package com.raytheon.uf.edex.ogc.common.db;

import java.io.Serializable;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Layer dimension metadata storage object
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09/2012   754       dhladky      initial creation, based on B Clements original
 * 04/22/2013   1746      dhladky      Removed DB dependency from WFS code
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class SimpleDimension implements Comparable<SimpleDimension>,
        Serializable {

    private static final long serialVersionUID = 4654482181227204619L;

	@XmlElement
	@DynamicSerializeElement
	protected String name;

	@XmlElement
	@DynamicSerializeElement
	protected String units;

	public SimpleDimension() {
	}

	public SimpleDimension(SimpleDimension other) {
		this.name = other.name;
		this.units = other.units;
	}

	/**
	 * @return live reference to values set, should not return null
	 */
	public abstract Set<String> getValues();

	/**
	 * @param layer
	 *            the layer that this dimension belongs to. Used by method to
	 *            determine the best default for dimension.
	 * @return
	 */
    public abstract String getDefaultValue(
            SimpleLayer<? extends SimpleDimension> layer);

	/**
	 * @param lowest
	 *            set true to return lowest value, otherwise highest is returned
	 * @return null if there are no values for this dimension
	 */
	protected String getDouble(boolean lowest) {
		Double rval = getValue(lowest);
		return rval != null ? rval.toString() : null;
	}

	/**
	 * @param lowest
	 *            set true to return lowest value, otherwise highest is returned
	 * @return null if there are no values for this dimension
	 */
	protected Double getValue(boolean lowest) {
		Double rval;
		TreeSet<Double> vals = LayerTransformer.getDimValuesAsDouble(this);
		if (vals.isEmpty()) {
			rval = null;
		} else {
			rval = lowest ? vals.first() : vals.last();
		}
		return rval;
	}

	/**
	 * @param lowest
	 *            set true to return lowest value, otherwise highest is returned
	 * @return null if there are no values for this dimension
	 */
	protected String getInt(boolean lowest) {
		Double val = getValue(lowest);
		return val != null ? String.valueOf(val.intValue()) : null;
	}

    /**
     * @param lowest
     *            set true to return lowest value, otherwise highest is returned
     * @return
     */
    protected String getString(boolean lowest) {
        Set<String> values = this.getValues();
        if (values.isEmpty()) {
            return null;
        }
        TreeSet<String> sorted = new TreeSet<String>(values);
        return lowest ? sorted.first() : sorted.last();
    }

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the units
	 */
	public String getUnits() {
		return units;
	}

	/**
	 * @param units
	 *            the units to set
	 */
	public void setUnits(String units) {
		this.units = units;
	}

    @Override
    public int compareTo(SimpleDimension o) {
        return this.name.compareTo(o.name);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SimpleDimension other = (SimpleDimension) obj;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "[name=" + name + ", values=" + getValues() + "]";
    }
}
