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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.adapters.GeometryAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Layer metadata storage object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2011            bclement     Initial creation
 * 04/22/2013   1746      dhladky      Removed DB dependency from WFS code
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 * @param <DIMENSION>
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class SimpleLayer<DIMENSION extends SimpleDimension> implements
        IPersistableDataObject<String>, Serializable {

    private static final long serialVersionUID = -9102070476178937194L;

	@XmlElement
	@DynamicSerializeElement
	protected int nx;

	@XmlElement
	@DynamicSerializeElement
	protected int ny;

	@XmlElement
	@DynamicSerializeElement
	protected String name;

	@XmlElement
	@DynamicSerializeElement
	protected String targetCrsCode;

	@XmlElement
	@DynamicSerializeElement
	protected double targetMinx;

	@XmlElement
	@DynamicSerializeElement
	protected double targetMiny;

	@XmlElement
	@DynamicSerializeElement
	protected double targetMaxx;

	@XmlElement
	@DynamicSerializeElement
	protected double targetMaxy;

	@XmlJavaTypeAdapter(value = GeometryAdapter.class)
	@DynamicSerializeElement
	protected Polygon crs84Bounds;
	
	@XmlElement
    @DynamicSerializeElement
    protected TreeSet<Date> times;

    @XmlElement
    @DynamicSerializeElement
    protected boolean timesAsRanges = false;

	/**
	 * 
	 */
    public SimpleLayer(TreeSet<Date> times) {
        this.times = times;
	}

    public SimpleLayer() {
        this.times = new TreeSet<Date>();
    }

    public SimpleLayer(SimpleLayer<DIMENSION> other) {
		this.crs84Bounds = (Polygon) other.crs84Bounds.clone();
		this.name = other.name;
		this.nx = other.nx;
		this.ny = other.ny;
		this.targetCrsCode = other.targetCrsCode;
		this.targetMaxx = other.targetMaxx;
		this.targetMaxy = other.targetMaxy;
		this.targetMinx = other.targetMinx;
		this.targetMiny = other.targetMiny;
        if (other.times != null) {
            this.times = new TreeSet<Date>(other.times);
        } else {
            this.times = new TreeSet<Date>();
        }
	}

    /**
     * Merge layer values from other into this layer
     * 
     * @param other
     */
    public void update(SimpleLayer<DIMENSION> other) {
        if (other == null) {
            return;
        }
        updateDates(other);
        updateDims(other);
    }

    /**
     * Merge time values from other into this layer
     * 
     * @param other
     */
    public void updateDates(SimpleLayer<DIMENSION> other) {
        Set<Date> otherTimes = other.getTimes();
        if (otherTimes == null) {
            return;
        }
        Set<Date> thisTimes = this.getTimes();
        if (thisTimes == null) {
            setTimes(new TreeSet<Date>(otherTimes));
        } else {
            for (Date time : otherTimes) {
                thisTimes.add(time);
            }
        }
    }

    /**
     * Merge dimension values from other into this layer
     * 
     * @param other
     */
    public void updateDims(SimpleLayer<DIMENSION> other) {
        Set<DIMENSION> otherDims = other.getDimensions();
        if (otherDims == null) {
            return;
        }
        Set<DIMENSION> thisDims = this.getDimensions();
        if (thisDims != null) {
            if (thisDims != null) {
                updateDimLists(thisDims, otherDims);
            }
        }
    }

    /**
     * Merge dimension values from shinyDims into oldDims
     * 
     * @param oldDims
     * @param shinyDims
     */
    protected void updateDimLists(Set<DIMENSION> oldDims,
            Set<DIMENSION> shinyDims) {
        HashMap<String, DIMENSION> oldMap = getDimMap(oldDims);
        HashMap<String, DIMENSION> shinyMap = getDimMap(shinyDims);
        for (String name : shinyMap.keySet()) {
            DIMENSION shinyDim = shinyMap.get(name);
            DIMENSION oldDim = oldMap.get(name);
            if (oldDim == null) {
                oldDims.add(shinyDim);
            } else {
                updateDimValues(oldDim, shinyDim);
            }
        }
    }

    /**
     * Create dimension lookup map keyed by dimension name
     * 
     * @param dims
     * @return
     */
    protected HashMap<String, DIMENSION> getDimMap(Set<DIMENSION> dims) {
        HashMap<String, DIMENSION> rval = new HashMap<String, DIMENSION>(
                dims.size());
        for (DIMENSION sd : dims) {
            rval.put(sd.getName(), sd);
        }
        return rval;
    }

    /**
     * Merge values from shinyDim into oldDim
     * 
     * @param oldDim
     * @param shinyDim
     */
    protected void updateDimValues(DIMENSION oldDim, DIMENSION shinyDim) {
        Set<String> oldValues = oldDim.getValues();
        Set<String> shinyValues = shinyDim.getValues();
        if (oldValues != null && shinyValues != null) {
            for (String val : shinyValues) {
                oldValues.add(val);
            }
        }
    }

	/**
	 * @return live reference to dimensions list, should not return null
	 */
    public abstract Set<DIMENSION> getDimensions();

	/**
	 * @return live reference to times set, should not return null
	 */
    public TreeSet<Date> getTimes() {
        return times;
    }
    
    public void setTimes(TreeSet<Date> times) {
        this.times = times;
    }

	/**
	 * @return list of formatted times for layer (could be ranges)
	 */
	public List<String> getTimeEntries() {
		return LayerTransformer.getTimes(this);
	}

	public Date getDefaultTime() {
		return this.getTimes().last();
	}

    /**
     * @param dimension
     * @return null if dimension not found
     */
    public DIMENSION getDimension(String dimension) {
        DIMENSION rval = null;
        for (DIMENSION d : this.getDimensions()) {
            if (d.getName().equalsIgnoreCase(dimension)) {
                rval = d;
                break;
            }
        }
        return rval;
    }

	/**
	 * @return formatted time string (could be a range)
	 */
	public String getDefaultTimeEntry() {
		return LayerTransformer.format(getDefaultTime());
	}

	public String toString() {
		return name == null ? super.toString() : name;
	}

	public int getNx() {
		return nx;
	}

	public void setNx(int nx) {
		this.nx = nx;
	}

	public int getNy() {
		return ny;
	}

	public void setNy(int ny) {
		this.ny = ny;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getTargetCrsCode() {
		return targetCrsCode;
	}

	public void setTargetCrsCode(String targetCrsCode) {
		this.targetCrsCode = targetCrsCode;
	}

	public double getTargetMinx() {
		return targetMinx;
	}

	public void setTargetMinx(double targetMinx) {
		this.targetMinx = targetMinx;
	}

	public double getTargetMiny() {
		return targetMiny;
	}

	public void setTargetMiny(double targetMiny) {
		this.targetMiny = targetMiny;
	}

	public double getTargetMaxx() {
		return targetMaxx;
	}

	public void setTargetMaxx(double targetMaxx) {
		this.targetMaxx = targetMaxx;
	}

	public double getTargetMaxy() {
		return targetMaxy;
	}

	public void setTargetMaxy(double targetMaxy) {
		this.targetMaxy = targetMaxy;
	}

	@Override
    public String getIdentifier() {
		return name;
	}

	public Polygon getCrs84Bounds() {
		return crs84Bounds;
	}

	public void setCrs84Bounds(Polygon crs84Bounds) {
		this.crs84Bounds = crs84Bounds;
	}

    /**
     * @return the timesAsRanges
     */
    public boolean isTimesAsRanges() {
        return timesAsRanges;
    }

    /**
     * @param timesAsRanges
     *            the timesAsRanges to set
     */
    public void setTimesAsRanges(boolean timesAsRanges) {
        this.timesAsRanges = timesAsRanges;
    }

}
