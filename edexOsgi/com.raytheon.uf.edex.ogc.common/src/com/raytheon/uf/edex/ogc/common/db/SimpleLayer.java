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

import java.util.Date;
import java.util.Set;
import java.util.SortedSet;

import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.FetchType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.hibernate.annotations.Sort;
import org.hibernate.annotations.SortType;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.serialization.adapters.GeometryAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Polygon;

 /**
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2011            bclement     Initial creation
 * 04/22/2013   1746      dhladky      Removed DB dependency from WFS code
 *
 **/

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class SimpleLayer<DIMENSION extends SimpleDimension> {

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

	@Column
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

	@Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
	@XmlJavaTypeAdapter(value = GeometryAdapter.class)
	@DynamicSerializeElement
	protected Polygon crs84Bounds;
	
	@XmlElement
    @DynamicSerializeElement
    @Sort(type = SortType.NATURAL)
    @ElementCollection(fetch = FetchType.EAGER)
    protected SortedSet<Date> times;

	/**
	 * @return live reference to dimensions list, should not return null
	 */
    public abstract Set<DIMENSION> getDimensions();

	/**
	 * @return live reference to times set, should not return null
	 */
    public SortedSet<Date> getTimes() {
        return times;
    }
    
    public void setTimes(SortedSet<Date> times) {
        this.times = times;
    }

	public Date getDefaultTime() {
		return this.getTimes().last();
	}

	@Override
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

	public Polygon getCrs84Bounds() {
		return crs84Bounds;
	}

	public void setCrs84Bounds(Polygon crs84Bounds) {
		this.crs84Bounds = crs84Bounds;
	}

}
