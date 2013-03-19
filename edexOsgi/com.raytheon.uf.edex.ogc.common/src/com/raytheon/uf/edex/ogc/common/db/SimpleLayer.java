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
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
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
 *
 **/

@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class SimpleLayer implements IPersistableDataObject<Object> {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	protected int id;

	@Column
	@XmlElement
	@DynamicSerializeElement
	protected int nx;

	@Column
	@XmlElement
	@DynamicSerializeElement
	protected int ny;

	@Column
	@XmlElement
	@DynamicSerializeElement
	@Index(name = "name_idx")
	protected String name;

	@Column
	@XmlElement
	@DynamicSerializeElement
	protected String targetCrsCode;

	@Column
	@XmlElement
	@DynamicSerializeElement
	protected double targetMinx;

	@Column
	@XmlElement
	@DynamicSerializeElement
	protected double targetMiny;

	@Column
	@XmlElement
	@DynamicSerializeElement
	protected double targetMaxx;

	@Column
	@XmlElement
	@DynamicSerializeElement
	protected double targetMaxy;

	@Column(name = "the_geom", columnDefinition = "geometry")
	@Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
	@XmlJavaTypeAdapter(value = GeometryAdapter.class)
	@DynamicSerializeElement
	protected Polygon crs84Bounds;

	/**
	 * @return live reference to dimensions list, should not return null
	 */
    public abstract Set<? extends SimpleDimension> getDimensions();

	/**
	 * @return live reference to times set, should not return null
	 */
	public abstract SortedSet<Date> getTimes();

	public Date getDefaultTime() {
		return this.getTimes().last();
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
	public Integer getIdentifier() {
		return id;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public Polygon getCrs84Bounds() {
		return crs84Bounds;
	}

	public void setCrs84Bounds(Polygon crs84Bounds) {
		this.crs84Bounds = crs84Bounds;
	}

}
