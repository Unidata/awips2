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

package com.raytheon.uf.common.dataplugin.satellite;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.hibernate.annotations.Type;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.serialization.adapters.GeometryAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Represents a map coverage area for satelllite images. It contains the
 * geometry information necessary for client applications to correctly
 * geo-locate and project satellite imagery.
 * 
 * This class maps to the spatial_satellite table in the postGres database via
 * Hibernate.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/07      353         bphillip   Initial Checkin
 * 
 * 
 * </pre>
 */
@Entity
@Table(name = "satellite_spatial")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SatMapCoverage extends PersistableDataObject implements
		ISpatialObject {

	private static final long serialVersionUID = 1L;

	@Id
	@DynamicSerializeElement
	private int gid;

	/**
	 * The projection of the map coverage 1=Mercator, 3=Lambert Conformal
	 * 5=Polar Stereographic
	 */
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Integer projection;

	/** Number of points along the x-axis */
	@XmlAttribute
	@DynamicSerializeElement
	@Column
	protected Integer nx;

	/** Number of points along the y-axis */
	@XmlAttribute
	@DynamicSerializeElement
	@Column
	protected Integer ny;

	/** The horizontal resolution of the grid */
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Float dx;

	/** The vertical resolution of the grid */
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Float dy;

	/**
	 * The orientation of the grid; i.e, the east longitude value of the
	 * meridian which is parallel to the y-axis (or columns of the grid) along
	 * which latitude increases as the y-coordinate increases (Note: the
	 * orientation longitude may or may not appear withing a particular grid.)
	 */
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Float lov;

	/**
	 * The latitude at which the Lambert projection cone is tangent to the
	 * earth. Polar Stereographic this value is set to 0. For Mercator this is
	 * The latitude at which the Mercator projection cylinder intersects the
	 * earth.
	 */
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Float latin;

	/** The latitude of the first grid point */
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Float la1;

	/** The longitude of the first grid point */
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Float lo1;

	/** The latitude of the last grid point (only used with Mercator projection) */
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Float la2;

	/**
	 * The longitude of the last grid point (only used with Mercator projection)
	 */
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Float lo2;

	@Column(length = 2047)
	@XmlAttribute
	@DynamicSerializeElement
	private String crsWKT;

	@Transient
	private CoordinateReferenceSystem crsObject;

	/** The map coverage */
	@Column(name = "the_geom", columnDefinition = "geometry")
	@Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
	@XmlJavaTypeAdapter(value = GeometryAdapter.class)
	@DynamicSerializeElement
	private Polygon location;

	public SatMapCoverage() {
		super();
	}

	/**
	 * Constructs a new SatMapCoverage Object
	 * 
	 * @param projection
	 * @param nx
	 *            The number of horizontal scan lines
	 * @param ny
	 *            The number vertical scan lines
	 * @param dx
	 *            The horizontal resolution
	 * @param dy
	 *            The vertical resolution
	 * @param lov
	 *            The orientation of the grid
	 * @param latin
	 *            The tangent latitude
	 * @param la1
	 *            The latitude of the first grid point
	 * @param lo1
	 *            The longitude of the first grid point
	 * @param la2
	 *            The latitude of the last grid point (null for Lambert
	 *            Conformal or Polar Stereographic)
	 * @param lo2
	 *            The longitude of the last grid point (null for Lambert
	 *            Conformal or Polar Stereographic)
	 * @param crs
	 *            The coordinate reference system
	 * @param geometry
	 *            The geometry
	 */
	public SatMapCoverage(Integer projection, Integer nx, Integer ny, Float dx,
			Float dy, Float lov, Float latin, Float la1, Float lo1, Float la2,
			Float lo2, CoordinateReferenceSystem crs, Geometry geometry) {

		this.projection = projection;
		this.nx = nx;
		this.ny = ny;
		this.dx = dx;
		this.dy = dy;
		this.lov = lov;
		this.latin = latin;
		this.la1 = la1;
		this.lo1 = lo1;
		this.la2 = la2;
		this.lo2 = lo2;
		this.crsObject = crs;
		this.crsWKT = crsObject.toWKT();
		this.location = (Polygon) geometry;
		gid = this.hashCode();
	}

	public int hashCode() {
		HashCodeBuilder hashBuilder = new HashCodeBuilder();
		hashBuilder.append(projection);
		hashBuilder.append(nx);
		hashBuilder.append(ny);
		hashBuilder.append(dx);
		hashBuilder.append(dy);
		hashBuilder.append(lov);
		hashBuilder.append(latin);
		hashBuilder.append(la1);
		hashBuilder.append(la2);
		hashBuilder.append(lo1);
		hashBuilder.append(lo2);
		return hashBuilder.toHashCode();
	}

	@Override
	public Polygon getGeometry() {
		return location;
	}

	@Override
	public CoordinateReferenceSystem getCrs() {
		if (crsObject == null) {
			try {
				crsObject = CRSCache.getInstance()
						.getCoordinateReferenceSystem(crsWKT);
			} catch (FactoryException e) {
				crsObject = null;
			}
		}
		return crsObject;
	}

	public Float getDx() {
		return dx;
	}

	public void setDx(Float dx) {
		this.dx = dx;
	}

	public Float getDy() {
		return dy;
	}

	public void setDy(Float dy) {
		this.dy = dy;
	}

	public Float getLov() {
		return lov;
	}

	public void setLov(Float lov) {
		this.lov = lov;
	}

	public Float getLatin() {
		return latin;
	}

	public void setLatin(Float latin) {
		this.latin = latin;
	}

	public Float getLa1() {
		return la1;
	}

	public void setLa1(Float la1) {
		this.la1 = la1;
	}

	public Float getLo1() {
		return lo1;
	}

	public void setLo1(Float lo1) {
		this.lo1 = lo1;
	}

	public Float getLa2() {
		return la2;
	}

	public void setLa2(Float la2) {
		this.la2 = la2;
	}

	public Float getLo2() {
		return lo2;
	}

	public void setLo2(Float lo2) {
		this.lo2 = lo2;
	}

	public Integer getProjection() {
		return projection;
	}

	public void setProjection(Integer projection) {
		this.projection = projection;
	}

	public int getGid() {
		return gid;
	}

	public void setGid(int gid) {
		this.gid = gid;
	}

	public Integer getNx() {
		return nx;
	}

	public void setNx(Integer nx) {
		this.nx = nx;
	}

	public Integer getNy() {
		return ny;
	}

	public void setNy(Integer ny) {
		this.ny = ny;
	}

	public String getCrsWKT() {
		return crsWKT;
	}

	public void setCrsWKT(String crsWKT) {
		this.crsWKT = crsWKT;
	}

	public Polygon getLocation() {
		return location;
	}

	public void setLocation(Polygon location) {
		this.location = location;
	}

}
