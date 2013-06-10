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
package com.raytheon.uf.common.dataplugin.bufrssmi;

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAttribute;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.vividsolutions.jts.geom.Geometry;

/**
 * PluginDataObject for Special Sensor Microwave/Imager data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2009            jkorman     Initial creation
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * May 17, 2013 1869       bsteffen    Remove DataURI column from sat plot
 *                                     types.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "bufrssmiseq")
@Table(name = "bufrssmi", uniqueConstraints = { @UniqueConstraint(columnNames = {
        "stationid", "refTime", "satId", "latitude", "longitude" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "bufrssmi",
		indexes = {
				@Index(name = "bufrssmi_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@DynamicSerialize
public class SSMIScanData extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData, IPersistable {

	private static final long serialVersionUID = 1L;

	@DataURI(position = 1)
	@XmlAttribute
	@DynamicSerializeElement
	private Integer satId;

	@Embedded
	@DataURI(position = 2, embedded = true)
	@DynamicSerializeElement
	private SurfaceObsLocation location;

	@DynamicSerializeElement
	@Transient
	private Integer orbitNumber;

	@DynamicSerializeElement
	@Transient
	private Integer scanNumber;

	@DynamicSerializeElement
	@Transient
	private Integer posNumber;

	// The profiler observation time.
	@Column
	@DynamicSerializeElement
	private Calendar timeObs;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	// Text of the WMO header
	@Column(length = 32)
	@DynamicSerializeElement
	private String wmoHeader;

	/**
	 * Empty constructor.
	 */
	public SSMIScanData() {
	}

	/**
	 * Constructor for DataURI construction through base class. This is used by
	 * the notification service.
	 * 
	 * @param uri
	 *            A data uri applicable to this class.
	 * @param tableDef
	 *            The table definitions for this class.
	 */
	public SSMIScanData(String uri) {
		super(uri);
	}

	/**
	 * Get this observation's geometry.
	 * 
	 * @return The geometry for this observation.
	 */
	public Geometry getGeometry() {
		return location.getGeometry();
	}

	/**
	 * Get the geometry latitude.
	 * 
	 * @return The geometry latitude.
	 */
	public double getLatitude() {
		return location.getLatitude();
	}

	/**
	 * Get the geometry longitude.
	 * 
	 * @return The geometry longitude.
	 */
	public double getLongitude() {
		return location.getLongitude();
	}

	/**
	 * Get the elevation, in meters, of the observing platform or location.
	 * 
	 * @return The observation elevation, in meters.
	 */
	public Integer getElevation() {
		return location.getElevation();
	}

	/**
	 * Was this location defined from the station catalog? False if not.
	 * 
	 * @return Was this location defined from the station catalog?
	 */
	public Boolean getLocationDefined() {
		return location.getLocationDefined();
	}

	/**
	 * @return the satId
	 */
	public Integer getSatId() {
		return satId;
	}

	/**
	 * @param satId
	 *            the satId to set
	 */
	public void setSatId(Integer satId) {
		this.satId = satId;
	}

	/**
	 * @return the orbitNumber
	 */
	public Integer getOrbitNumber() {
		return orbitNumber;
	}

	/**
	 * @param orbitNumber
	 *            the orbitNumber to set
	 */
	public void setOrbitNumber(Integer orbitNumber) {
		this.orbitNumber = orbitNumber;
	}

	/**
	 * @return the scanNumber
	 */
	public Integer getScanNumber() {
		return scanNumber;
	}

	/**
	 * @param scanNumber
	 *            the scanNumber to set
	 */
	public void setScanNumber(Integer scanNumber) {
		this.scanNumber = scanNumber;
	}

	/**
	 * @return the posNumber
	 */
	public Integer getPosNumber() {
		return posNumber;
	}

	/**
	 * @param posNumber
	 *            the posNumber to set
	 */
	public void setPosNumber(Integer posNumber) {
		this.posNumber = posNumber;
	}

	/**
	 * Get the observation time for this data.
	 * 
	 * @return The data observation time.
	 */
	public Calendar getTimeObs() {
		return timeObs;
	}

	/**
	 * Set the observation time for this data.
	 * 
	 * @param timeObs
	 *            The data observation time.
	 */
	public void setTimeObs(Calendar timeObs) {
		this.timeObs = timeObs;
	}

	/**
	 * @return the wmoHeader
	 */
	public String getWmoHeader() {
		return wmoHeader;
	}

	/**
	 * @param wmoHeader
	 *            the wmoHeader to set
	 */
	public void setWmoHeader(String wmoHeader) {
		this.wmoHeader = wmoHeader;
	}

	@Override
	public SurfaceObsLocation getSpatialObject() {
		return location;
	}

	public SurfaceObsLocation getLocation() {
		return location;
	}

	public void setLocation(SurfaceObsLocation location) {
		this.location = location;
	}

	/**
     * 
     */
	@Override
	public PointDataView getPointDataView() {
		return pointDataView;
	}

	/**
     * 
     */
	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;
	}

	/**
	 * 
	 * @return
	 */
	public final SSMIScanData copyObs() {
		SSMIScanData obs = new SSMIScanData();

		obs.dataTime = dataTime.clone();
		obs.timeObs = TimeTools.copy(timeObs);
		obs.orbitNumber = orbitNumber;
		obs.satId = satId;
		obs.scanNumber = scanNumber;
		obs.wmoHeader = wmoHeader;

		return obs;
	}

	/**
	 * Returns the hashCode for this object. This implementation returns the
	 * hashCode of the generated dataURI.
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((getDataURI() == null) ? 0 : getDataURI().hashCode());
		return result;
	}

	/**
	 * Checks if this record is equal to another by checking the generated
	 * dataURI.
	 * 
	 * @param obj
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		SSMIScanData other = (SSMIScanData) obj;
		if (getDataURI() == null) {
			if (other.getDataURI() != null) {
				return false;
			}
		} else if (!getDataURI().equals(other.getDataURI())) {
			return false;
		}
		return true;
	}

}
