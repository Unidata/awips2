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
package com.raytheon.uf.common.dataplugin.svrwx;

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 4, 2010            jsanchez     Initial creation
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "svrwxseq")
@Table(name = "svrwx", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "svrwx",
		indexes = {
				@Index(name = "svrwx_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SvrWxRecord extends PersistablePluginDataObject implements
		ISpatialEnabled, IPointData {

	private static final long serialVersionUID = 1L;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	// Text of the WMO header
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private String wmoHeader = "";

	@Embedded
	@DataURI(position = 2, embedded = true)
	@XmlElement
	@DynamicSerializeElement
	private SurfaceObsLocation location;

	@Transient
	@XmlElement
	@DynamicSerializeElement
	protected String eventKey;

	@XmlElement
	@DynamicSerializeElement
	@Column
	@DataURI(position = 1)
	protected String reportType;

	@Transient
	@XmlElement
	@DynamicSerializeElement
	protected String details;

	@Transient
	@XmlElement
	@DynamicSerializeElement
	protected String greenTime;

	/**
	 * Empty default constructor
	 */
	public SvrWxRecord() {
	}

	/**
	 * Construct an instance of this class using the supplied datauri.
	 * 
	 * @param dataUri
	 */
	public SvrWxRecord(String dataUri) {
		super(dataUri);
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

	/**
	 * Set the data uri for this observation.
	 * 
	 * @param dataURI
	 */
	@Override
	public void setDataURI(String dataURI) {
		super.setDataURI(dataURI);
		identifier = dataURI;
	}

	@Override
	public IDecoderGettable getDecoderGettable() {
		return null;
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

	public String getEventKey() {
		return eventKey;
	}

	public void setEventKey(String eventKey) {
		this.eventKey = eventKey;
	}

	public String getGreenTime() {
		return greenTime;
	}

	public void setGreenTime(String greenTime) {
		this.greenTime = greenTime;
	}

	public String getDetails() {
		return details;
	}

	public void setDetails(String details) {
		this.details = details;
	}

	public String getReportType() {
		return reportType;
	}

	public void setReportType(String reportType) {
		this.reportType = reportType;
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
	 * Get the station identifier for this observation.
	 * 
	 * @return the stationId
	 */
	public String getStationId() {
		return location.getStationId();
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
	 * Get whether the location for this observation is defined.
	 * 
	 * @return Is this location defined.
	 */
	public Boolean getLocationDefined() {
		return location.getLocationDefined();
	}

	@Override
	public PointDataView getPointDataView() {
		return pointDataView;
	}

	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		Calendar c = getDataTime().getRefTimeAsCalendar();
		if (c != null) {
			sb.append(String.format("SvrWx:%1$tY%1$tm%1$td%1$tH%1$tM",
					getDataTime().getRefTimeAsCalendar()));
		} else {
			sb.append("SvrWx:YYYYMMDDHHmm");
		}
		sb.append(String.format("%6.2f %7.2f:", getLatitude(), getLongitude()));
		return sb.toString();
	}
}
