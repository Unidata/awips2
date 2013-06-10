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
package com.raytheon.uf.common.dataplugin.poessounding;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

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
 * The POESSounding class encapsulates the location and time information for a
 * sounding observation as well as providing a container for the vertical level
 * data above the location.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 03, 2008 1026       jkorman     Initial implementation.
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * May 15, 2013 1869       bsteffen    Remove DataURI from goes/poes soundings.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "poessoundingseq")
@Table(name = "poessounding", uniqueConstraints = { @UniqueConstraint(columnNames = {
        "stationid", "reftime", "latitude", "longitude" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "poessounding",
		indexes = {
				@Index(name = "poessounding_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@DynamicSerialize
public class POESSounding extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData {

	private static final long serialVersionUID = 1L;

	// The profiler observation time.
	// @Column
	// @DynamicSerializeElement
	// @XmlElement
	// private Calendar timeObs;

	// @XmlAttribute
	// @DynamicSerializeElement
	// private Long fcstSeconds;

	// Text of the WMO header
	@Column(length = 32)
	@DynamicSerializeElement
	private String wmoHeader;

	@Transient
	private Set<POESSoundingLevel> soundingLevels;

	@Embedded
	@DataURI(position = 1, embedded = true)
	@DynamicSerializeElement
	private SurfaceObsLocation location;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	/**
	 * Create an empty ProfilerObs object.
	 */
	public POESSounding() {
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
	public POESSounding(String uri) {
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
	 * Was this location defined from the station catalog? False if not.
	 * 
	 * @return Was this location defined from the station catalog?
	 */
	public Boolean getLocationDefined() {
		return location.getLocationDefined();
	}

	/**
	 * Set the WMOHeader of the file that contained this data.
	 * 
	 * @return The wmoHeader
	 */
	public String getWmoHeader() {
		return wmoHeader;
	}

	/**
	 * Get the WMOHeader of the file that contained this data.
	 * 
	 * @param wmoHeader
	 *            The WMOHeader to set
	 */
	public void setWmoHeader(String wmoHeader) {
		this.wmoHeader = wmoHeader;
	}

	/**
	 * @return the soundingLevels
	 */
	public Set<POESSoundingLevel> getSoundingLevels() {
		return soundingLevels;
	}

	/**
	 * @param soundingLevels
	 *            the soundingLevels to set
	 */
	public void setSoundingLevels(Set<POESSoundingLevel> soundingLevels) {
		this.soundingLevels = soundingLevels;
	}

	/**
	 * @param soundingLevels
	 *            the soundingLevels to set
	 */
	public void addSoundingLevel(POESSoundingLevel soundingLevel) {
		if (soundingLevels == null) {
			soundingLevels = new HashSet<POESSoundingLevel>();
		}
		soundingLevels.add(soundingLevel);
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

	@Override
	public PointDataView getPointDataView() {
		return this.pointDataView;
	}

	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;
	}

}
