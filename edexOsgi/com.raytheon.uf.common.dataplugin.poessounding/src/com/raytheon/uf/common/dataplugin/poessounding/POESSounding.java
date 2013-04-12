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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import javax.measure.quantity.Angle;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;
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
 * The POESSounding class encapsulates the location and time information for a
 * sounding observation as well as providing a container for the vertical level
 * data above the location.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080303           1026 jkorman     Initial implementation.
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "poessoundingseq")
@Table(name = "poessounding", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
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
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class POESSounding extends PersistablePluginDataObject implements
		ISpatialEnabled, IDecoderGettable, IPointData {

	private static final long serialVersionUID = 1L;

	public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

	private static final HashMap<String, String> PARM_MAP = new HashMap<String, String>();
	static {
		PARM_MAP.put("NLAT", STA_LAT);
		PARM_MAP.put("NLON", STA_LON);
	}

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
	@XmlElement
	@DynamicSerializeElement
	private String wmoHeader;

	@Transient
	private Set<POESSoundingLevel> soundingLevels;

	@Embedded
	@DataURI(position = 1, embedded = true)
	@XmlElement
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

	/**
	 * Return this class reference as the IDecoderGettable interface
	 * implementation.
	 * 
	 * @return Returns reference to this class.
	 */
	@Override
	public IDecoderGettable getDecoderGettable() {
		return this;
	}

	/**
	 * Get the value of a parameter that is represented as a String.
	 * 
	 * @param paramName
	 *            The name of the parameter value to retrieve.
	 * @return The String value of the parameter. If the parameter is unknown, a
	 *         null reference is returned.
	 */
	@Override
	public String getString(String paramName) {
		if ("STA".matches(paramName)) {
			return this.getStationId();
		}
		return null;
	}

	/**
	 * Get the value and units of a named parameter within this observation.
	 * 
	 * @param paramName
	 *            The name of the parameter value to retrieve.
	 * @return An Amount with value and units. If the parameter is unknown, a
	 *         null reference is returned.
	 */
	@Override
	public Amount getValue(String paramName) {
		Amount a = null;

		String pName = PARM_MAP.get(paramName);

		if (STA_LAT.equals(pName)) {
			a = new Amount(this.getLatitude(), LOCATION_UNIT);
		} else if (STA_LON.equals(pName)) {
			a = new Amount(this.getLongitude(), LOCATION_UNIT);
		}
		return a;
	}

	/**
	 * Get the value of a parameter that is represented as a collection of
	 * values.
	 * 
	 * @param paramName
	 *            The name of the parameter value to retrieve.
	 * @return The value of the parameter as an Amount. If the parameter is
	 *         unknown, a null reference is returned.
	 */
	@Override
	public Collection<Amount> getValues(String paramName) {
		return null;
	}

	/**
	 * Get the value of a parameter that is represented as a collection of
	 * Strings.
	 * 
	 * @param paramName
	 *            The name of the parameter value to retrieve.
	 * @return The value of the parameter as an String. If the parameter is
	 *         unknown, a null reference is returned.
	 */
	@Override
	public String[] getStrings(String paramName) {
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

	@Override
	public PointDataView getPointDataView() {
		return this.pointDataView;
	}

	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;
	}
}
