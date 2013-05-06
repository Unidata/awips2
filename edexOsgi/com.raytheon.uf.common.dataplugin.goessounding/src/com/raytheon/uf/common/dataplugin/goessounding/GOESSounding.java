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
package com.raytheon.uf.common.dataplugin.goessounding;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

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
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.adapters.GeometryAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * The GOESSounding class encapsulates the location and time information for a
 * sounding observation as well as providing a container for the vertical level
 * data above the location.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080414           1077 jkorman     Initial implementation.
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "goessoundingseq")
@Table(name = "goessounding", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "goessounding",
		indexes = {
				@Index(name = "goessounding_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@DynamicSerialize
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class GOESSounding extends PersistablePluginDataObject implements
		ISpatialEnabled, IDecoderGettable, IPointData, IPersistable {

	private static final long serialVersionUID = 1L;

	public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

	private static final HashMap<String, String> PARM_MAP = new HashMap<String, String>();
	static {
		PARM_MAP.put("NLAT", STA_LAT);
		PARM_MAP.put("NLON", STA_LON);
	}

	@Embedded
	@DataURI(position = 1, embedded = true)
	@XmlElement
	@DynamicSerializeElement
	private SurfaceObsLocation location;

	// The bounding box that contains this observation.
	@Column(name = "boxGeometry", columnDefinition = "geometry")
	@Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
	@XmlJavaTypeAdapter(value = GeometryAdapter.class)
	@DynamicSerializeElement
	private Geometry boxGeometry;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	// Text of the WMO header
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private String wmoHeader;

	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Integer satId;

	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Integer satInstrument;

	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Integer qualityInfo;

	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Integer sounderChannels;

	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Double solarElevation;

	// The profiler observation time.
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Calendar timeObs;

	@Transient
	@XmlElement
	@DynamicSerializeElement
	private List<GOESSoundingLevel> soundingLevels;

	/**
	 * Create an empty ProfilerObs object.
	 */
	public GOESSounding() {
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
	public GOESSounding(String uri) {
		super(uri);
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
	 * @return the satInstrument
	 */
	public Integer getSatInstrument() {
		return satInstrument;
	}

	/**
	 * @param satInstrument
	 *            the satInstrument to set
	 */
	public void setSatInstrument(Integer satInstrument) {
		this.satInstrument = satInstrument;
	}

	/**
	 * @return the qualityInfo
	 */
	public Integer getQualityInfo() {
		return qualityInfo;
	}

	/**
	 * @param qualityInfo
	 *            the qualityInfo to set
	 */
	public void setQualityInfo(Integer qualityInfo) {
		this.qualityInfo = qualityInfo;
	}

	/**
	 * Get the satellite channels used to to create the sounding data.
	 * 
	 * <pre>
	 *  bit  Channel  Wavelength
	 *   #            micrometers
	 *   1       1      14.71
	 *   2       2      14.37
	 *   3       3      14.06
	 *   4       4      13.64
	 *   5       5      13.37
	 *   6       6      12.66
	 *   7       7      12.02
	 *   8       8      11.03
	 *   9       9       9.71
	 *  10      10       7.43
	 *  11      11       7.02
	 *  12      12       6.51
	 *  13      13       4.57
	 *  14      14       4.52
	 *  15      15       4.45
	 *  16      16       4.13
	 *  17      17       3.98
	 *  18      18       3.74
	 *  19      19       0.969
	 *  All 20    Missing value
	 * </pre>
	 * 
	 * @return The sounder channels.
	 */
	public Integer getSounderChannels() {
		return sounderChannels;
	}

	/**
	 * Get the satellite channels used to to create the sounding data.
	 * 
	 * @param sounderChannels
	 *            The sounder channels.
	 */
	public void setSounderChannels(Integer sounderChannels) {
		this.sounderChannels = sounderChannels;
	}

	/**
	 * Get the bounding box that contains this observation.
	 * 
	 * @return The bounding box Geometry.
	 */
	public Geometry getBoxGeometry() {
		return boxGeometry;
	}

	/**
	 * Set the bounding box that contains this observation.
	 * 
	 * @param boxGeometry
	 *            The bounding box Geometry.
	 */
	public void setBoxGeometry(Geometry boxGeometry) {
		this.boxGeometry = boxGeometry;
	}

	/**
	 * @return the solarElevation
	 */
	public Double getSolarElevation() {
		return solarElevation;
	}

	/**
	 * @param solarElevation
	 *            the solarElevation to set
	 */
	public void setSolarElevation(Double solarElevation) {
		this.solarElevation = solarElevation;
	}

	/**
	 * @return the soundingLevels
	 */
	public List<GOESSoundingLevel> getSoundingLevels() {
		return soundingLevels;
	}

	/**
	 * @param soundingLevels
	 *            the soundingLevels to set
	 */
	public void setSoundingLevels(List<GOESSoundingLevel> soundingLevels) {
		this.soundingLevels = soundingLevels;
	}

	/**
	 * @param soundingLevels
	 *            the soundingLevels to set
	 */
	public void addSoundingLevel(GOESSoundingLevel soundingLevel) {
		if (soundingLevels == null) {
			soundingLevels = new ArrayList<GOESSoundingLevel>();
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

	public void setLocation(SurfaceObsLocation location) {
		this.location = location;
	}

	public SurfaceObsLocation getLocation() {
		return location;
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
