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
package com.raytheon.uf.common.dataplugin.bufrua;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.quantity.Angle;
import javax.measure.quantity.Length;
import javax.measure.quantity.Temperature;
import javax.measure.quantity.Velocity;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
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
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.vividsolutions.jts.geom.Geometry;

/**
 * UAObs represents a single vertical upper air observation at a single point in
 * time and space. The observation serves as a container for an indefinite
 * number of levels of data. This class may represent a fixed or mobile
 * location. For mobile stations, the station's movement information may be
 * captured as well as it's location.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080104            712 jkorman     Lat/Lon were set incorrectly.   
 * 20080107            720 jkorman     remove default assignments from attributes.
 * 20080108            382 jkorman     Added IVerticalSoundingCreator impl.
 * 20080114            763 jkorman     Added &quot;below&quot; ground level exclusion to
 *                                     getValue.
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "bufruaseq")
@Table(name = "bufrua", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "bufrua",
		indexes = {
				@Index(name = "bufrua_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class UAObs extends PersistablePluginDataObject implements
		ISpatialEnabled, IDecoderGettable, IPointData, IPersistable {

	private static final long serialVersionUID = 1L;

	private static final Comparator<UAObs> corComparator = new Comparator<UAObs>() {
		@Override
		public int compare(UAObs a, UAObs b) {
			int compValue = 0;
			String wmoA = a.getWmoHeader();
			String wmoB = b.getWmoHeader();

			if (wmoA != null) {
				if (wmoB != null) {
					compValue = wmoA.compareTo(wmoB);
				}
			}
			if (compValue != 0) {
				compValue *= -1;
			}
			return compValue;
		}
	};

	public static final Unit<Length> DISTANCE_UNIT = SI.METER;

	public static final Unit<Temperature> TEMPERATURE_UNIT = SI.KELVIN;

	public static final Unit<Velocity> WIND_SPEED_UNIT = SI.METERS_PER_SECOND;

	public static final Unit<Angle> WIND_DIR_UNIT = NonSI.DEGREE_ANGLE;

	public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

	private static final HashMap<String, String> PARM_MAP = new HashMap<String, String>();
	static {
		PARM_MAP.put("GH", UA_GEOHGT);
		PARM_MAP.put("Px", UA_PRESSURE);

		PARM_MAP.put("T", SFC_TEMP);
		PARM_MAP.put("DpT", SFC_DWPT);

		PARM_MAP.put("WS", SFC_WNDSPD);
		PARM_MAP.put("WD", SFC_WNDDIR);

		PARM_MAP.put("NLAT", STA_LAT);
		PARM_MAP.put("NLON", STA_LON);
	}

	public static final String UA_PARAM_PTRN = ".*:PRESS=\\d{2,4}";

	// Non persisted value. Hold the last requested parameter name.
	@Transient
	private String parameterName = null;

	// Non persisted value. Hold the last requested level value.
	@Transient
	private Integer levelId;

	// Time of the observation.
	@Column
	@XmlElement
	@DynamicSerializeElement
	private Calendar validTime;

	// Time of the observation to the nearest hour.
	@Column
	@XmlElement
	@DynamicSerializeElement
	private Calendar refHour;

	// The observation report type.
	@DataURI(position = 1)
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Integer reportType;

	@Embedded
	@DataURI(position = 4, embedded = true)
	@XmlElement
	@DynamicSerializeElement
	private SurfaceObsLocation location;

	// Correction indicator from wmo header
	@DataURI(position = 2)
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private String corIndicator;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	// Text of the WMO header
	@DataURI(position = 3)
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private String wmoHeader;

	// Station pressure in Pascals.
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Integer pressure_station;

	// The total cloud cover in 1/8s coverage.
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Integer totalCloudCover;

	// The platform directio in angular degrees.
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Integer platformDirection;

	// The platform movement in meters per second.
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Double platformMovement;

	// ICAO of station if known.
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private String stationName;

	// The level data for this observation.
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private List<UAObsLevel> levels;

	@Column(insertable = false, updatable = false)
	@XmlAttribute
	@DynamicSerializeElement
	private Integer idx;

	public void setIdx(Integer idx) {
		this.idx = idx;
	}

	public Integer getIdx() {
		return idx;
	}

	/**
	 * Empty constructor.
	 */
	public UAObs() {
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
	public UAObs(String uri) {
		super(uri);
		corIndicator = "null".equals(corIndicator) ? null : corIndicator;
		if (location != null) {
			String staId = location.getStationId();
			location.setStationId("null".equals(staId) ? null : staId);
		}
	}

	/**
	 * Get the set of levels for this observation.
	 * 
	 * @return The level data.
	 */
	public List<UAObsLevel> getLevels() {
		return levels;
	}

	/**
	 * Set the set of levels for this observation.
	 * 
	 * @param levels
	 *            the levels to set
	 */
	public void setLevels(List<UAObsLevel> levels) {
		this.levels = levels;
	}

	/**
	 * 
	 * @param cloud
	 */
	public void addLevel(UAObsLevel level) {
		if (levels == null) {
			levels = new ArrayList<UAObsLevel>();
		}
		levels.add(level);
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
	 * Get the report correction indicator.
	 * 
	 * @return The corIndicator
	 */
	public String getCorIndicator() {
		return corIndicator;
	}

	/**
	 * Set the report correction indicator.
	 * 
	 * @param corIndicator
	 *            The corIndicator.
	 */
	public void setCorIndicator(String corIndicator) {
		this.corIndicator = corIndicator;
	}

	/**
	 * Get the report data for this observation.
	 * 
	 * @return The Report data.
	 */
	public String getReportData() {
		String s = null;
		if (messageData instanceof String) {
			s = (String) messageData;
		}
		return s;
	}

	/**
	 * Set the report data for this observation.
	 * 
	 * @param reportData
	 *            The Report data.
	 */
	public void setReportData(String reportData) {
		messageData = reportData;
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

	/**
	 * Get the observation report type.
	 * 
	 * @return the reportType
	 */
	public Integer getReportType() {
		return reportType;
	}

	/**
	 * Set the observation report type.
	 * 
	 * @param reportType
	 *            the reportType to set
	 */
	public void setReportType(Integer reportType) {
		this.reportType = reportType;
	}

	/**
	 * @return the validTime
	 */
	public Calendar getValidTime() {
		return validTime;
	}

	/**
	 * @param validTime
	 *            the validTime to set
	 */
	public void setValidTime(Calendar validTime) {
		this.validTime = validTime;
	}

	/**
	 * Get the reference hour
	 * 
	 * @return the refHour
	 */
	public Calendar getRefHour() {
		return refHour;
	}

	/**
	 * Set the reference hour
	 * 
	 * @param refHour
	 *            the refHour to set
	 */
	public void setRefHour(Calendar refHour) {
		this.refHour = refHour;
	}

	/**
	 * Get the station pressure at the observation site.
	 * 
	 * @return the pressure_station
	 */
	public Integer getPressure_station() {
		return pressure_station;
	}

	/**
	 * Set the station pressure at the observation site.
	 * 
	 * @param pressure_station
	 *            the pressure_station to set
	 */
	public void setPressure_station(Integer pressure_station) {
		this.pressure_station = pressure_station;
	}

	/**
	 * Get the total clould cover (n/8s).
	 * 
	 * @return the totalCloudCover
	 */
	public Integer getTotalCloudCover() {
		return totalCloudCover;
	}

	/**
	 * Get the direction the platform is moving. (Valid only for mobile
	 * observations i.e. TEMPSHIP.
	 * 
	 * @return the platformDirection
	 */
	public Integer getPlatformDirection() {
		return platformDirection;
	}

	/**
	 * Set the direction the platform is moving. (Valid only for mobile
	 * observations i.e. TEMPSHIP.
	 * 
	 * @param platformDirection
	 *            the platformDirection to set
	 */
	public void setPlatformDirection(Integer platformDirection) {
		this.platformDirection = platformDirection;
	}

	/**
	 * Get the movement of the platform in meters per second.
	 * 
	 * @return The platform movement in meters per second.
	 */
	public Double getPlatformMovement() {
		return platformMovement;
	}

	/**
	 * Set the movement of the platform in meters per second.
	 * 
	 * @param shipMovement
	 *            The platform movement in meters per second.
	 */
	public void setPlatformMovement(Double platformMovement) {
		this.platformMovement = platformMovement;
	}

	/**
	 * Set the total clould cover (n/8s).
	 * 
	 * @param totalCloudCover
	 *            the totalCloudCover to set
	 */
	public void setTotalCloudCover(Integer totalCloudCover) {
		this.totalCloudCover = totalCloudCover;
	}

	/**
	 * @return the stationName
	 */
	public String getStationName() {
		return stationName;
	}

	/**
	 * @param stationName
	 *            the stationName to set
	 */
	public void setStationName(String stationName) {
		this.stationName = stationName;
	}

	/**
	 * Set the data uri for this observation.
	 * 
	 * @param A
	 *            data uri.
	 */
	@Override
	public void setDataURI(String dataURI) {
		identifier = dataURI;
	}

	/**
	 * Get the IDecoderGettable reference for this record.
	 * 
	 * @return The IDecoderGettable reference for this record.
	 */
	@Override
	public IDecoderGettable getDecoderGettable() {
		return this;
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
	 * Get the value and units of a named parameter within this observation. The
	 * parameter name may include level information for these observation data.
	 * The format for parameter is:
	 * 
	 * <pre>
	 *    &quot;parameterName&quot; may be one of 
	 *        &quot;GH&quot;  geopotential height
	 *        &quot;Px&quot;  pressure
	 *        &quot;T&quot;   temperature
	 *        &quot;DpT&quot; dewpoint
	 *        &quot;WS&quot;  wind speed
	 *        &quot;WD&quot;  wind direction
	 *    followed by a level specification &quot;:PRESS=xxxx&quot; where xxxx is a level
	 *    in hPa (millibars). To retrieve the temperature from the 850hPa level
	 *    use the following getValue(&quot;T:PRESS=850&quot;);
	 *    
	 *    Some data is specific to the observation, latitude/longitude for
	 *    example. These data may be retrieved using the parameter minus any level
	 *    information as follows
	 *    &quot;NLAT&quot;  station latitude
	 *    &quot;NLON&quot;  station longitude
	 * </pre>
	 * 
	 * If the sounding data defines a surface level, and a request for a level
	 * below surface is requested, a null value is returned.
	 * 
	 * @param paramName
	 *            The name of the parameter value to retrieve.
	 * @return An Amount with value and units. If the parameter is unknown, a
	 *         null reference is returned.
	 */
	@Override
	public Amount getValue(String paramName) {
		Amount a = null;

		if (parseParameter(paramName)) {
			String pName = PARM_MAP.get(parameterName);
			if ((pName != null) && (levels != null) && (levels.size() > 0)) {

				UAObsLevel obsLevel = getLevel(levelId);
				if (obsLevel != null) {
					Integer iValue = null;
					Double dValue = null;
					if (UA_GEOHGT.equals(pName)) {
						iValue = obsLevel.getGeoHeight();
						if (iValue != null) {
							a = new Amount(iValue, DISTANCE_UNIT);
						}
					} else if (SFC_TEMP.equals(pName)) {
						dValue = obsLevel.getTemp();
						if (dValue != null) {
							a = new Amount(dValue, TEMPERATURE_UNIT);
						}
					} else if (SFC_DWPT.equals(pName)) {
						dValue = obsLevel.getDwpt();
						if (dValue != null) {
							a = new Amount(dValue, TEMPERATURE_UNIT);
						}
					} else if (SFC_WNDSPD.equals(pName)) {
						dValue = obsLevel.getWindSpeed();
						if (dValue != null) {
							a = new Amount(dValue, WIND_SPEED_UNIT);
						}
					} else if (SFC_WNDDIR.equals(pName)) {
						iValue = obsLevel.getWindDirection();
						if (iValue != null) {
							a = new Amount(iValue, WIND_DIR_UNIT);
						}
					}
				}
			}
		} else {
			// Assume we are trying to get an observation attribute.
			String pName = PARM_MAP.get(paramName);
			if (STA_LAT.equals(pName)) {
				a = new Amount(this.getLatitude(), LOCATION_UNIT);
			} else if (STA_LON.equals(pName)) {
				a = new Amount(this.getLongitude(), LOCATION_UNIT);
			}
		}
		return a;
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
	public Collection<Amount> getValues(String paramName) {
		return null;
	}

	/**
	 * Determine if the parameter is a level request, and parse out the pressure
	 * level and parameter name if so.
	 * 
	 * @param parameter
	 *            The parameter string to parse.
	 * @return This is a level parameter.
	 */
	private boolean parseParameter(String parameter) {
		boolean goodParse = false;
		Pattern p = Pattern.compile(UA_PARAM_PTRN);
		Matcher m = p.matcher(parameter);
		if (m.find()) {
			int start = parameter.indexOf(":PRESS=");
			if (start > 0) {
				parameterName = parameter.substring(0, start);
				start += ":PRESS=".length();
				levelId = Integer.parseInt(parameter.substring(start));
			}
			goodParse = true;
		}
		return goodParse;
	}

	/**
	 * Get a specified pressure level data if it exists. If the specified level
	 * is below the declared surface pressure a null reference is returned.
	 * 
	 * @param level
	 *            A pressure level to get.
	 * @return The requested level, if found, null reference if not.
	 */
	private UAObsLevel getLevel(Integer level) {
		UAObsLevel retValue = null;
		if (level != null) {
			level = level * 100;
			for (UAObsLevel l : levels) {
				if (IDecoderConstants.MANPRE_LEVEL.equals(l.getVertSig())
						|| IDecoderConstants.SIGPRE_LEVEL
								.equals(l.getVertSig())) {

					if (level.equals(l.getPressure())) {
						retValue = l;
						break;
					}
				}
			}
		}
		if (retValue != null) {
			UAObsLevel sfc = getSurfaceLevel();
			if (sfc != null) {
				if (LayerTools.isLowerThan(sfc, retValue)) {
					retValue = null;
				}
			}
		}
		return retValue;
	}

	/**
	 * Get the defined surface level. If a surface level cannot be found, then
	 * return null.
	 * 
	 * @return The surface level found, or null.
	 */
	private UAObsLevel getSurfaceLevel() {
		UAObsLevel retValue = null;
		if (levels != null) {
			for (UAObsLevel level : levels) {
				if (IDecoderConstants.SFC_LEVEL.equals(level.getVertSig())) {
					retValue = level;
					break;
				}
			}
		}
		return retValue;
	}

	@Override
	public String[] getStrings(String paramName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public SurfaceObsLocation getSpatialObject() {
		return location;
	}

	public SurfaceObsLocation getLocation() {
		if (location == null) {
			location = new SurfaceObsLocation();
		}
		return location;
	}

	public void setLocation(SurfaceObsLocation location) {
		this.location = location;
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
		UAObs other = (UAObs) obj;
		if (getDataURI() == null) {
			if (other.getDataURI() != null) {
				return false;
			}
		} else if (!getDataURI().equals(other.getDataURI())) {
			return false;
		}
		return true;
	}

	/**
	 * Returns a
	 * 
	 * @param obsList
	 * @return
	 */
	public static final List<UAObs> sortByCorrection(List<UAObs> obsList) {

		// No need to sort for null, empty, or one item.
		if ((obsList != null) && (obsList.size() > 1)) {
			Collections.sort(obsList, getCorComparator());
		}
		return obsList;
	}

	public static Comparator<UAObs> getCorComparator() {
		return corComparator;
	}

	@Override
	public String toString() {
		return wmoHeader;
	}

	public static final void main(String[] args) {

		List<UAObs> obsList = new ArrayList<UAObs>();
		UAObs obsA = new UAObs();
		obsA.setWmoHeader("IUSZ42 KWBC 271845 CCA");
		obsList.add(obsA);
		UAObs obsB = new UAObs();
		obsB.setWmoHeader("IUSZ42 KWBC 271835 CCA");
		obsList.add(obsB);
		UAObs obs = new UAObs();
		obs.setWmoHeader("IUSZ42 KWBC 271815");
		obsList.add(obs);
		obs = new UAObs();
		obs.setWmoHeader("IUSZ42 KWBC 271825 CCA");
		obsList.add(obs);

		System.out.println(obsList);
		obsList = sortByCorrection(obsList);
		System.out.println(obsList);

		int c = UAObs.getCorComparator().compare(obsA, obsB);
		System.out.println(c);

		UAObs test = new UAObs(
				"/bufrua/2011-10-07_00:00:00.0/2022/null/IUSZ52_KWBC_070040/72634/44.90833/-84.71944");

		System.out.println(test.dataURI);

	}
}