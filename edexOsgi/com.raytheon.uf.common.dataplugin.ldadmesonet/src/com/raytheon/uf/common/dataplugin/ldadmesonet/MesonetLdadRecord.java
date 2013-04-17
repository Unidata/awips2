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
package com.raytheon.uf.common.dataplugin.ldadmesonet;

import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.TimeZone;

import javax.measure.quantity.Angle;
import javax.measure.quantity.Length;
import javax.measure.quantity.Pressure;
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
import com.vividsolutions.jts.geom.Geometry;

/**
 * Record implementation for ldadmesonet plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                     
 * ate          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 9/4/09                   vkorolev    Initial creation
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857  bgonzale    Added SequenceGenerator annotation.
 * </pre>
 * 
 * @author vkorolev
 * @version 1
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ldadmesonetseq")
@Table(name = "ldadmesonet", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "ldadmesonet",
		indexes = {
				@Index(name = "ldadmesonet_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MesonetLdadRecord extends PersistablePluginDataObject implements
		ISpatialEnabled, IDecoderGettable, IPointData, IPersistable {

	private static final long serialVersionUID = 1L;

	private static final String OBS_TIME_FMT = "%1$tY/%<tm/%<td %<tH:%<tM:%<tS";

	public static final String OBS_TEXT = "text";

	public static final Unit<Length> LENGTH_UNIT = SI.METER;

	public static final Unit<Temperature> TEMPERATURE_UNIT = SI.KELVIN;

	public static final Unit<Velocity> WIND_SPEED_UNIT = SI.METERS_PER_SECOND;

	public static final Unit<Angle> WIND_DIR_UNIT = NonSI.DEGREE_ANGLE;

	public static final Unit<Pressure> PRESSURE_UNIT = SI.PASCAL;

	public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

	private static final HashMap<String, String> PARM_MAP = new HashMap<String, String>();
	static {
		PARM_MAP.put("T", SFC_TEMP);
		PARM_MAP.put("DpT", SFC_DWPT);
		PARM_MAP.put("WS", SFC_WNDSPD);
		PARM_MAP.put("WD", SFC_WNDDIR);
		PARM_MAP.put("WGS", SFC_WNDGST);
		PARM_MAP.put("ASET", "SFC.PRESS.ALTIMETER");
		PARM_MAP.put("PMSL", PRES_SLP);
		PARM_MAP.put("NLAT", STA_LAT);
		PARM_MAP.put("NLON", STA_LON);
		PARM_MAP.put("STA", "STA");
		PARM_MAP.put("stationId", "STA");
		PARM_MAP.put("message", OBS_TEXT);
		PARM_MAP.put(OBS_TEXT, OBS_TEXT);
	}

	//
	@DataURI(position = 1)
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private String reportType;

	// LDAD data provider
	@DataURI(position = 2)
	@Column
	@DynamicSerializeElement
	@XmlAttribute
	private String dataProvider; // Typical data providers: CDoT, KDoT, UDFCD,

	// etc.

	// Home WFO Id for the LDAD data
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private String homeWFO;

	// Date and time of observation
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Calendar observationTime; // observationTime

	@Embedded
	@DataURI(position = 3, embedded = true)
	@XmlElement
	@DynamicSerializeElement
	private SurfaceObsLocation location; // latitude, longitude, elevation,

	// stationId "RALC2"

	// Data provider Id
	@Column
	// @DataURI(position = 3)
	@DynamicSerializeElement
	@XmlElement
	private String providerId; // * "110" "FA6026DA" Data Provider station Id

	// Alphanumeric station name
	@Column
	@DynamicSerializeElement
	@XmlElement
	private String stationName; // * "Ralston_Res" "BEN CREEK AIRSTRIP"

	// ?????????????????

	// Handbook Id (AFOS id or SHEF id)
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private String handbook5Id; // * "" ????????????????

	// LDAD mesonet station type.
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private String stationType; // * "STO" "RAWS" ????????????

	// Date and time data was processed by the data provider (e.g ALERT)
	@Column
	@DynamicSerializeElement
	@XmlElement
	private Long reportTime; // * 1.247436157E9 time data was processed by the

	// provider

	// Date and time the data was received
	@Column
	@DynamicSerializeElement
	@XmlElement
	private Double receivedTime; // * time data was received - seconds since

	// 1-1-1970

	// numeric WMO identification number
	@Column
	@DynamicSerializeElement
	@XmlElement
	private Long numericWMOid; // numeric WMO identification

	// Data platform type.
	@Column
	@DynamicSerializeElement
	@XmlElement
	private Short dataPlatformType; // short -32767 moving (e.g. floating buoy

	// or ship)

	// Data platform true direction.
	@Column
	@DynamicSerializeElement
	@XmlElement
	private Float platformTrueDirection; // data platform true direction degree

	// Data platform true speed
	@Column
	@DynamicSerializeElement
	@XmlElement
	private Float platformTrueSpeed; // data platform true speed meter/sec

	// Air temperature - time of last change (ALERT)
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Double tempChangeTime; // time of temperature last change - seconds

	// since 1970-1-1 00:00:00.0

	// Wet bulb temperature
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float wetBulbTemperature; // kelvin

	// Relative humidity - time of last change (ALERT)
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Double rhChangeTime; // time of last relative humidity change

	// Station pressure
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float stationPressure;

	// Station pressure - time of last change (ALERT)
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Double stationPressChangeTime; // time of last station press change

	// 3 Hour pressure change character
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Short pressChangeChar; // long_name =

	// "character of pressure change";

	// value0 = "press same or higher than 3 hrs ago";
	// value1 = "increasing then steady";
	// value2 = "increasing";
	// value3 = "decreasing or steady,then increasing";
	// value4 = "steady";
	// value5 = "press same or lower than 3 hrs ago";
	// value6 = "decreasing then steady";
	// value7 = "decreasing";
	// value8 = "steady or increasing,then decreasing";
	// 3 Hour pressure change value
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float pressChange3Hour; // pascal 3 hour pressure change

	// Wind direction - time of last change (ALERT)
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Double windDirChangeTime; // seconds since 1970-1-1 00:00:00.0

	// Wind speed - time of last change (ALERT)
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Double windSpeedChangeTime;

	// Wind gust - time of last change (ALERT)
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Double windGustChangeTime;

	// Wind direction, "minimum", at mininum windspeed
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float windDirMin; // degree

	// Wind direction, "maximum", at gust
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float windDirMax; // degree

	// Sky Cover Group
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private String skyCover; // char ref FMH-1

	// Altitude of the cloud bases of the cloud groups in "skyCover"
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float skyLayerBase; // sky cover layer base - meter

	// Visibility
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float visibility; // meter

	// not in cdl
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private String visibilityStatus;

	// Fraction of sky covered by clouds, fraction of sky covered by clouds
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float totalCloudCover; // tenths

	// Height of the lowest cloud layer
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Short cloudBaseHeight; // cloudBaseHeight:long_name =

	// "height of the lowest cloud layer";

	// cloudBaseHeight:value0 = "0 to 100 ft";
	// cloudBaseHeight:value1 = "200 to 300 ft";
	// cloudBaseHeight:value2 = "400 to 600 ft";
	// cloudBaseHeight:value3 = "700 to 900 ft";
	// cloudBaseHeight:value4 = "1000 to 1900 ft";
	// cloudBaseHeight:value5 = "2000 to 3200 ft";
	// cloudBaseHeight:value6 = "3300 to 4900 ft";
	// cloudBaseHeight:value7 = "5000 to 6500 ft";
	// cloudBaseHeight:value8 = "7000 to 8000 ft";
	// cloudBaseHeight:value9 = "8500 or higher or no clouds";
	// cloudBaseHeight:value-1 = "unknown or cld base below sfc of stn";
	// cloudBaseHeight:reference = "FMH-2";
	// Present Weather
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private String presWeather; // present weather ref FMH-1

	// Low level cloud type
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Short lowLevelCloudType; // lowLevelCloudType:long_name =

	// "low level cloud type";

	// lowLevelCloudType:value0 = "no Cu, Cb, Sc, or St";
	// lowLevelCloudType:value1 = "Cu w/ little vertical extent";
	// lowLevelCloudType:value2 = "Cu w/ moderate or great vertical extent";
	// lowLevelCloudType:value3 = "Cb w/out fibrous or striated upper part";
	// lowLevelCloudType:value4 = "Sc formed by spreading Cu";
	// lowLevelCloudType:value5 = "Sc not formed by spreading Cu";
	// lowLevelCloudType:value6 = "St or ragged St";
	// lowLevelCloudType:value7 = "ragged St or ragged Cu";
	// lowLevelCloudType:value8 = "Cu and Sc w/ bases at different levels";
	// lowLevelCloudType:value9 = "Cb w/ fibrous or striated upper part";
	// lowLevelCloudType:value-1 = "low clouds not visibile (obscured)";
	// lowLevelCloudType:reference = "FMH-2";

	// Middle level cloud type
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Short midLevelCloudType; // midLevelCloudType:long_name =

	// "middle level cloud type";

	// midLevelCloudType:value0 = "no Ac, As, or Ns";
	// midLevelCloudType:value1 = "semi-transparent As";
	// midLevelCloudType:value2 = "opaque As, or Ns";
	// midLevelCloudType:value3 = "semi-transparent Ac predominant";
	// midLevelCloudType:value4 = "Ac continually changing in appearance";
	// midLevelCloudType:value5 = "Ac invading the sky";
	// midLevelCloudType:value6 = "Ac formed by spreading of Cu or Cb";
	// midLevelCloudType:value7 = "double layered Ac or thick Ac; or Ac & As";
	// midLevelCloudType:value8 = "turreted Ac or Ac in tuffts";
	// midLevelCloudType:value9 = "Ac of a chaotic sky";
	// midLevelCloudType:value-1 = "middle clouds not visibile (obscured)";
	// midLevelCloudType:reference = "FMH-2";
	// High level cloud type
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Short highLevelCloudType; // highLevelCloudType:long_name =

	// "high level cloud type";

	// highLevelCloudType:value0 = "no Ci, Cs, or Cc";
	// highLevelCloudType:value1 = "Ci in filaments or hooks";
	// highLevelCloudType:value2 = "dense Ci & turreted Ci & Ci in tufts";
	// highLevelCloudType:value3 = "dense Ci orig from Cb, present";
	// highLevelCloudType:value4 = "Ci invading the sky";
	// highLevelCloudType:value5 = "Cs not exceeding 45 degrees altitude";
	// highLevelCloudType:value6 = "Cs exceeding 45 degrees altitude";
	// highLevelCloudType:value7 = "Cs covering the whole sky";
	// highLevelCloudType:value8 = "Cs not invading the sky";
	// highLevelCloudType:value9 = "Cc alone, or Cc more than (Ci & Cs)";
	// highLevelCloudType:value-1 = "high clouds not visibile (obscured)";
	// highLevelCloudType:_FillValue = -32767s;
	// highLevelCloudType:missing_value = -9999;
	// highLevelCloudType:reference = "FMH-2";
	// Maximum temperature recording period
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Short maxTempRecordPeriod; // maxTempRecordPeriod:long_name =

	// "maximum temperature recording period";

	// maxTempRecordPeriod:value0 = "1 hour";
	// maxTempRecordPeriod:value1 = "3 hours";
	// maxTempRecordPeriod:value2 = "6 hours";
	// maxTempRecordPeriod:value3 = "9 hours";
	// maxTempRecordPeriod:value4 = "12 hours";
	// maxTempRecordPeriod:value5 = "15 hours";
	// maxTempRecordPeriod:value6 = "18 hours";
	// maxTempRecordPeriod:value7 = "21 hours";
	// maxTempRecordPeriod:value8 = "24 hours";
	// maxTempRecordPeriod:value9 = "since last station report";
	// Maximum temperature
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float maximumTemperature; // kelvin

	// Minimum temperature recording period
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Short minTempRecordPeriod; // minTempRecordPeriod:long_name =

	// "minimum temperature recording period";

	// minTempRecordPeriod:value0 = "1 hour";
	// minTempRecordPeriod:value1 = "3 hours";
	// minTempRecordPeriod:value2 = "6 hours";
	// minTempRecordPeriod:value3 = "9 hours";
	// minTempRecordPeriod:value4 = "12 hours";
	// minTempRecordPeriod:value5 = "15 hours";
	// minTempRecordPeriod:value6 = "18 hours";
	// minTempRecordPeriod:value7 = "21 hours";
	// minTempRecordPeriod:value8 = "24 hours";
	// minTempRecordPeriod:value9 = "since last station report";
	// Minimum temperature
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float minimumTemperature; // kelvin

	// precip accumulation unknown time
	//
	// This is only used for CDoT. All of its accum precip is in
	// relation to precip since midnight local time. Obviously, each
	// report has a longer time period for precip coverage than the
	// previous report, so it's up to the user to figure out the
	// difference in time of the stored report versus midnight.
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float precipAccum;

	// Precipitation type
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Short precipType; // precipType:long_name = "precipitation type";

	// precipType:value0 = "no precipitation";
	// precipType:value1 ="precipitation present but unclassified";
	// precipType:value2 ="rain";
	// precipType:value3 ="snow";
	// precipType:value4 ="mixed rain and snow";
	// precipType:value29 = "RPU-to-maxSensor communications failure";
	// precipType:value30 ="sensor failure";
	// Precipitation intensity
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Short precipIntensity; // precipIntensity:long_name =

	// "precipitation intensity";

	// precipIntensity:value0 = "precipitation intensity info not available";
	// precipIntensity:value2 = "light";
	// precipIntensity:value3 = "moderate";
	// precipIntensity:value4 = "heavy";

	// Time elapsed since last precipitation
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Double timeSinceLastPcp; // seconds

	// Solar radiation
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float solarRadiation; // watt/meter2

	// Solar radiation - time of last change (ALERT)
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Double solarRadChangeTime; // seconds since 1970-1-1 00:00:00.0

	// Sea surface temperature
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float seaSurfaceTemp; // kelvin

	// Wave period
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float wavePeriod; // second

	// Wave height
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float waveHeight; // meter

	// Raw text LDAD mesonet message
	@Column
	@DynamicSerializeElement
	@XmlElement
	private String rawMessage;

	// Air Temperature in Kelvin
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float temperature; // temperature

	// Dew point temperature in degrees Kelvin.
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float dewpoint; // dewpoint

	// Relative Humidity in percent.
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float relHumidity; // relHumidity

	// Wind direction in angular degrees. Integer
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float windDir;

	// Observation wind speed in meters per second.
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float windSpeed; // float windSpeed

	// Wind gust in meters per second.
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float windGust;

	// Observation pressure in Pa.
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float pressure; // Float stationPressure

	// Sea level pressure
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float seaLevelPressure; // Float seaLevelPressure

	// Altimeter setting in Pa.
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float altimeter;

	// Precipitation rate in m/sec.
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float precipRate; // float - precipitation rate

	// not in cdl
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float fuelTemperature;

	// not in cdl
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float fuelMoisture;

	// not in cdl
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float soilTemperature;

	// not in cdl
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Float soilMoisture;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	/**
     * 
     */
	public MesonetLdadRecord() {
	}

	/**
	 * Constructor for DataURI construction through base class. This is used by
	 * the notification service.
	 * 
	 * @param uri
	 *            A data uri applicable to this class.
	 */
	public MesonetLdadRecord(String uri) {
		super(uri);
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
	@SuppressWarnings("unused")
	private Geometry getGeometry() {
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
	 * @return the location
	 */

	public SurfaceObsLocation getLocation() {
		return location;
	}

	/**
	 * @param location
	 *            the location to set
	 */
	public void setLocation(SurfaceObsLocation location) {
		this.location = location;
	}

	public Calendar getObservationTime() {
		return observationTime;
	}

	public void setObservationTime(Calendar observationTime) {
		this.observationTime = observationTime;
	}

	// ******************************************

	/**
	 * @return the temperature
	 */
	public Float getTemperature() {
		return temperature;
	}

	/**
	 * @param temperature
	 */
	public void setTemperature(Float temperature) {
		this.temperature = temperature;
	}

	/**
	 * @return the Dewpoint
	 */
	public Float getDewpoint() {
		return dewpoint;
	}

	/**
	 * @param Dewpoint
	 *            the Dewpoint to set
	 */
	public void setDewpoint(Float dewpoint) {
		this.dewpoint = dewpoint;
	}

	/**
	 * @return the humidity
	 */
	public Float getRelHumidity() {
		return relHumidity;
	}

	/**
	 * @param humidity
	 *            the humidity to set
	 */
	public void setRelHumidity(Float relHumidity) {
		this.relHumidity = relHumidity;
	}

	/**
	 * @return the windDirection
	 */
	public Float getWindDir() {
		return windDir;
	}

	/**
	 * @param windDirection
	 *            the windDirection to set
	 */
	public void setWindDir(Float windDir) {
		this.windDir = windDir;
	}

	/**
	 * @return the windSpeed
	 */
	public Float getWindSpeed() {
		return windSpeed;
	}

	/**
	 * @param windSpeed
	 *            the windSpeed to set
	 */
	public void setWindSpeed(Float windSpeed) {
		this.windSpeed = windSpeed;
	}

	/**
	 * @return the windGust
	 */
	public Float getWindGust() {
		return windGust;
	}

	/**
	 * @param windGust
	 *            the windGust to set
	 */
	public void setWindGust(Float windGust) {
		this.windGust = windGust;
	}

	/**
	 * @return the pressure
	 */
	public Float getPressure() {
		return pressure;
	}

	/**
	 * @param pressure
	 *            the pressure to set
	 */
	public void setPressure(Float pressure) {
		this.pressure = pressure;
	}

	/**
	 * @return the seaLevelPressure
	 */
	public Float getSeaLevelPressure() {
		return seaLevelPressure;
	}

	/**
	 * @param seaLevelPressure
	 *            the seaLevelPressure to set
	 */
	public void setSeaLevelPressure(Float seaLevelPressure) {
		this.seaLevelPressure = seaLevelPressure;
	}

	/**
	 * @return the altimeter
	 */
	public Float getAltimeter() {
		return altimeter;
	}

	/**
	 * @param altimeter
	 *            the altimeter to set
	 */
	public void setAltimeter(Float altimeter) {
		this.altimeter = altimeter;
	}

	/**
	 * @return the precip
	 */
	public Float getPrecipRate() {
		return precipRate;
	}

	/**
	 * @param precip
	 *            the precip to set
	 */
	public void setPrecipRate(Float precipRate) {
		this.precipRate = precipRate;
	}

	// ******************************************

	/**
     * 
     */
	public void setSpatialObject(SurfaceObsLocation loc) {
		location = loc;
	}

	/**
     * 
     */
	@Override
	public SurfaceObsLocation getSpatialObject() {
		return location;
	}

	/**
	 * This class implements IDecoderGettable so return this instance.
	 * 
	 * @return The reference to this instance.
	 */
	@Override
	public IDecoderGettable getDecoderGettable() {
		return this;
	}

	/**
     * 
     */
	@Override
	public String getString(String paramName) {
		String retValue = null;
		String pName = PARM_MAP.get(paramName);
		if ("STA".matches(pName)) {
			retValue = getStationId();
		} else if (OBS_TEXT.equals(pName)) {
			retValue = getStationId();
		}

		return retValue;
	}

	@Override
	public String[] getStrings(String paramName) {
		return null;
	}

	@Override
	public Amount getValue(String paramName) {
		Amount a = null;
		String pName = PARM_MAP.get(paramName);

		if (SFC_TEMP.equals(pName)) {
			a = new Amount(temperature, TEMPERATURE_UNIT);
		} else if (SFC_DWPT.equals(pName)) {
			a = new Amount(dewpoint, TEMPERATURE_UNIT);
		} else if (SFC_WNDSPD.equals(pName)) {
			a = new Amount(windSpeed, WIND_SPEED_UNIT);
		} else if (SFC_WNDGST.equals(pName)) {
			a = new Amount(windGust, WIND_SPEED_UNIT);
		} else if (SFC_WNDDIR.equals(pName)) {
			a = new Amount(windDir, WIND_DIR_UNIT);
		} else if (PRES_ALTSG.equals(pName)) {
			a = new Amount(altimeter, PRESSURE_UNIT);
		} else if (STA_LAT.equals(pName)) {
			a = new Amount(getLatitude(), LOCATION_UNIT);
		} else if (STA_LON.equals(pName)) {
			a = new Amount(getLongitude(), LOCATION_UNIT);
		} else if (PRES_SLP.equals(pName)) {
			a = new Amount(seaLevelPressure, PRESSURE_UNIT);
		}

		return a;
	}

	/**
     * 
     */
	@Override
	public Collection<Amount> getValues(String paramName) {
		return null;
	}

	/**
	 * @param providerId
	 *            the providerId to set
	 */
	public void setProviderId(String providerId) {
		this.providerId = providerId;
	}

	/**
	 * @return the providerId
	 */
	public String getProviderId() {
		return providerId;
	}

	/**
	 * @param stationName
	 *            the stationName to set
	 */
	public void setStationName(String stationName) {
		this.stationName = stationName;
	}

	/**
	 * @return the stationName
	 */
	public String getStationName() {
		return stationName;
	}

	/**
	 * @param handbook5Id
	 *            the handbook5Id to set
	 */
	public void setHandbook5Id(String handbook5Id) {
		this.handbook5Id = handbook5Id;
	}

	/**
	 * @return the handbook5Id
	 */
	public String getHandbook5Id() {
		return handbook5Id;
	}

	/**
	 * @param homeWFO
	 *            the homeWFO to set
	 */
	public void setHomeWFO(String homeWFO) {
		this.homeWFO = homeWFO;
	}

	/**
	 * @return the homeWFO
	 */
	public String getHomeWFO() {
		return homeWFO;
	}

	/**
	 * @param stationType
	 *            the stationType to set
	 */
	public void setStationType(String stationType) {
		this.stationType = stationType;
	}

	/**
	 * @return the stationType
	 */
	public String getStationType() {
		return stationType;
	}

	/**
	 * @param dataProvider
	 *            the dataProvider to set
	 */
	public void setDataProvider(String dataProvider) {
		this.dataProvider = dataProvider;
	}

	/**
	 * @return the dataProvider
	 */
	public String getDataProvider() {
		return dataProvider;
	}

	/**
	 * @param reportTime
	 *            the reportTime to set
	 */
	public void setReportTime(Long reportTime) {
		this.reportTime = reportTime;
	}

	/**
	 * @return the reportTime
	 */
	public Long getReportTime() {
		return reportTime;
	}

	/**
	 * @param receivedTime
	 *            the receivedTime to set
	 */
	public void setReceivedTime(Double receivedTime) {
		this.receivedTime = receivedTime;
	}

	/**
	 * @return the receivedTime
	 */
	public Double getReceivedTime() {
		return receivedTime;
	}

	/**
	 * @param numericWMOid
	 *            the numericWMOid to set
	 */
	public void setNumericWMOid(Long numericWMOid) {
		this.numericWMOid = numericWMOid;
	}

	/**
	 * @return the numericWMOid
	 */
	public Long getNumericWMOid() {
		return numericWMOid;
	}

	/**
	 * @param dataPlatformType
	 *            the dataPlatformType to set
	 */
	public void setDataPlatformType(Short dataPlatformType) {
		this.dataPlatformType = dataPlatformType;
	}

	/**
	 * @return the dataPlatformType
	 */
	public Short getDataPlatformType() {
		return dataPlatformType;
	}

	/**
	 * @param platformTrueDirection
	 *            the platformTrueDirection to set
	 */
	public void setPlatformTrueDirection(Float platformTrueDirection) {
		this.platformTrueDirection = platformTrueDirection;
	}

	/**
	 * @return the platformTrueDirection
	 */
	public Float getPlatformTrueDirection() {
		return platformTrueDirection;
	}

	/**
	 * @param platformTrueSpeed
	 *            the platformTrueSpeed to set
	 */
	public void setPlatformTrueSpeed(Float platformTrueSpeed) {
		this.platformTrueSpeed = platformTrueSpeed;
	}

	/**
	 * @return the platformTrueSpeed
	 */
	public Float getPlatformTrueSpeed() {
		return platformTrueSpeed;
	}

	/**
	 * @param tempChangeTime
	 *            the tempChangeTime to set
	 */
	public void setTempChangeTime(Double tempChangeTime) {
		this.tempChangeTime = tempChangeTime;
	}

	/**
	 * @return the tempChangeTime
	 */
	public Double getTempChangeTime() {
		return tempChangeTime;
	}

	/**
	 * @param wetBulbTemperature
	 *            the wetBulbTemperature to set
	 */
	public void setWetBulbTemperature(Float wetBulbTemperature) {
		this.wetBulbTemperature = wetBulbTemperature;
	}

	/**
	 * @return the wetBulbTemperature
	 */
	public Float getWetBulbTemperature() {
		return wetBulbTemperature;
	}

	/**
	 * @param rhChangeTime
	 *            the rhChangeTime to set
	 */
	public void setRhChangeTime(Double rhChangeTime) {
		this.rhChangeTime = rhChangeTime;
	}

	/**
	 * @return the rhChangeTime
	 */
	public Double getRhChangeTime() {
		return rhChangeTime;
	}

	/**
	 * @param stationPressChangeTime
	 *            the stationPressChangeTime to set
	 */
	public void setStationPressChangeTime(Double stationPressChangeTime) {
		this.stationPressChangeTime = stationPressChangeTime;
	}

	/**
	 * @return the stationPressChangeTime
	 */
	public Double getStationPressChangeTime() {
		return stationPressChangeTime;
	}

	/**
	 * @param pressChange3Hour
	 *            the pressChange3Hour to set
	 */
	public void setPressChange3Hour(Float pressChange3Hour) {
		this.pressChange3Hour = pressChange3Hour;
	}

	/**
	 * @return the pressChange3Hour
	 */
	public Float getPressChange3Hour() {
		return pressChange3Hour;
	}

	/**
	 * @param windDirChangeTime
	 *            the windDirChangeTime to set
	 */
	public void setWindDirChangeTime(Double windDirChangeTime) {
		this.windDirChangeTime = windDirChangeTime;
	}

	/**
	 * @return the windDirChangeTime
	 */
	public Double getWindDirChangeTime() {
		return windDirChangeTime;
	}

	/**
	 * @param windSpeedChangeTime
	 *            the windSpeedChangeTime to set
	 */
	public void setWindSpeedChangeTime(Double windSpeedChangeTime) {
		this.windSpeedChangeTime = windSpeedChangeTime;
	}

	/**
	 * @return the windSpeedChangeTime
	 */
	public Double getWindSpeedChangeTime() {
		return windSpeedChangeTime;
	}

	/**
	 * @param windGustChangeTime
	 *            the windGustChangeTime to set
	 */
	public void setWindGustChangeTime(Double windGustChangeTime) {
		this.windGustChangeTime = windGustChangeTime;
	}

	/**
	 * @return the windGustChangeTime
	 */
	public Double getWindGustChangeTime() {
		return windGustChangeTime;
	}

	/**
	 * @param windDirMin
	 *            the windDirMin to set
	 */
	public void setWindDirMin(Float windDirMin) {
		this.windDirMin = windDirMin;
	}

	/**
	 * @return the windDirMin
	 */
	public Float getWindDirMin() {
		return windDirMin;
	}

	/**
	 * @param windDirMax
	 *            the windDirMax to set
	 */
	public void setWindDirMax(Float windDirMax) {
		this.windDirMax = windDirMax;
	}

	/**
	 * @return the windDirMax
	 */
	public Float getWindDirMax() {
		return windDirMax;
	}

	/**
	 * @param skyCover
	 *            the skyCover to set
	 */
	public void setSkyCover(String skyCover) {
		this.skyCover = skyCover;
	}

	/**
	 * @return the skyCover
	 */
	public String getSkyCover() {
		return skyCover;
	}

	/**
	 * @param skyLayerBase
	 *            the skyLayerBase to set
	 */
	public void setSkyLayerBase(Float skyLayerBase) {
		this.skyLayerBase = skyLayerBase;
	}

	/**
	 * @return the skyLayerBase
	 */
	public Float getSkyLayerBase() {
		return skyLayerBase;
	}

	/**
	 * @param visibility
	 *            the visibility to set
	 */
	public void setVisibility(Float visibility) {
		this.visibility = visibility;
	}

	/**
	 * @return the visibility
	 */
	public Float getVisibility() {
		return visibility;
	}

	/**
	 * @param totalCloudCover
	 *            the totalCloudCover to set
	 */
	public void setTotalCloudCover(Float totalCloudCover) {
		this.totalCloudCover = totalCloudCover;
	}

	/**
	 * @return the totalCloudCover
	 */
	public Float getTotalCloudCover() {
		return totalCloudCover;
	}

	/**
	 * @param presWeather
	 *            the presWeather to set
	 */
	public void setPresWeather(String presWeather) {
		this.presWeather = presWeather;
	}

	/**
	 * @return the presWeather
	 */
	public String getPresWeather() {
		return presWeather;
	}

	/**
	 * @param maximumTemperature
	 *            the maximumTemperature to set
	 */
	public void setMaximumTemperature(Float maximumTemperature) {
		this.maximumTemperature = maximumTemperature;
	}

	/**
	 * @return the maximumTemperature
	 */
	public Float getMaximumTemperature() {
		return maximumTemperature;
	}

	/**
	 * @param minimumTemperature
	 *            the minimumTemperature to set
	 */
	public void setMinimumTemperature(Float minimumTemperature) {
		this.minimumTemperature = minimumTemperature;
	}

	/**
	 * @return the minimumTemperature
	 */
	public Float getMinimumTemperature() {
		return minimumTemperature;
	}

	/**
	 * @param timeSinceLastPcp
	 *            the timeSinceLastPcp to set
	 */
	public void setTimeSinceLastPcp(Double timeSinceLastPcp) {
		this.timeSinceLastPcp = timeSinceLastPcp;
	}

	/**
	 * @return the timeSinceLastPcp
	 */
	public Double getTimeSinceLastPcp() {
		return timeSinceLastPcp;
	}

	/**
	 * @param solarRadiation
	 *            the solarRadiation to set
	 */
	public void setSolarRadiation(Float solarRadiation) {
		this.solarRadiation = solarRadiation;
	}

	/**
	 * @return the solarRadiation
	 */
	public Float getSolarRadiation() {
		return solarRadiation;
	}

	/**
	 * @param solarRadChangeTime
	 *            the solarRadChangeTime to set
	 */
	public void setSolarRadChangeTime(Double solarRadChangeTime) {
		this.solarRadChangeTime = solarRadChangeTime;
	}

	/**
	 * @return the solarRadChangeTime
	 */
	public Double getSolarRadChangeTime() {
		return solarRadChangeTime;
	}

	/**
	 * @param seaSurfaceTemp
	 *            the seaSurfaceTemp to set
	 */
	public void setSeaSurfaceTemp(Float seaSurfaceTemp) {
		this.seaSurfaceTemp = seaSurfaceTemp;
	}

	/**
	 * @return the seaSurfaceTemp
	 */
	public Float getSeaSurfaceTemp() {
		return seaSurfaceTemp;
	}

	/**
	 * @param wavePeriod
	 *            the wavePeriod to set
	 */
	public void setWavePeriod(Float wavePeriod) {
		this.wavePeriod = wavePeriod;
	}

	/**
	 * @return the wavePeriod
	 */
	public Float getWavePeriod() {
		return wavePeriod;
	}

	/**
	 * @param waveHeight
	 *            the waveHeight to set
	 */
	public void setWaveHeight(Float waveHeight) {
		this.waveHeight = waveHeight;
	}

	/**
	 * @return the waveHeight
	 */
	public Float getWaveHeight() {
		return waveHeight;
	}

	/**
	 * @param pressChangeChar
	 *            the pressChangeChar to set
	 */
	public void setPressChangeChar(Short pressChangeChar) {
		this.pressChangeChar = pressChangeChar;
	}

	/**
	 * @return the pressChangeChar
	 */
	public Short getPressChangeChar() {
		return pressChangeChar;
	}

	/**
	 * @param cloudBaseHeight
	 *            the cloudBaseHeight to set
	 */
	public void setCloudBaseHeight(Short cloudBaseHeight) {
		this.cloudBaseHeight = cloudBaseHeight;
	}

	/**
	 * @return the cloudBaseHeight
	 */
	public Short getCloudBaseHeight() {
		return cloudBaseHeight;
	}

	/**
	 * @param lowLevelCloudType
	 *            the lowLevelCloudType to set
	 */
	public void setLowLevelCloudType(Short lowLevelCloudType) {
		this.lowLevelCloudType = lowLevelCloudType;
	}

	/**
	 * @return the lowLevelCloudType
	 */
	public Short getLowLevelCloudType() {
		return lowLevelCloudType;
	}

	/**
	 * @param midLevelCloudType
	 *            the midLevelCloudType to set
	 */
	public void setMidLevelCloudType(Short midLevelCloudType) {
		this.midLevelCloudType = midLevelCloudType;
	}

	/**
	 * @return the midLevelCloudType
	 */
	public Short getMidLevelCloudType() {
		return midLevelCloudType;
	}

	/**
	 * @param highLevelCloudType
	 *            the highLevelCloudType to set
	 */
	public void setHighLevelCloudType(Short highLevelCloudType) {
		this.highLevelCloudType = highLevelCloudType;
	}

	/**
	 * @return the highLevelCloudType
	 */
	public Short getHighLevelCloudType() {
		return highLevelCloudType;
	}

	/**
	 * @param maxTempRecordPeriod
	 *            the maxTempRecordPeriod to set
	 */
	public void setMaxTempRecordPeriod(Short maxTempRecordPeriod) {
		this.maxTempRecordPeriod = maxTempRecordPeriod;
	}

	/**
	 * @return the maxTempRecordPeriod
	 */
	public Short getMaxTempRecordPeriod() {
		return maxTempRecordPeriod;
	}

	/**
	 * @param minTempRecordPeriod
	 *            the minTempRecordPeriod to set
	 */
	public void setMinTempRecordPeriod(Short minTempRecordPeriod) {
		this.minTempRecordPeriod = minTempRecordPeriod;
	}

	/**
	 * @return the minTempRecordPeriod
	 */
	public Short getMinTempRecordPeriod() {
		return minTempRecordPeriod;
	}

	/**
	 * @param precipType
	 *            the precipType to set
	 */
	public void setPrecipType(Short precipType) {
		this.precipType = precipType;
	}

	/**
	 * @return the precipType
	 */
	public Short getPrecipType() {
		return precipType;
	}

	/**
	 * @param precipIntensity
	 *            the precipIntensity to set
	 */
	public void setPrecipIntensity(Short precipIntensity) {
		this.precipIntensity = precipIntensity;
	}

	/**
	 * @return the precipIntensity
	 */
	public Short getPrecipIntensity() {
		return precipIntensity;
	}

	public void setFuelTemperature(Float fuelTemperature) {
		this.fuelTemperature = fuelTemperature;
	}

	public Float getFuelTemperature() {
		return fuelTemperature;
	}

	public void setFuelMoisture(Float fuelMoisture) {
		this.fuelMoisture = fuelMoisture;
	}

	public Float getFuelMoisture() {
		return fuelMoisture;
	}

	public void setSoilTemperature(Float soilTemperature) {
		this.soilTemperature = soilTemperature;
	}

	public Float getSoilTemperature() {
		return soilTemperature;
	}

	public void setSoilMoisture(Float soilMoisture) {
		this.soilMoisture = soilMoisture;
	}

	public Float getSoilMoisture() {
		return soilMoisture;
	}

	public void setVisibilityStatus(String visibilityStatus) {
		this.visibilityStatus = visibilityStatus;
	}

	public String getVisibilityStatus() {
		return visibilityStatus;
	}

	public void setRawMessage(String rawMessage) {
		this.rawMessage = rawMessage;
	}

	public String getRawMessage() {
		return rawMessage;
	}

	public void setPrecipAccum(Float precipAccum) {
		this.precipAccum = precipAccum;
	}

	public Float getPrecipAccum() {
		return precipAccum;
	}

	public void setStationPressure(Float stationPressure) {
		this.stationPressure = stationPressure;
	}

	public Float getStationPressure() {
		return stationPressure;
	}

	/**
	 * @param stationId
	 *            the stationId to set
	 */
	public void setStationId(String stationId) {
	}

	public void delete() {
		// TODO Auto-generated method stub

	}

	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;

	}

	/**
	 * @return the pointDataView
	 */
	@Override
	public PointDataView getPointDataView() {
		return this.pointDataView;
	}

	public String toMessage() {
		StringBuilder sb = null;
		if ((getStationId() != null) && (observationTime != null)) {
			sb = new StringBuilder();

			sb.append(getStationId());
			sb.append(",");
			sb.append(String.format(OBS_TIME_FMT, observationTime));
			sb.append(",");
			sb.append((temperature != null) ? temperature : "");
			sb.append(",");
			sb.append((dewpoint != null) ? dewpoint : "");
			sb.append(",");
			sb.append((relHumidity != null) ? relHumidity : "");
			sb.append(",");
			sb.append((wetBulbTemperature != null) ? wetBulbTemperature : "");
			// ---------------------------------------------------------------
			sb.append(",");
			sb.append((windDir != null) ? windDir : "");

			sb.append(",");
			sb.append((windSpeed != null) ? windSpeed : "");

			sb.append(",");
			sb.append((windGust != null) ? windGust : "");
			// ---------------------------------------------------------------
			sb.append(",");
			sb.append((pressure != null) ? pressure : "");

			sb.append(",");
			sb.append((stationPressure != null) ? stationPressure : "");

			sb.append(",");
			sb.append((altimeter != null) ? altimeter : "");

			sb.append(",");
			sb.append((seaLevelPressure != null) ? seaLevelPressure : "");

			sb.append(",");
			sb.append((pressChangeChar != null) ? pressChangeChar : "");

			sb.append(",");
			sb.append((pressChange3Hour != null) ? pressChange3Hour : "");
			// ---------------------------------------------------------------
			sb.append(",");
			sb.append((precipAccum != null) ? precipAccum : "");

			sb.append(",");
			sb.append((precipIntensity != null) ? precipIntensity : "");

			sb.append(",");
			sb.append((precipType != null) ? precipType : "");
			// ---------------------------------------------------------------
			sb.append(",");
			sb.append((totalCloudCover != null) ? totalCloudCover : "");

			sb.append(",");
			sb.append((skyCover != null) ? skyCover : "");

			sb.append(",");
			sb.append((skyLayerBase != null) ? skyLayerBase : "");

			sb.append(",");
			sb.append((cloudBaseHeight != null) ? cloudBaseHeight : "");

			sb.append(",");
			sb.append((visibility != null) ? visibility : "");

			sb.append(",");
			sb.append((visibilityStatus != null) ? visibilityStatus : "");

			sb.append(",");
			sb.append((presWeather != null) ? presWeather : "");
		}
		return (sb == null) ? null : sb.toString();
	}

	public static final void main(String[] args) {

		Calendar c = Calendar.getInstance();
		c.setTimeZone(TimeZone.getTimeZone("Z"));

		MesonetLdadRecord rec = new MesonetLdadRecord();
		rec.setObservationTime(c);
		rec.setProviderId("ABCDE");
		rec.setTemperature(303.2f);
		rec.setDewpoint(289.6f);
		rec.setWindDir(255f);
		rec.setWindSpeed(15f);
		rec.setWindGust(27f);
		rec.setPressure(99820f);
		rec.setAltimeter(29.85f);
		rec.setSeaLevelPressure(100215f);

		System.out.println(rec.toMessage());

	}

}
