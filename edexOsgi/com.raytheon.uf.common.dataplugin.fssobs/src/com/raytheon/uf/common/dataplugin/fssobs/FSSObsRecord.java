package com.raytheon.uf.common.dataplugin.fssobs;

import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;

import javax.measure.quantity.Angle;
import javax.measure.quantity.DataAmount;
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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "fssobsseq")
@Table(name = "fssobs", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "fssobs",
		indexes = {
				@Index(name = "fssobs_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class FSSObsRecord extends PersistablePluginDataObject implements
		ISpatialEnabled, IDecoderGettable, IPersistable, IPointData {

	private static final long serialVersionUID = 1L;

	public static final String PLUGIN_NAME = "fssobs";

	private static final int MISSING = -9999;

	// UNITS

	public static final Unit<Temperature> TEMPERATURE_UNIT = SI.CELSIUS;

	public static final Unit<Velocity> WIND_SPEED_UNIT = NonSI.KNOT;

	public static final Unit<Length> HEIGHT_UNIT = SI.METER;

	public static final Unit<Angle> WIND_DIR_UNIT = NonSI.DEGREE_ANGLE;

	public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

	public static final Unit<Pressure> PRESSURE_UNIT = SI.HECTO(SI.PASCAL);

	public static final Unit<Length> PRECIP_UNIT = NonSI.INCH;

	public static final Unit<Length> WAVE_UNIT = SI.METER;

	public static final Unit<Length> VISIBILITY_UNIT = NonSI.MILE;

	public static final Unit<DataAmount> CLOUD_COVER = NonSI.OCTET;

	/** Metar specific parameter keys */
	public static final class ParameterKey {
		public static final String SFC_ALTIMETER = "SFC.PRESS.ALTIMETER";

		public static final String PRESSURE_CHANGE = "PCHNG";

		public static final String VISIBILITY = "VIS";

		public static final String PRECIPITATION_1HR = "PR1HR";
	}

	private static final HashMap<String, String> PARM_MAP = new HashMap<String, String>();
	static {
		PARM_MAP.put("NLAT", STA_LAT);
		PARM_MAP.put("NLON", STA_LON);
		PARM_MAP.put("rawMessage", "rawMessage");
	}

	/** is feature new **/
	@Transient
	@DynamicSerializeElement
	@XmlElement
	public boolean isNew = true;

	// Current CWA (WFO)
	@Column
	@DataURI(position = 2)
	@DynamicSerializeElement
	@XmlElement(nillable = false)
	private String cwa;

	// Monitor which should use this station record
	// fog = "fog"
	// safeseas = "ss"
	// snow = "snow"
	@Column
	@DataURI(position = 4)
	@DynamicSerializeElement
	@XmlElement
	private String monitorUse = "";

	// Station name
	@Column
	@DynamicSerializeElement
	@XmlElement
	private String stnName;

	/* From ============ObReport================= */

	@XmlElement
	@DynamicSerializeElement
	@Column
	@DataURI(position = 1)
	protected String reportType;

	@Embedded
	@DataURI(position = 3, embedded = true)
	@XmlElement
	@DynamicSerializeElement
	private SurfaceObsLocation location;

	// Observing platform identifier (same as stationID)
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private String platformId;

	// Indicator of whether observing platform is stationary
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private boolean isStationary;

	// Actual time of the observation
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private Calendar timeObs;

	// Time of the observation to the nearest hour.
	@XmlElement
	@DynamicSerializeElement
	@Column
	private Calendar refHour;

	// Raw message
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private String rawMessage;

	// Observed wind speed in knots
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float windSpeed = -9999;;

	// Wind gust in knots
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float windGust = -9999;;

	// Observed maximum wind speed in knots
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float maxWindSpeed = -9999;

	// Observed wind direction in azimuth degrees
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float windDir;

	// Observed wind chill in Fahrenheit
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float windChill = -9999;

	// Observed high resolution wave height in
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float highResWaveHeight = -9999;

	// Observed wave steepness in seconds ??? None
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float waveSteepness = -9999;

	// Observed visibility in Statute miles
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float visibility = -9999;

	// Observed visibility in meters for Maritime obs.
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float horzVisibility = -9999;

	// Observed temperature in degrees in Farenheit
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float temperature = -9999;

	// in feet
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private Double waveHeight = -9999.0;

	// in seconds
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private Integer wavePeriod = -9999;

	// in Azimuth degrees
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private Double primarySwellWaveDir = -9999.0;

	// in seconds
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private Integer primarySwellWavePeriod = -9999;

	// in feet
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private Double primarySwellWaveHeight = -9999.0;

	// in Azimuth degrees
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private Double secondarySwellWaveDir = -9999.0;

	// in seconds
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private Integer secondarySwellWavePeriod = -9999;

	// in feet
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private Double secondarySwellWaveHeight = -9999.0;

	// // Pressure in inches of mercury
	// @Transient
	// @DynamicSerializeElement
	// @XmlElement
	// private float pressure = -9999;

	// Three-hour pressure change in thousandths of an inch of mercury ????
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float pressChange3Hour = -9999f;

	// Pressure change character for metar plot model
	/** A string denoting the pressure tendency(rising or falling) */
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private String pressChangeChar;

	// Observed dewpoint in degrees Farenheit
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float dewpoint = -9999f;

	// Observed sea surface temperature in degrees in Farenheit
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float seaSurfaceTemp = -9999f;

	// the sea level pressure in hPa
	@XmlElement
	@DynamicSerializeElement
	@Transient
	private float seaLevelPress = -9999f;

	// Altimeter setting in mm Hg.
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private float pressureAltimeter = -9999f;

	// Observed hourly precipitation in inches
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float hourlyPrecip = -9999f;

	// Observed snow depth in inch
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float snowDepth = -9999f;

	// Observed snow increasing rapidly, hourly total in inches
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float snincrHourly = -9999f;

	// Observed snow increasing rapidly, total in inches
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float snincrTotal = -9999f;

	// Observed frostbite time in minutes
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float frostbiteTime;

	// present weather conditions for metar plot model
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private String[] presWeather;

	// Observed relative humidity in percent
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float relativeHumidity = -9999f;

	// Observed ceiling in feet above ground level
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float ceiling = -9999f;

	// Observed dewpoint depression in Farenheit
	@Transient
	@DynamicSerializeElement
	@XmlElement
	private float dewpointDepr = -9999f;

	@XmlElement
	@DynamicSerializeElement
	@Transient
	private String[] skyCover;

	@XmlElement
	@DynamicSerializeElement
	@Transient
	private int totCloudAmount = -9999;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	// ----------------------------------------------------------------------------------------------------
	public FSSObsRecord() {
	}

	public FSSObsRecord(String uri) {
		super(uri);
	}

	@Override
	public IDecoderGettable getDecoderGettable() {
		return null;
	}

	@Override
	public Amount getValue(String paramName) {
		Amount a = null;

		String pName = PARM_MAP.get(paramName);

		if (SFC_TEMP.equals(pName) && (temperature != -9999f)) {
			a = new Amount(temperature, TEMPERATURE_UNIT);
		} else if (SFC_DWPT.equals(pName) && (dewpoint != -9999f)) {
			a = new Amount(dewpoint, TEMPERATURE_UNIT);
		} else if (SFC_WNDSPD.equals(pName) && (windSpeed != -9999f)) {
			a = new Amount(windSpeed, WIND_SPEED_UNIT);
		} else if (SFC_WNDDIR.equals(pName) && (windDir != -9999f)) {
			a = new Amount(windDir, WIND_DIR_UNIT);
		} else if (SFC_WNDGST.equals(pName) && (windGust != -9999f)) {
			a = new Amount(windGust, WIND_SPEED_UNIT);
		} else if (PRES_SLP.equals(pName) && (seaLevelPress != -9999f)) {
			a = new Amount(seaLevelPress, PRESSURE_UNIT);
			// } else if (PRES_ALTSG.equals(pName) && (pressureAltimeter !=
			// -9999f)) {
			// a = new Amount(pressureAltimeter, PRESSURE_UNIT);
		} else if (STA_LAT.equals(pName)) {
			a = new Amount(this.getLatitude(), LOCATION_UNIT);
		} else if (STA_LON.equals(pName)) {
			a = new Amount(this.getLongitude(), LOCATION_UNIT);
		} else if ("WT".equals(pName) && this.seaSurfaceTemp != -9999f) {
			a = new Amount(this.seaSurfaceTemp, TEMPERATURE_UNIT);
		} else if ("WH".equals(pName)) {
			a = new Amount(waveHeight, WAVE_UNIT);
		} else if ("SWP".equals(pName)) {
			a = new Amount(primarySwellWavePeriod, WAVE_UNIT);
		} else if ("SWH".equals(pName)) {
			a = new Amount(primarySwellWaveHeight, WAVE_UNIT);
			// } else if ("PCHNG".equals(pName) && pressChange3Hour != MISSING)
			// {
			// a = new Amount(pressChange3Hour, PRESSURE_UNIT);
		} else if ("PKWND".equals(paramName) && maxWindSpeed != MISSING) {
			a = new Amount(maxWindSpeed, WIND_SPEED_UNIT);
		} else if ("SWS".equals(paramName) || "SWGS".equals(paramName)) {
			a = new Amount(1, WIND_SPEED_UNIT);
		} else if ("SWD".equals(paramName) && primarySwellWaveDir != MISSING) {
			a = new Amount(primarySwellWaveDir, WIND_DIR_UNIT);
		}

		return a;
	}

	/**
	 * @return the isNew
	 */
	public boolean getIsNew() {
		return isNew;
	}

	/**
	 * @return the cwa
	 */
	public String getCwa() {
		return cwa;
	}

	/**
	 * @param monitorUse
	 *            the monitorUse to set
	 */
	public void setMonitorUse(String monitorUse) {
		this.monitorUse = monitorUse;
	}

	/**
	 * @return the monitorUse
	 */
	public String getMonitorUse() {
		return monitorUse;
	}

	/**
	 * @return the stnName
	 */
	public String getStnName() {
		return stnName;
	}

	/**
	 * @return the reportType
	 */
	public String getReportType() {
		return reportType;
	}

	/**
	 * @return the location
	 */
	public SurfaceObsLocation getLocation() {
		return location;
	}

	/**
	 * @param platformId
	 *            the platformId to set
	 */
	public void setPlatformId(String platformId) {
		this.platformId = platformId;
	}

	/**
	 * @return the platformId
	 */
	public String getPlatformId() {
		return platformId;
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
	 * @return the isStationary
	 */
	public boolean isStationary() {
		return isStationary;
	}

	/**
	 * @return the refHour
	 */
	public Calendar getRefHour() {
		return refHour;
	}

	/**
	 * @return the rawMessage
	 */
	public String getRawMessage() {
		return rawMessage;
	}

	/**
	 * @return the windSpeed
	 */
	public float getWindSpeed() {
		return windSpeed;
	}

	/**
	 * @return the windGust
	 */
	public float getWindGust() {
		return windGust;
	}

	/**
	 * @return the maxWindSpeed
	 */
	public float getMaxWindSpeed() {
		return maxWindSpeed;
	}

	/**
	 * @return the windDir
	 */
	public float getWindDir() {
		return windDir;
	}

	/**
	 * @return the windChill
	 */
	public float getWindChill() {
		return windChill;
	}

	// /**
	// * @return the highResWaveHeight
	// */
	// public float getHighResWaveHeight() {
	// return highResWaveHeight;
	// }

	/**
	 * @return the waveSteepness
	 */
	public float getWaveSteepness() {
		return waveSteepness;
	}

	/**
	 * @return the visibility
	 */
	public float getVisibility() {
		return visibility;
	}

	/**
	 * @return the temperature
	 */
	public float getTemperature() {
		return temperature;
	}

	/**
	 * @return the waveHeight
	 */
	public Double getWaveHeight() {
		return waveHeight;
	}

	/**
	 * @return the wavePeriod
	 */
	public Integer getWavePeriod() {
		return wavePeriod;
	}

	/**
	 * @return the primarySwellWaveDir
	 */
	public Double getPrimarySwellWaveDir() {
		return primarySwellWaveDir;
	}

	/**
	 * @return the primarySwellWavePeriod
	 */
	public Integer getPrimarySwellWavePeriod() {
		return primarySwellWavePeriod;
	}

	/**
	 * @return the primarySwellWaveHeight
	 */
	public Double getPrimarySwellWaveHeight() {
		return primarySwellWaveHeight;
	}

	/**
	 * @return the secondarySwellWaveDir
	 */
	public Double getSecondarySwellWaveDir() {
		return secondarySwellWaveDir;
	}

	/**
	 * @return the secondarySwellWavePeriod
	 */
	public Integer getSecondarySwellWavePeriod() {
		return secondarySwellWavePeriod;
	}

	/**
	 * @return the secondarySwellWaveHeight
	 */
	public Double getSecondarySwellWaveHeight() {
		return secondarySwellWaveHeight;
	}

	// /**
	// * @return the pressure
	// */
	// public float getPressure() {
	// return pressure;
	// }
	//
	//
	// /**
	// * @return the pressChangeChar
	// */
	// public String getPressChangeChar() {
	// return pressChangeChar;
	// }

	/**
	 * @return the dewpoint
	 */
	public float getDewpoint() {
		return dewpoint;
	}

	/**
	 * @return the seaSurfaceTemp
	 */
	public float getSeaSurfaceTemp() {
		return seaSurfaceTemp;
	}

	/**
	 * @return the seaLevelPress
	 */
	public float getSeaLevelPress() {
		return seaLevelPress;
	}

	/**
	 * @param pressureAltimeter
	 *            the pressureAltimeter to set
	 */
	public void setPressureAltimeter(float pressureAltimeter) {
		this.pressureAltimeter = pressureAltimeter;
	}

	/**
	 * @return the pressureAltimeter
	 */
	public float getPressureAltimeter() {
		return pressureAltimeter;
	}

	/**
	 * @param pressChange3Hour
	 *            the pressChange3Hour to set
	 */
	public void setPressChange3Hour(float pressChange3Hour) {
		this.pressChange3Hour = pressChange3Hour;
	}

	/**
	 * @return the pressChange3Hour
	 */
	public float getPressChange3Hour() {
		return pressChange3Hour;
	}

	/**
	 * @return the snowDepth
	 */
	public float getSnowDepth() {
		return snowDepth;
	}

	/**
	 * @return the snincrHourly
	 */
	public float getSnincrHourly() {
		return snincrHourly;
	}

	/**
	 * @return the snincrTotal
	 */
	public float getSnincrTotal() {
		return snincrTotal;
	}

	/**
	 * @return the frostbiteTime
	 */
	public float getFrostbiteTime() {
		return frostbiteTime;
	}

	/**
	 * @return the relativeHumidity
	 */
	public float getRelativeHumidity() {
		return relativeHumidity;
	}

	/**
	 * @param ceiling
	 *            the ceiling to set
	 */
	public void setCeiling(float ceiling) {
		this.ceiling = ceiling;
	}

	/**
	 * @return the ceiling
	 */
	public float getCeiling() {
		return ceiling;
	}

	/**
	 * @return the dewpointDepr
	 */
	public float getDewpointDepr() {
		return dewpointDepr;
	}

	/**
	 * @param isNew
	 *            the isNew to set
	 */
	public void setIsNew(boolean isNew) {
		this.isNew = isNew;
	}

	/**
	 * @param cwa
	 *            the cwa to set
	 */
	public void setCwa(String cwa) {
		this.cwa = cwa;
	}

	/**
	 * @param stnName
	 *            the stnName to set
	 */
	public void setStnName(String stnName) {
		this.stnName = stnName;
	}

	/**
	 * @param reportType
	 *            the reportType to set
	 */
	public void setReportType(String reportType) {
		this.reportType = reportType;
	}

	/**
	 * @param location
	 *            the location to set
	 */
	public void setLocation(SurfaceObsLocation location) {
		this.location = location;
	}

	/**
	 * @param isStationary
	 *            the isStationary to set
	 */
	public void setIsStationary(boolean isStationary) {
		this.isStationary = isStationary;
	}

	public boolean getIsStationary() {
		return isStationary;
	}

	/**
	 * @param timeObs
	 *            the timeObs to set
	 */
	public void setTimeObs(Calendar timeObs) {
		this.timeObs = timeObs;
	}

	/**
	 * @return the timeObs
	 */
	public Calendar getTimeObs() {
		return timeObs;
	}

	/**
	 * @param refHour
	 *            the refHour to set
	 */
	public void setRefHour(Calendar refHour) {
		this.refHour = refHour;
	}

	/**
	 * @param rawMessage
	 *            the rawMessage to set
	 */
	public void setRawMessage(String rawMessage) {
		this.rawMessage = rawMessage;
	}

	/**
	 * @param windSpeed
	 *            the windSpeed to set
	 */
	public void setWindSpeed(float windSpeed) {
		this.windSpeed = windSpeed;
	}

	/**
	 * @param windGust
	 *            the windGust to set
	 */
	public void setWindGust(float windGust) {
		this.windGust = windGust;
	}

	/**
	 * @param maxWindSpeed
	 *            the maxWindSpeed to set
	 */
	public void setMaxWindSpeed(float maxWindSpeed) {
		this.maxWindSpeed = maxWindSpeed;
	}

	/**
	 * @param windDir
	 *            the windDir to set
	 */
	public void setWindDir(float windDir) {
		this.windDir = windDir;
	}

	/**
	 * @param windChill
	 *            the windChill to set
	 */
	public void setWindChill(float windChill) {
		this.windChill = windChill;
	}

	public void setHighResWaveHeight(float highResWaveHeight) {
		this.highResWaveHeight = highResWaveHeight;
	}

	public float getHighResWaveHeight() {
		return highResWaveHeight;
	}

	/**
	 * @param waveSteepness
	 *            the waveSteepness to set
	 */
	public void setWaveSteepness(float waveSteepness) {
		this.waveSteepness = waveSteepness;
	}

	/**
	 * @param visibility
	 *            the visibility to set
	 */
	public void setVisibility(float visibility) {
		this.visibility = visibility;
	}

	/**
	 * @param horzVisibility
	 *            the horzVisibility to set
	 */
	public void setHorzVisibility(float horzVisibility) {
		this.horzVisibility = horzVisibility;
	}

	/**
	 * @return the horzVisibility
	 */
	public float getHorzVisibility() {
		return horzVisibility;
	}

	/**
	 * @param temperature
	 *            the temperature to set
	 */
	public void setTemperature(float temperature) {
		this.temperature = temperature;
	}

	/**
	 * @param waveHeight
	 *            the waveHeight to set
	 */
	public void setWaveHeight(Double waveHeight) {
		this.waveHeight = waveHeight;
	}

	/**
	 * @param wavePeriod
	 *            the wavePeriod to set
	 */
	public void setWavePeriod(Integer wavePeriod) {
		this.wavePeriod = wavePeriod;
	}

	/**
	 * @param primarySwellWaveDir
	 *            the primarySwellWaveDir to set
	 */
	public void setPrimarySwellWaveDir(Double primarySwellWaveDir) {
		this.primarySwellWaveDir = primarySwellWaveDir;
	}

	/**
	 * @param primarySwellWavePeriod
	 *            the primarySwellWavePeriod to set
	 */
	public void setPrimarySwellWavePeriod(Integer primarySwellWavePeriod) {
		this.primarySwellWavePeriod = primarySwellWavePeriod;
	}

	/**
	 * @param primarySwellWaveHeight
	 *            the primarySwellWaveHeight to set
	 */
	public void setPrimarySwellWaveHeight(Double primarySwellWaveHeight) {
		this.primarySwellWaveHeight = primarySwellWaveHeight;
	}

	/**
	 * @param secondarySwellWaveDir
	 *            the secondarySwellWaveDir to set
	 */
	public void setSecondarySwellWaveDir(Double secondarySwellWaveDir) {
		this.secondarySwellWaveDir = secondarySwellWaveDir;
	}

	/**
	 * @param secondarySwellWavePeriod
	 *            the secondarySwellWavePeriod to set
	 */
	public void setSecondarySwellWavePeriod(Integer secondarySwellWavePeriod) {
		this.secondarySwellWavePeriod = secondarySwellWavePeriod;
	}

	/**
	 * @param secondarySwellWaveHeight
	 *            the secondarySwellWaveHeight to set
	 */
	public void setSecondarySwellWaveHeight(Double secondarySwellWaveHeight) {
		this.secondarySwellWaveHeight = secondarySwellWaveHeight;
	}

	/**
	 * @param pressChangeChar
	 *            the pressChangeChar to set
	 */
	public void setPressChangeChar(String pressChangeChar) {
		this.pressChangeChar = pressChangeChar;
	}

	/**
	 * @return the pressChangeChar
	 */
	public String getPressChangeChar() {
		return pressChangeChar;
	}

	/**
	 * @param dewpoint
	 *            the dewpoint to set
	 */
	public void setDewpoint(float dewpoint) {
		this.dewpoint = dewpoint;
	}

	/**
	 * @param seaSurfaceTemp
	 *            the seaSurfaceTemp to set
	 */
	public void setSeaSurfaceTemp(float seaSurfaceTemp) {
		this.seaSurfaceTemp = seaSurfaceTemp;
	}

	/**
	 * @param seaLevelPress
	 *            the seaLevelPress to set
	 */
	public void setSeaLevelPress(float seaLevelPress) {
		this.seaLevelPress = seaLevelPress;
	}

	/**
	 * @param hourlyPrecip
	 *            the hourlyPrecip to set
	 */
	public void setHourlyPrecip(float hourlyPrecip) {
		this.hourlyPrecip = hourlyPrecip;
	}

	/**
	 * @return the hourlyPrecip
	 */
	public float getHourlyPrecip() {
		return hourlyPrecip;
	}

	/**
	 * @param snowDepth
	 *            the snowDepth to set
	 */
	public void setSnowDepth(float snowDepth) {
		this.snowDepth = snowDepth;
	}

	/**
	 * @param snincrHourly
	 *            the snincrHourly to set
	 */
	public void setSnincrHourly(float snincrHourly) {
		this.snincrHourly = snincrHourly;
	}

	/**
	 * @param snincrTotal
	 *            the snincrTotal to set
	 */
	public void setSnincrTotal(float snincrTotal) {
		this.snincrTotal = snincrTotal;
	}

	/**
	 * @param frostbiteTime
	 *            the frostbiteTime to set
	 */
	public void setFrostbiteTime(float frostbiteTime) {
		this.frostbiteTime = frostbiteTime;
	}

	/**
	 * @param relativeHumidity
	 *            the relativeHumidity to set
	 */
	public void setRelativeHumidity(float relativeHumidity) {
		this.relativeHumidity = relativeHumidity;
	}

	/**
	 * @param presWeather
	 *            the presWeather to set
	 */
	public void setPresWeather(String[] presWeather) {
		this.presWeather = presWeather;
	}

	/**
	 * @return the presWeather
	 */
	public String[] getPresWeather() {
		return presWeather;
	}

	/**
	 * @param dewpointDepr
	 *            the dewpointDepr to set
	 */
	public void setDewpointDepr(float dewpointDepr) {
		this.dewpointDepr = dewpointDepr;
	}

	/**
	 * @return the skyCover
	 */
	public String[] getSkyCover() {
		return skyCover;
	}

	/**
	 * @param skyCover
	 *            the skyCover to set
	 */
	public void setSkyCover(String[] skyCover) {
		this.skyCover = skyCover;
	}

	/**
	 * @param totCloudAmount
	 *            the totCloudAmount to set
	 */
	public void setTotCloudAmount(int totCloudAmount) {
		this.totCloudAmount = totCloudAmount;
	}

	/**
	 * @return the totCloudAmount
	 */
	public int getTotCloudAmount() {
		return totCloudAmount;
	}

	@Override
	public Collection<Amount> getValues(String paramName) {
		return null;
	}

	@Override
	public String getString(String paramName) {
		return null;
	}

	@Override
	public String[] getStrings(String paramName) {
		return null;
	}

	@Override
	public ISpatialObject getSpatialObject() {
		return location;
	}

	/**
	 * @param pointDataView
	 *            the pointDataView to set
	 */
	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;
	}

	/**
	 * @return the pointDataView
	 */
	@Override
	public PointDataView getPointDataView() {
		return pointDataView;
	}

	/**
	 * Used for debugging.
	 */
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("fssObsRec:\n\t");
		if (this != null) {
			sb.append(this.getDataURI() + "\n\t");
			sb.append(this.getLocation().getStationId() + " ===> "
					+ this.getStnName() + "\n\t");
			sb.append("Latitude = " + this.getLocation().getLatitude() + "\n\t");
			sb.append("Longitude = " + this.getLocation().getLongitude()
					+ "\n\t");
			sb.append(this.getReportType() + "\n\t");
			sb.append("Visibility = " + this.getVisibility() + "\n\t");
			sb.append("Temperature = " + this.getTemperature() + "\n\t");
			sb.append(this.getDataTime().getRefTime() + "\n");
		}
		return sb.toString();
	}

}
