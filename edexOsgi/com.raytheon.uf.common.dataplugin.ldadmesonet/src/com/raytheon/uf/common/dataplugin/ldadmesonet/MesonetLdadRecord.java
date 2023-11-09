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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.locationtech.jts.geom.Geometry;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.annotations.NullString;
import com.raytheon.uf.common.dataplugin.ldad.LdadRecord;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record implementation for ldadmesonet plugin.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 04, 2009           vkorolev  Initial creation
 * Apr 04, 2013  1846     bkowal    Added an index on refTime and forecastTime
 * Apr 12, 2013  1857     bgonzale  Added SequenceGenerator annotation.
 * May 07, 2013  1869     bsteffen  Remove dataURI column from PluginDataObject.
 * May 15, 2013  1869     bsteffen  Remove DataURI column from ldadmesonet.
 * Aug 30, 2013  2298     rjpeter   Make getPluginName abstract
 * Jul 20, 2015  4360     rferrel   Named unique constraint Made reportType and
 *                                  dataProvider not nullable
 * Mar 06, 2018  6851     randerso  Changed to extend LdadRecord. Code cleanup.
 * May 10, 2018  7288     randerso  Removed unused and unimplemented
 *                                  setStationId() method
 * Aug 08, 2022  8892     tjensen   Update indexes for Hibernate 5
 *
 * </pre>
 *
 * @author vkorolev
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ldadmesonetseq")
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@Table(name = "ldadmesonet", uniqueConstraints = {
        @UniqueConstraint(name = "uk_ldadmesonet_datauri_fields", columnNames = {
                "stationid", "reftime", "reporttype", "dataprovider",
                "latitude", "longitude" }) }, indexes = {
                        @Index(name = "%TABLE%_refTimeIndex", columnList = "refTime, forecastTime"),
                        @Index(name = "%TABLE%_stationIndex", columnList = "stationId") })

@DynamicSerialize
public class MesonetLdadRecord extends LdadRecord
        implements ISpatialEnabled, IPointData, IPersistable {

    private static final long serialVersionUID = 1L;

    private static final String OBS_TIME_FMT = "%1$tY/%<tm/%<td %<tH:%<tM:%<tS";

    // TODO: move common fields from MesonetLdadRecord and HydroLdadRecord up to
    // LdadRecord. Unfortunately this changes the dataURI so would require
    // additional changes.

    // TODO: this is a worthless field (always contains "mesonet") and should be
    // removed when dataURI is changed for the above TODO
    @DataURI(position = 1)
    @NullString
    @Column(nullable = false)
    @DynamicSerializeElement
    private String reportType = "mesonet";

    // LDAD data provider
    // Typical data providers: CDoT, KDoT, UDFCD, etc.
    @DataURI(position = 2)
    @NullString
    @Column(nullable = false)
    @DynamicSerializeElement
    private String dataProvider;

    // Home WFO Id for the LDAD data
    @Column
    @DynamicSerializeElement
    private String homeWFO;

    // Date and time of observation
    @Column
    @DynamicSerializeElement
    private Date observationTime;

    // latitude, longitude, elevation, stationId "RALC2"
    @Embedded
    @DataURI(position = 3, embedded = true)
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    // Data provider Id
    @Column
    @DynamicSerializeElement
    private String providerId;

    // Alphanumeric station name
    // * "Ralston_Res" "BEN CREEK AIRSTRIP"
    @Column
    @DynamicSerializeElement
    private String stationName;

    // Handbook Id (AFOS id or SHEF id)
    @Transient
    @DynamicSerializeElement
    private String handbook5Id;

    // LDAD mesonet station type.
    @Transient
    @DynamicSerializeElement
    private String stationType;

    // Date and time data was processed by the data provider (e.g ALERT)
    // seconds since epoch
    @Column
    @DynamicSerializeElement
    private Long reportTime;

    // Date and time the data was received
    // seconds since epoch
    @Column
    @DynamicSerializeElement
    private Double receivedTime;

    // numeric WMO identification number
    @Column
    @DynamicSerializeElement
    private Long numericWMOid;

    // Data platform type.
    // short -32767 moving (e.g. floating buoy or ship)
    @Column
    @DynamicSerializeElement
    private Short dataPlatformType;

    // Data platform true direction degree.
    @Column
    @DynamicSerializeElement
    private Float platformTrueDirection;

    // Data platform true speed meter/sec
    @Column
    @DynamicSerializeElement
    private Float platformTrueSpeed;

    // Air temperature - time of last change (ALERT) - seconds
    @Transient
    @DynamicSerializeElement
    private Double tempChangeTime;

    // since 1970-1-1 00:00:00.0

    // Wet bulb temperature kelvin
    @Transient
    @DynamicSerializeElement
    private Float wetBulbTemperature;

    // Relative humidity - time of last change (ALERT)
    @Transient
    @DynamicSerializeElement
    private Double rhChangeTime;

    // Station pressure
    @Transient
    @DynamicSerializeElement
    private Float stationPressure;

    // Station pressure - time of last change (ALERT)
    @Transient
    @DynamicSerializeElement
    private Double stationPressChangeTime;

    // 3 Hour pressure change character
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
    private Short pressChangeChar;

    // pascal 3 hour pressure change
    @Transient
    @DynamicSerializeElement
    private Float pressChange3Hour;

    // Wind direction - time of last change (ALERT)
    // seconds since 1970-1-1 00:00:00.0
    @Transient
    @DynamicSerializeElement
    private Double windDirChangeTime;

    // Wind speed - time of last change (ALERT)
    @Transient
    @DynamicSerializeElement
    private Double windSpeedChangeTime;

    // Wind gust - time of last change (ALERT)
    @Transient
    @DynamicSerializeElement
    private Double windGustChangeTime;

    // Wind direction, "minimum", at mininum windspeed degree
    @Transient
    @DynamicSerializeElement
    private Float windDirMin;

    // Wind direction, "maximum", at gust degree
    @Transient
    @DynamicSerializeElement
    private Float windDirMax;

    // Sky Cover Group
    // char ref FMH-1
    @Transient
    @DynamicSerializeElement
    private String skyCover;

    // Altitude of the cloud bases of the cloud groups in "skyCover"
    // sky cover layer base - meter
    @Transient
    @DynamicSerializeElement
    private Float skyLayerBase;

    // Visibility meter
    @Transient
    @DynamicSerializeElement
    private Float visibility;

    // not in cdl
    @Transient
    @DynamicSerializeElement
    private String visibilityStatus;

    // Fraction of sky covered by clouds, fraction of sky covered by clouds
    // tenths
    @Transient
    @DynamicSerializeElement
    private Float totalCloudCover;

    // Height of the lowest cloud layer
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
    @Transient
    @DynamicSerializeElement
    private Short cloudBaseHeight;

    // Present Weather ref FMH-1
    @Transient
    @DynamicSerializeElement
    private String presWeather;

    // Low level cloud type
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
    @Transient
    @DynamicSerializeElement
    private Short lowLevelCloudType;

    // Middle level cloud type
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
    @Transient
    @DynamicSerializeElement
    private Short midLevelCloudType;

    // High level cloud type
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
    @Transient
    @DynamicSerializeElement
    private Short highLevelCloudType;

    // Maximum temperature recording period
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
    @Transient
    @DynamicSerializeElement
    private Short maxTempRecordPeriod;

    // Maximum temperature kelvin
    @Transient
    @DynamicSerializeElement
    private Float maximumTemperature;

    // Minimum temperature recording period
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
    @Transient
    @DynamicSerializeElement
    private Short minTempRecordPeriod;

    // Minimum temperature kelvin
    @Transient
    @DynamicSerializeElement
    private Float minimumTemperature;

    // precip accumulation unknown time
    //
    // This is only used for CDoT. All of its accum precip is in
    // relation to precip since midnight local time. Obviously, each
    // report has a longer time period for precip coverage than the
    // previous report, so it's up to the user to figure out the
    // difference in time of the stored report versus midnight.
    @Transient
    @DynamicSerializeElement
    private Float precipAccum;

    // Precipitation type
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
    private Short precipType;

    // precipIntensity:value0 = "precipitation intensity info not available";
    // precipIntensity:value2 = "light";
    // precipIntensity:value3 = "moderate";
    // precipIntensity:value4 = "heavy";
    @Transient
    @DynamicSerializeElement
    private Short precipIntensity;

    // Time elapsed since last precipitation seconds
    @Transient
    @DynamicSerializeElement
    private Double timeSinceLastPcp;

    // Solar radiation - watt/meter2
    @Transient
    @DynamicSerializeElement
    private Float solarRadiation;

    // Solar radiation - time of last change (ALERT)
    // seconds since 1970-1-1 00:00:00.0
    @Transient
    @DynamicSerializeElement
    private Double solarRadChangeTime;

    // Sea surface temperature - kelvin
    @Transient
    @DynamicSerializeElement
    private Float seaSurfaceTemp;

    // Wave period - second
    @Transient
    @DynamicSerializeElement
    private Float wavePeriod;

    // Wave height - meter
    @Transient
    @DynamicSerializeElement
    private Float waveHeight;

    // Raw text LDAD mesonet message
    @Column
    @DynamicSerializeElement
    private String rawMessage;

    // Air Temperature in Kelvin
    @Transient
    @DynamicSerializeElement
    private Float temperature;

    // Dew point temperature in degrees Kelvin.
    @Transient
    @DynamicSerializeElement
    private Float dewpoint;

    // Relative Humidity in percent.
    @Transient
    @DynamicSerializeElement
    private Float relHumidity;

    // Wind direction in angular degrees. Integer
    @Transient
    @DynamicSerializeElement
    private Float windDir;

    // Observation wind speed in meters per second.
    @Transient
    @DynamicSerializeElement
    private Float windSpeed;

    // Wind gust in meters per second.
    @Transient
    @DynamicSerializeElement
    private Float windGust;

    // Observation pressure in Pa.
    @Transient
    @DynamicSerializeElement
    private Float pressure;

    // Sea level pressure
    @Transient
    @DynamicSerializeElement
    private Float seaLevelPressure;

    // Altimeter setting in Pa.
    @Transient
    @DynamicSerializeElement
    private Float altimeter;

    // Precipitation rate in m/sec.
    @Transient
    @DynamicSerializeElement
    private Float precipRate;

    // not in cdl
    @Transient
    @DynamicSerializeElement
    private Float fuelTemperature;

    // not in cdl
    @Transient
    @DynamicSerializeElement
    private Float fuelMoisture;

    // not in cdl
    @Transient
    @DynamicSerializeElement
    private Float soilTemperature;

    // not in cdl
    @Transient
    @DynamicSerializeElement
    private Float soilMoisture;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    /**
     * Default Constructor
     */
    public MesonetLdadRecord() {
        super();
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

    /**
     * @return the reportType
     */
    public String getReportType() {
        return reportType;
    }

    /**
     * @param reportType
     *            the reportType to set
     */
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
    @Override
    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
    }

    // ******************************************

    /**
     * @return the observationTime
     */
    @Override
    public Date getObservationTime() {
        return observationTime;
    }

    /**
     * @param observationTime
     *            the observationTime to set
     */
    @Override
    public void setObservationTime(Date observationTime) {
        this.observationTime = observationTime;
    }

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
     * @param dewpoint
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
     * @param relHumidity
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
     * @param windDir
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
     * @param precipRate
     *            the precipRate to set
     */
    public void setPrecipRate(Float precipRate) {
        this.precipRate = precipRate;
    }

    /**
     * @param location
     *            the spatial object to set
     */
    public void setSpatialObject(SurfaceObsLocation location) {
        this.location = location;
    }

    @Override
    public SurfaceObsLocation getSpatialObject() {
        return location;
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
    @Override
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
    @Override
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
    @Override
    public void setReportTime(long reportTime) {
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

    /**
     * @return the fuelTemperature
     */
    public Float getFuelTemperature() {
        return fuelTemperature;
    }

    /**
     * @param fuelTemperature
     *            the fuelTemperature to set
     */
    public void setFuelTemperature(Float fuelTemperature) {
        this.fuelTemperature = fuelTemperature;
    }

    /**
     * @return the fuelMoisture
     */
    public Float getFuelMoisture() {
        return fuelMoisture;
    }

    /**
     * @param fuelMoisture
     *            the fuelMoisture to set
     */
    public void setFuelMoisture(Float fuelMoisture) {
        this.fuelMoisture = fuelMoisture;
    }

    /**
     * @return the soilTemperature
     */
    public Float getSoilTemperature() {
        return soilTemperature;
    }

    /**
     * @return the soilMoisture
     */
    public Float getSoilMoisture() {
        return soilMoisture;
    }

    /**
     * @param soilMoisture
     *            the soilMoisture to set
     */
    public void setSoilMoisture(Float soilMoisture) {
        this.soilMoisture = soilMoisture;
    }

    /**
     * @param soilTemperature
     *            the soilTemperature to set
     */
    public void setSoilTemperature(Float soilTemperature) {
        this.soilTemperature = soilTemperature;
    }

    /**
     * @return the visibilityStatus
     */
    public String getVisibilityStatus() {
        return visibilityStatus;
    }

    /**
     * @param visibilityStatus
     *            the visibilityStatus to set
     */
    public void setVisibilityStatus(String visibilityStatus) {
        this.visibilityStatus = visibilityStatus;
    }

    /**
     * @return the rawMessage
     */
    public String getRawMessage() {
        return rawMessage;
    }

    /**
     * @param rawMessage
     *            the rawMessage to set
     */
    @Override
    public void setRawMessage(String rawMessage) {
        this.rawMessage = rawMessage;
    }

    /**
     * @return the precipAccum
     */
    public Float getPrecipAccum() {
        return precipAccum;
    }

    /**
     * @param precipAccum
     *            the precipAccum to set
     */
    public void setPrecipAccum(Float precipAccum) {
        this.precipAccum = precipAccum;
    }

    /**
     * @return the stationPressure
     */
    public Float getStationPressure() {
        return stationPressure;
    }

    /**
     * @param stationPressure
     *            the stationPressure to set
     */
    public void setStationPressure(Float stationPressure) {
        this.stationPressure = stationPressure;
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

    @Override
    public String toMessage() {
        StringBuilder sb = null;
        if ((getStationId() != null) && (getObservationTime() != null)) {
            sb = new StringBuilder();

            sb.append(getStationId());
            sb.append(",");
            sb.append(String.format(OBS_TIME_FMT, getObservationTime()));
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

    @Override
    public String getPluginName() {
        return "ldadmesonet";
    }
}
