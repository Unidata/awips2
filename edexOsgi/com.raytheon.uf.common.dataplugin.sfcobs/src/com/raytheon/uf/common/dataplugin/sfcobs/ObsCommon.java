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
package com.raytheon.uf.common.dataplugin.sfcobs;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import javax.persistence.Access;
import javax.persistence.AccessType;
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

import com.raytheon.uf.common.dataplugin.NullUtil;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.annotations.NullString;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import org.locationtech.jts.geom.Geometry;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 01, 2009           jkorman     Initial creation
 * Apr 04, 2013  1846     bkowal      Added an index on refTime and
 *                                    forecastTime
 * Apr 12, 2013  1857     bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013  1869     bsteffen    Remove dataURI column from
 *                                    PluginDataObject.
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Jun 11, 2014  2061     bsteffen    Remove IDecoderGettable 
 * Jul 27, 2015  4360     rferrel     Named unique constraint. Made reportType and corIndicator non-nullable.
 * Apr 20, 2016  DR18361  MPorricelli Added 1-min peak wind, snow depth, lowest pressure
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "sfcobsseq")
@Table(name = "sfcobs", uniqueConstraints = { @UniqueConstraint(name = "uk_sfcobs_datauri_fields", columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "sfcobs", indexes = { @Index(name = "sfcobs_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ObsCommon extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData {

    private static final long serialVersionUID = 1L;

    //
    @DataURI(position = 1)
    @NullString
    @Column(nullable = false)
    @XmlAttribute
    @DynamicSerializeElement
    @Index(name = "reporttype_index")
    private Integer reportType;

    // Correction indicator from wmo header
    @DataURI(position = 2)
    @NullString
    @Column(nullable = false, length = 1)
    @XmlElement
    @DynamicSerializeElement
    private String corIndicator = NullUtil.NULL_STRING;

    @Embedded
    @DataURI(position = 3, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    // Time of the observation.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar timeObs;

    // Time of the observation to the nearest hour.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar refHour;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private String obsText = "";

    // Text of the WMO header
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private String wmoHeader = "";

    // Observation air temperature in degrees Kelvin.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double temp = -9999.0;

    // Observation dewpoint temperature in degrees Kelvin.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double dwpt = -9999.0;

    // Relative Humidity in percent. Decimal(5,2)
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double humidity = -9999.0;

    // Observation sea surface temperature in degrees Kelvin.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double seaTemp = -9999.0;

    // Observation wetbulb temperature in degrees Kelvin.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double wetBulb = -9999.0;

    // Observation wind direction in angular degrees. Integer
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer windDirection = -9999;

    // Observation wind speed in meters per second.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double windSpeed = -9999.0;

    // Observation wind speed gust in meters per second.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double windGust = -9999.0;

    // Direction of the peak wind observation in angular degrees
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer peakWindDir = -9999;

    // Speed of the peak wind observation in meters per second.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double peakWindSpeed = -9999.0;

    // Time of the peak wind observation.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Long peakWindTime = -1L;
    // Direction of the one-minute peak wind observation in angular degrees
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer peakWindDirOneMin = -9999;

    // Speed of the one-minute peak wind observation in meters per second.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double peakWindSpeedOneMin = -9999.0;

    // Time of the one-minute peak wind observation.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Long peakWindTimeOneMin = -1L;

    // The equivilent 10 meter wind speed in meters per second.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double wind10mSpeed = -9999.0;

    // The equivilent 20 meter wind speed in meters per second.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double wind20mSpeed = -9999.0;

    // Altimeter setting in Pascals.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer pressureAltimeter = -9999;

    // Lowest pressure in Pascals.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer lowestPressure = -9999;

    // Time of the lowest pressure.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Long lowestPressureTime = -1L;

    // Sea level pressure in Pascals.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer pressureSealevel = -9999;

    // Station pressure in Pascals.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer pressureStation = -9999;

    // Three hour pressure change in pascals.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double pressChange3Hr = -9999.0;

    // Three hour pressure change characteristic.
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer pressChangeChar = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer wx_past_1 = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer wx_past_2 = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer wx_present = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private String presWeather = null;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer wx_report_type = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer horzVisibility = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer vertVisibility = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer totalCloudCover = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer cloudBaseHeight = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer lowCloudType = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer midCloudType = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer highCloudType = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer platformDirection = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double platformMovement = -9999.0;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private String shipIceData = "";

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double windWaveHeight = -9999.0;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer windWavePeriod = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double waveSteepness = -9999.0;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double waveHeight = -9999.0;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer wavePeriod = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double highResWaveHeight = -9999.0;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double primarySwellWaveDir = -9999.0;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer primarySwellWavePeriod = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double primarySwellWaveHeight = -9999.0;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double secondarySwellWaveDir = -9999.0;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer secondarySwellWavePeriod = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double secondarySwellWaveHeight = -9999.0;

    // Snow depth in mm
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Double snowDepth = -9999.0;

    // State of ground (category)
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private Integer stateOfGroundWithSnow = -9999;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private List<AncCloud> ancClouds;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private List<AncWave> ancWaves;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private List<AncTemp> ancTemp;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private List<AncPrecip> ancPrecip;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private List<AncWind> ancWinds;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private List<AncPressure> ancPressure;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private List<InterWinds> interWinds;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    /**
     * Empty default constructor
     */
    public ObsCommon() {
    }

    /**
     * Construct an instance of this class using the supplied datauri.
     * 
     * @param dataUri
     */
    public ObsCommon(String dataUri) {
        super(dataUri);
    }

    /**
     * @return the reportType
     */
    public Integer getReportType() {
        return reportType;
    }

    /**
     * @param reportType
     *            the reportType to set
     */
    public void setReportType(Integer reportType) {
        this.reportType = reportType;
    }

    /**
     * @return the corIndicator
     */
    public String getCorIndicator() {
        return NullUtil.convertNullStringToNull(this.corIndicator);
    }

    /**
     * @param corIndicator
     *            the corIndicator to set
     */
    public void setCorIndicator(String corIndicator) {
        this.corIndicator = NullUtil.convertNullToNullString(corIndicator);
    }

    /**
     * @return the timeObs
     */
    public Calendar getTimeObs() {
        return timeObs;
    }

    /**
     * @param timeObs
     *            the timeObs to set
     */
    public void setTimeObs(Calendar timeObs) {
        this.timeObs = timeObs;
    }

    /**
     * @return the refHour
     */
    public Calendar getRefHour() {
        return refHour;
    }

    /**
     * @param refHour
     *            the refHour to set
     */
    public void setRefHour(Calendar refHour) {
        this.refHour = refHour;
    }

    /**
     * @return the obsText
     */
    public String getObsText() {
        return obsText;
    }

    /**
     * @param obsText
     *            the obsText to set
     */
    public void setObsText(String obsText) {
        this.obsText = obsText;
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
     * @return the temp
     */
    public Double getTemp() {
        return temp;
    }

    /**
     * @param temp
     *            the temp to set
     */
    public void setTemp(Double temp) {
        this.temp = temp;
    }

    /**
     * @return the dwpt
     */
    public Double getDwpt() {
        return dwpt;
    }

    /**
     * @param dwpt
     *            the dwpt to set
     */
    public void setDwpt(Double dwpt) {
        this.dwpt = dwpt;
    }

    /**
     * @return the humidity
     */
    public Double getHumidity() {
        return humidity;
    }

    /**
     * @param humidity
     *            the humidity to set
     */
    public void setHumidity(Double humidity) {
        this.humidity = humidity;
    }

    /**
     * @return the seaTemp
     */
    public Double getSeaTemp() {
        return seaTemp;
    }

    /**
     * @param seaTemp
     *            the seaTemp to set
     */
    public void setSeaTemp(Double seaTemp) {
        this.seaTemp = seaTemp;
    }

    /**
     * @return the wetBulb
     */
    public Double getWetBulb() {
        return wetBulb;
    }

    /**
     * @param wetBulb
     *            the wetBulb to set
     */
    public void setWetBulb(Double wetBulb) {
        this.wetBulb = wetBulb;
    }

    /**
     * @return the windDirection
     */
    public Integer getWindDirection() {
        return windDirection;
    }

    /**
     * @param windDirection
     *            the windDirection to set
     */
    public void setWindDirection(Integer windDirection) {
        this.windDirection = windDirection;
    }

    /**
     * @return the windSpeed
     */
    public Double getWindSpeed() {
        return windSpeed;
    }

    /**
     * @param windSpeed
     *            the windSpeed to set
     */
    public void setWindSpeed(Double windSpeed) {
        this.windSpeed = windSpeed;
    }

    /**
     * 
     * @return the windGust
     */
    public Double getWindGust() {
        return windGust;
    }

    /**
     * 
     * @param windGust
     *            the windGust to set
     */
    public void setWindGust(Double windGust) {
        this.windGust = windGust;
    }

    /**
     * Get the direction of the peak wind observation.
     * 
     * @return The direction of the peak wind observation in angular degrees
     */
    public Integer getPeakWindDir() {
        return peakWindDir;
    }

    /**
     * Set the direction of the peak wind observation.
     * 
     * @param peakWindDir
     *            The direction of the peak wind observation in angular degrees
     */
    public void setPeakWindDir(Integer peakWindDir) {
        this.peakWindDir = peakWindDir;
    }

    /**
     * Get the speed of the peak wind observation.
     * 
     * @return The speed of the peak wind observation in meters per second.
     */
    public Double getPeakWindSpeed() {
        return peakWindSpeed;
    }

    /**
     * Set the speed of the peak wind observation.
     * 
     * @param peakWindSpeed
     *            The speed of the peak wind observation in meters per second.
     */
    public void setPeakWindSpeed(Double peakWindSpeed) {
        this.peakWindSpeed = peakWindSpeed;
    }

    /**
     * Set the time of the peak wind observation.
     * 
     * @return The time of the peak wind observation (msecs from 1-1-1970).
     */
    public Long getPeakWindTime() {
        return peakWindTime;
    }

    /**
     * Get the time of the peak wind observation.
     * 
     * @param peakWindTime
     *            The time of the peak wind observation (msecs from 1-1-1970).
     */
    public void setPeakWindTime(Long peakWindTime) {
        this.peakWindTime = peakWindTime;
    }
    /**
     * Get the direction of the one-minute peak wind observation.
     *
     * @return The direction of the one-minute peak wind observation in angular degrees
     */
    public Integer getPeakWindDirOneMin() {
        return peakWindDirOneMin;
    }

    /**
     * Set the direction of the peak wind observation.
     *
     * @param peakWindDirOneMin
     *            The direction of the peak wind observation in angular degrees
     */
    public void setPeakWindDirOneMin(Integer peakWindDirOneMin) {
        this.peakWindDirOneMin = peakWindDirOneMin;
    }

    /**
     * Get the speed of the one-minute peak wind observation.
     *
     * @return The speed of the one-minute peak wind observation in meters per second.
     */
    public Double getPeakWindSpeedOneMin() {
        return peakWindSpeedOneMin;
    }

    /**
     * Set the speed of the one-minute peak wind observation.
     *
     * @param peakWindSpeedOneMin
     *            The speed of the one-minute peak wind observation in meters per second.
     */
    public void setPeakWindSpeedOneMin(Double peakWindSpeedOneMin) {
        this.peakWindSpeedOneMin = peakWindSpeedOneMin;
    }

    /**
     * Set the time of the one-minute peak wind observation.
     *
     * @return The time of the one-minute peak wind observation (msecs from 1-1-1970).
     */
    public Long getPeakWindTimeOneMin() {
        return peakWindTimeOneMin;
    }

    /**
     * Get the time of the one-minute peak wind observation.
     *
     * @param peakWindTimeOneMin
     *            The time of the one-minute peak wind observation (msecs from 1-1-1970).
     */
    public void setPeakWindTimeOneMin(Long peakWindTimeOneMin) {
        this.peakWindTimeOneMin = peakWindTimeOneMin;
    }

    /**
     * Get the equivilent 10 meter wind speed.
     * 
     * @return The equivilent 10 meter wind speed in meters per second.
     */
    public Double getWind10mSpeed() {
        return wind10mSpeed;
    }

    /**
     * Set the equivilent 10 meter wind speed.
     * 
     * @param windSpeed
     *            The equivilent 20 meter wind speed in meters per second.
     */
    public void setWind10mSpeed(Double windSpeed) {
        this.wind10mSpeed = windSpeed;
    }

    /**
     * Get the equivilent 20 meter wind speed.
     * 
     * @return The equivilent 20 meter wind speed in meters per second.
     */
    public Double getWind20mSpeed() {
        return wind20mSpeed;
    }

    /**
     * Set the equivilent 20 meter wind speed.
     * 
     * @param windSpeed
     *            The equivilent 20 meter wind speed in meters per second.
     */
    public void setWind20mSpeed(Double windSpeed) {
        this.wind20mSpeed = windSpeed;
    }
    /**
     * Get the lowest pressure of previous hour.
     *
     * @return Lowest pressure in Pa
     */
    public Integer getLowestPressure() {
        return lowestPressure;
    }
    /**
     * Set the lowest pressure of previous hour.
     *
     * @param lowestPressure
     *            The lowest pressure in Pa
     */
    public void setLowestPressure(Integer lowestPressure) {
        this.lowestPressure = lowestPressure;
    }
    /**
     * Get observation time of the lowest pressure of previous hour.
     *
     * @return Time of the lowest pressure (msecs from 1-1-1970)
     */
    public Long getLowestPressureTime() {
        return lowestPressureTime;
    }
    /**
     * Set the observation time of the lowest pressure.
     *
     * @param lowestPressureTime
     *            The time of the lowest pressure observation (msecs from 1-1-1970).
     */
    public void setLowestPressureTime(Long lowestPressureTime) {
        this.lowestPressureTime = lowestPressureTime;
    }

    /**
     * Get the altimeter setting.
     * 
     * @return The altimeter setting in Pascals.
     */
    public Integer getPressureAltimeter() {
        return pressureAltimeter;
    }

    /**
     * Set the altimeter setting.
     * 
     * @param pressure
     *            The altimeter setting in Pascals.
     */
    public void setPressureAltimeter(Integer pressure) {
        pressureAltimeter = pressure;
    }

    /**
     * Get the sea level pressure.
     * 
     * @return The sea level pressure in Pascals.
     */
    public Integer getPressureSealevel() {
        return pressureSealevel;
    }

    /**
     * Set the sea level pressure.
     * 
     * @param pressure
     *            The sea level pressure in Pascals.
     */
    public void setPressureSealevel(Integer pressure) {
        pressureSealevel = pressure;
    }

    /**
     * Get the station pressure.
     * 
     * @return The station pressure in Pascals.
     */
    public Integer getPressureStation() {
        return pressureStation;
    }

    /**
     * Set the station pressure.
     * 
     * @param pressure
     *            The station pressure in Pascals.
     */
    public void setPressureStation(Integer pressure) {
        this.pressureStation = pressure;
    }

    /**
     * Get the three hour pressure change.
     * 
     * @return The three hour pressure change in Pascals.
     */
    public Double getPressChange3Hr() {
        return pressChange3Hr;
    }

    /**
     * Set the three hour pressure change.
     * 
     * <pre>
     *   0 = press same or higher than 3 hrs ago
     *   1 = increasing then steady
     *   2 = increasing
     *   3 = decreasing or steady,then increasing
     *   4 = steady
     *   5 = press same or lower than 3 hrs ago
     *   6 = decreasing then steady
     *   7 = decreasing
     *   8 = steady or increasing,then decreasing
     * </pre>
     * 
     * @param pressure
     *            The three hour pressure change in Pascals.
     */
    public void setPressChange3Hr(Double pressure) {
        this.pressChange3Hr = pressure;
    }

    /**
     * Get the three hour pressure change characteristic.
     * 
     * @return The three hour pressure change characteristic.
     */
    public Integer getPressChangeChar() {
        return pressChangeChar;
    }

    /**
     * Set the three hour pressure change characteristic.
     * 
     * @param pressChangeChar
     *            The three hour pressure change characteristic.
     */
    public void setPressChangeChar(Integer pressChangeChar) {
        this.pressChangeChar = pressChangeChar;
    }

    /**
     * @return the wx_past_1
     */
    public Integer getWx_past_1() {
        return wx_past_1;
    }

    /**
     * @param wx_past_1
     *            the wx_past_1 to set
     */
    public void setWx_past_1(Integer wx_past_1) {
        this.wx_past_1 = wx_past_1;
    }

    /**
     * @return the wx_past_2
     */
    public Integer getWx_past_2() {
        return wx_past_2;
    }

    /**
     * @param wx_past_2
     *            the wx_past_2 to set
     */
    public void setWx_past_2(Integer wx_past_2) {
        this.wx_past_2 = wx_past_2;
    }

    /**
     * @return the wx_present
     */
    public Integer getWx_present() {
        return wx_present;
    }

    /**
     * @param wx_present
     *            the wx_present to set
     */
    public void setWx_present(Integer wx_present) {
        this.wx_present = wx_present;
    }

    /**
     * @return the presWeather
     */
    public String getPresWeather() {
        return presWeather;
    }

    /**
     * @param presWeather
     *            the presWeather to set
     */
    public void setPresWeather(String presWeather) {
        this.presWeather = presWeather;
    }

    /**
     * @return the wx_report_type
     */
    public Integer getWx_report_type() {
        return wx_report_type;
    }

    /**
     * @param wx_report_type
     *            the wx_report_type to set
     */
    public void setWx_report_type(Integer wx_report_type) {
        this.wx_report_type = wx_report_type;
    }

    /**
     * @return the horzVisibility
     */
    public Integer getHorzVisibility() {
        return horzVisibility;
    }

    /**
     * @param horzVisibility
     *            the horzVisibility to set
     */
    public void setHorzVisibility(Integer horzVisibility) {
        this.horzVisibility = horzVisibility;
    }

    /**
     * @return the vertVisibility
     */
    public Integer getVertVisibility() {
        return vertVisibility;
    }

    /**
     * @param vertVisibility
     *            the vertVisibility to set
     */
    public void setVertVisibility(Integer vertVisibility) {
        this.vertVisibility = vertVisibility;
    }

    /**
     * @return the totalCloudCover
     */
    public Integer getTotalCloudCover() {
        return totalCloudCover;
    }

    /**
     * @param totalCloudCover
     *            the totalCloudCover to set
     */
    public void setTotalCloudCover(Integer totalCloudCover) {
        this.totalCloudCover = totalCloudCover;
    }

    /**
     * @return the cloudBaseHeight
     */
    public Integer getCloudBaseHeight() {
        return cloudBaseHeight;
    }

    /**
     * @param cloudBaseHeight
     *            the cloudBaseHeight to set
     */
    public void setCloudBaseHeight(Integer cloudBaseHeight) {
        this.cloudBaseHeight = cloudBaseHeight;
    }

    /**
     * @return the lowCloudType
     */
    public Integer getLowCloudType() {
        return lowCloudType;
    }

    /**
     * @param lowCloudType
     *            the lowCloudType to set
     */
    public void setLowCloudType(Integer lowCloudType) {
        this.lowCloudType = lowCloudType;
    }

    /**
     * @return the midCloudType
     */
    public Integer getMidCloudType() {
        return midCloudType;
    }

    /**
     * @param midCloudType
     *            the midCloudType to set
     */
    public void setMidCloudType(Integer midCloudType) {
        this.midCloudType = midCloudType;
    }

    /**
     * @return the highCloudType
     */
    public Integer getHighCloudType() {
        return highCloudType;
    }

    /**
     * @param highCloudType
     *            the highCloudType to set
     */
    public void setHighCloudType(Integer highCloudType) {
        this.highCloudType = highCloudType;
    }

    /**
     * @return the platformDirection
     */
    public Integer getPlatformDirection() {
        return platformDirection;
    }

    /**
     * @param platformDirection
     *            the platformDirection to set
     */
    public void setPlatformDirection(Integer direction) {
        platformDirection = direction;
    }

    /**
     * @return the platformMovement
     */
    public Double getPlatformMovement() {
        return platformMovement;
    }

    /**
     * @param platformMovement
     *            the platformMovement to set
     */
    public void setPlatformMovement(Double movement) {
        platformMovement = movement;
    }

    /**
     * @return the shipIceData
     */
    public String getShipIceData() {
        return shipIceData;
    }

    /**
     * @param shipIceData
     *            the shipIceData to set
     */
    public void setShipIceData(String iceData) {
        shipIceData = iceData;
    }

    /**
     * Set the wind wave height.
     * 
     * @param windWaveHeight
     *            The windWaveHeight in meters.
     */
    public void setWindWaveHeight(Double waveHeight) {
        windWaveHeight = waveHeight;
    }

    /**
     * Get the wind wave height.
     * 
     * @return The windWaveHeight in meters.
     */
    public Double getWindWaveHeight() {
        return windWaveHeight;
    }

    /**
     * Set the wind wave period.
     * 
     * @param windWavePeriod
     *            The windWavePeriod in seconds.
     */
    public void setWindWavePeriod(Integer wavePeriod) {
        windWavePeriod = wavePeriod;
    }

    /**
     * Get the wind wave period.
     * 
     * @return The windWavePeriod in seconds.
     */
    public Integer getWindWavePeriod() {
        return windWavePeriod;
    }

    /**
     * @return the waveSteepness
     */
    public Double getWaveSteepness() {
        return waveSteepness;
    }

    /**
     * @param waveSteepness
     *            the waveSteepness to set
     */
    public void setWaveSteepness(Double waveSteepness) {
        this.waveSteepness = waveSteepness;
    }

    /**
     * @return the waveHeight
     */
    public Double getWaveHeight() {
        return waveHeight;
    }

    /**
     * @param waveHeight
     *            the waveHeight to set
     */
    public void setWaveHeight(Double waveHeight) {
        this.waveHeight = waveHeight;
    }

    /**
     * @return the wavePeriod
     */
    public Integer getWavePeriod() {
        return wavePeriod;
    }

    /**
     * @param wavePeriod
     *            the wavePeriod to set
     */
    public void setWavePeriod(Integer wavePeriod) {
        this.wavePeriod = wavePeriod;
    }

    /**
     * @return the highResWaveHeight
     */
    public Double getHighResWaveHeight() {
        return highResWaveHeight;
    }

    /**
     * @param highResWaveHeight
     *            the highResWaveHeight to set
     */
    public void setHighResWaveHeight(Double waveHeight) {
        highResWaveHeight = waveHeight;
    }

    /**
     * @return the primarySwellWaveDir
     */
    public Double getPrimarySwellWaveDir() {
        return primarySwellWaveDir;
    }

    /**
     * @param primarySwellWaveDir
     *            the primarySwellWaveDir to set
     */
    public void setPrimarySwellWaveDir(Double waveDir) {
        primarySwellWaveDir = waveDir;
    }

    /**
     * @return the primarySwellWavePeriod
     */
    public Integer getPrimarySwellWavePeriod() {
        return primarySwellWavePeriod;
    }

    /**
     * @param primarySwellWavePeriod
     *            the primarySwellWavePeriod to set
     */
    public void setPrimarySwellWavePeriod(Integer primarySwellWavePeriod) {
        this.primarySwellWavePeriod = primarySwellWavePeriod;
    }

    /**
     * @return the primarySwellWaveHeight
     */
    public Double getPrimarySwellWaveHeight() {
        return primarySwellWaveHeight;
    }

    /**
     * @param primarySwellWaveHeight
     *            the primarySwellWaveHeight to set
     */
    public void setPrimarySwellWaveHeight(Double waveHeight) {
        primarySwellWaveHeight = waveHeight;
    }

    /**
     * @return the secondarySwellWaveDir
     */
    public Double getSecondarySwellWaveDir() {
        return secondarySwellWaveDir;
    }

    /**
     * @param secondarySwellWaveDir
     *            the secondarySwellWaveDir to set
     */
    public void setSecondarySwellWaveDir(Double waveDir) {
        secondarySwellWaveDir = waveDir;
    }

    /**
     * @return the secondarySwellWavePeriod
     */
    public Integer getSecondarySwellWavePeriod() {
        return secondarySwellWavePeriod;
    }

    /**
     * @param secondarySwellWavePeriod
     *            the secondarySwellWavePeriod to set
     */
    public void setSecondarySwellWavePeriod(Integer wavePeriod) {
        secondarySwellWavePeriod = wavePeriod;
    }

    /**
     * @return the secondarySwellWaveHeight
     */
    public Double getSecondarySwellWaveHeight() {
        return secondarySwellWaveHeight;
    }

    /**
     * @param secondarySwellWaveHeight
     *            the secondarySwellWaveHeight to set
     */
    public void setSecondarySwellWaveHeight(Double height) {
        secondarySwellWaveHeight = height;
    }

    /**
     * @return the snowDepth
     */
    public Double getSnowDepth() {
        return snowDepth;
    }
    /**
     * @param snowDepth
     *            the snowDepth to set
     */
    public void setSnowDepth(Double snowDepth) {
        this.snowDepth = snowDepth;
    }
    /**
     * @return stateOfGroundWithSnow indicator
     */
    public Integer getStateOfGroundWithSnow() {
        return stateOfGroundWithSnow;
    }
    /**
     * @param stateOfGroundWithSnow
     *            the stateOfGroundWithSnow indicator to set
     */
    public void setStateOfGroundWithSnow(Integer stateOfGroundWithSnow) {
        this.stateOfGroundWithSnow = stateOfGroundWithSnow;
    }
    /**
     * @return the ancClouds
     */
    public List<AncCloud> getAncClouds() {
        return ancClouds;
    }

    /**
     * @param ancClouds
     *            the ancClouds to set
     */
    public void setAncClouds(List<AncCloud> ancClouds) {
        this.ancClouds = ancClouds;
    }

    /**
     * 
     * @param cloud
     */
    public void addCloud(AncCloud cloud) {
        if (ancClouds == null) {
            ancClouds = new ArrayList<AncCloud>();
        }
        ancClouds.add(cloud);
    }

    /**
     * @return the ancWaves
     */
    public List<AncWave> getAncWaves() {
        return ancWaves;
    }

    /**
     * @param ancWaves
     *            the ancWaves to set
     */
    public void setAncWaves(List<AncWave> ancWaves) {
        this.ancWaves = ancWaves;
    }

    /**
     * 
     * @param wave
     */
    public void addWave(AncWave wave) {
        if (ancWaves == null) {
            ancWaves = new ArrayList<AncWave>();
        }
        ancWaves.add(wave);
    }

    /**
     * @return the ancTemp
     */
    public List<AncTemp> getAncTemp() {
        return ancTemp;
    }

    /**
     * @param ancTemp
     *            the ancTemp to set
     */
    public void setAncTemp(List<AncTemp> ancTemp) {
        this.ancTemp = ancTemp;
    }

    /**
     * 
     * @param temp
     */
    public void addTemp(AncTemp temp) {
        if (ancTemp == null) {
            ancTemp = new ArrayList<AncTemp>();
        }
        ancTemp.add(temp);
    }

    /**
     * @return the ancPrecip
     */
    public List<AncPrecip> getAncPrecip() {
        return ancPrecip;
    }

    /**
     * @param ancPrecip
     *            the ancPrecip to set
     */
    public void setAncPrecip(List<AncPrecip> ancPrecip) {
        this.ancPrecip = ancPrecip;
    }

    /**
     * 
     * @param precip
     */
    public void addPrecip(AncPrecip precip) {
        if (ancPrecip == null) {
            ancPrecip = new ArrayList<AncPrecip>();
        }
        ancPrecip.add(precip);
    }

    /**
     * @return the ancWinds
     */
    public List<AncWind> getAncWinds() {
        return ancWinds;
    }

    /**
     * @param ancWinds
     *            the ancWinds to set
     */
    public void setAncWinds(List<AncWind> ancWinds) {
        this.ancWinds = ancWinds;
    }

    /**
     * 
     * @param wind
     */
    public void addWind(AncWind wind) {
        if (ancWinds == null) {
            ancWinds = new ArrayList<AncWind>();
        }
        ancWinds.add(wind);
    }

    /**
     * @return the ancPressure
     */
    public List<AncPressure> getAncPressure() {
        return ancPressure;
    }

    /**
     * @param ancPressure
     *            the ancPressure to set
     */
    public void setAncPressure(List<AncPressure> ancPressure) {
        this.ancPressure = ancPressure;
    }

    /**
     * 
     * @param pressure
     */
    public void addPressure(AncPressure pressure) {
        if (ancPressure == null) {
            ancPressure = new ArrayList<AncPressure>();
        }
        ancPressure.add(pressure);
    }

    /**
     * @return the ancPressure
     */
    public List<InterWinds> getInterWinds() {
        return interWinds;
    }

    /**
     * @param ancPressure
     *            the ancPressure to set
     */
    public void setInterWinds(List<InterWinds> winds) {
        interWinds = winds;
    }

    /**
     * 
     * @param pressure
     */
    public void addInterWind(InterWinds wind) {
        if (interWinds == null) {
            interWinds = new ArrayList<InterWinds>();
        }
        interWinds.add(wind);
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
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "sfcobs";
    }
}
