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

package com.raytheon.uf.common.dataplugin.obs.metar;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.measure.quantity.Angle;
import javax.measure.quantity.Length;
import javax.measure.quantity.Pressure;
import javax.measure.quantity.Temperature;
import javax.measure.quantity.Velocity;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.obs.metar.util.SkyCover;
import com.raytheon.uf.common.dataplugin.obs.metar.util.WeatherCondition;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Record implementation for metar plugin
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#    Engineer    Description
 *  ------------ ---------- ----------- ---------------------------------------
 *  Feb 14, 2007 139        bphillip    Initial creation
 *  Nov 15, 2007            njensen     Added static units info.
 *  Nov 29, 2007 472        jkorman     Added IDecoderGettable interface.
 *  Dec 04, 2007 472        jkorman     getValue was using wrong select value.
 *  Dec 07, 2007 452        bphillip    Added station lat/lon
 *  Dec 17, 2007 472        jkorman     Changed to use ALTIMETER_UNIT.
 *  Dec 21, 2007 666        jkorman     Modified to default all numerics to
 *                                      -9999.
 *  Apr 23, 2009 2338       jsanchez    Implemented precip plots, cloud/vis.
 *                                      Added @DynamicSerializeElement to
 *                                      location.
 *  May 28, 2009 2225       jsanchez    Implemented tempFromTenths and
 *                                      dewPointFromTenths.
 *  Jun 29, 2009 2538       jsanchez    Made the sort public.
 *  Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                      forecastTime
 *  Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 *  May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 *  Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 *  Feb 11, 2014 2784       rferrel     Remove override of setIdentifier.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "obsseq")
@Table(name = "obs", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "obs", indexes = { @Index(name = "obs_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MetarRecord extends PersistablePluginDataObject implements
        ISpatialEnabled, IDecoderGettable, IPointData {

    public static final String PLUGIN_NAME = "obs";

    public static final String STATION_ID = "stationId";

    public static final Unit<Temperature> TEMPERATURE_UNIT = SI.CELSIUS;

    public static final Unit<Velocity> WIND_SPEED_UNIT = NonSI.KNOT;

    public static final Unit<Length> HEIGHT_UNIT = SI.METER;

    public static final Unit<Length> VISIBILITY_UNIT = SI.KILO(NonSI.MILE);

    public static final Unit<Angle> WIND_DIR_UNIT = NonSI.DEGREE_ANGLE;

    public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

    public static final Unit<Pressure> PRESSURE_UNIT = SI.HECTO(SI.PASCAL);

    public static final Unit<Pressure> ALTIMETER_UNIT = SI.PASCAL;

    public static final Unit<Length> PRECIP_UNIT = NonSI.INCH;

    /** Metar specific parameter keys */
    public static final class ParameterKey {
        public static final String SFC_ALTIMETER = "SFC.PRESS.ALTIMETER";

        public static final String PRESSURE_CHANGE = "PCHNG";

        public static final String VISIBILITY = "VIS";

        public static final String PRECIPITATION_1HR = "PR1HR";

        public static final String PRECIPITATION_3HR = "PR3HR";

        public static final String PRECIPITATION_6HR = "PR6HR";

        public static final String PRECIPITATION_24HR = "PR24HR";
    }

    /** Parameter keys used in the legacy system */
    public static final class LegacyParameterKey {

        public static final String TEMPERATURE = "T";

        public static final String DEW_POINT = "DpT";

        public static final String WIND_SPEED = "wSp";

        public static final String WIND_DIRECTION = "WD";

        public static final String WIND_GUST = "Gust";

        public static final String ALTIMETER = "Alti";

        public static final String SEA_LEVEL_PRESSURE = "msl-P";

        public static final String PRESSURE_CHANGE_3HR = "PT3";

        public static final String VISABILITY = "Vis";

        public static final String PRECIPITATION_1HR = "TP";

        public static final String PRECIPITATION_3HR = "TP3hr";

        public static final String PRECIPITATION_6HR = "TP6hr";

        public static final String PRECIPITATION_24HR = "TP24hr";

        // public static final String SNOW_DEPTH = "snow";
        //
        // public static final String SNOW_WATER = "weqs";
        //
        // public static final String SNOWFALL6_HOUR = "TP24hr";
        //
        // public static final String SUNSHINE = "msun";
        //
        // public static final String TEMP_MAX_6HOUR = "t6xc";
        //
        // public static final String TEMP_MIN_6HOUR = "t6nc";

        private String value;

        /**
         * 
         * @param value
         *            the value of this legacy parameter key
         */
        public void setValue(String value) {
            this.value = value;
        }

        /**
         * 
         * @return the value of this legacy parameter key
         */
        public String getValue() {
            return value;
        }

    }

    /** Serializable id * */
    private static final long serialVersionUID = 1L;

    /**
     * Maps common AWIPS I keys to IDecoderGettable and newer parameter key
     * constants
     */
    private static final HashMap<String, String> PARM_MAP = new HashMap<String, String>();
    static {
        PARM_MAP.put(LegacyParameterKey.TEMPERATURE, SFC_TEMP);
        PARM_MAP.put(LegacyParameterKey.DEW_POINT, SFC_DWPT);
        PARM_MAP.put(LegacyParameterKey.WIND_SPEED, SFC_WNDSPD);
        PARM_MAP.put(LegacyParameterKey.WIND_DIRECTION, SFC_WNDDIR);
        PARM_MAP.put(LegacyParameterKey.WIND_GUST, SFC_WNDGST);
        PARM_MAP.put(LegacyParameterKey.ALTIMETER, ParameterKey.SFC_ALTIMETER);
        PARM_MAP.put(LegacyParameterKey.SEA_LEVEL_PRESSURE, PRES_SLP);
        PARM_MAP.put("NLAT", STA_LAT);
        PARM_MAP.put("NLON", STA_LON);
        PARM_MAP.put(LegacyParameterKey.PRESSURE_CHANGE_3HR,
                ParameterKey.PRESSURE_CHANGE);
        // PARM_MAP.put("T24", "T24"); // not used
        // PARM_MAP.put("DpT24", "DpT24"); // not used
        // PARM_MAP.put("WS24", "WS24"); // not used
        // PARM_MAP.put("WD24", "WD24"); // not used
        // PARM_MAP.put("WGS24", "WGS24"); // not used
        // PARM_MAP.put("ASET24", "ASET24"); // not used
        // PARM_MAP.put("HIWC", "HIWC"); // not used
        PARM_MAP.put(LegacyParameterKey.VISABILITY, ParameterKey.VISIBILITY);
        PARM_MAP.put(LegacyParameterKey.PRECIPITATION_1HR,
                ParameterKey.PRECIPITATION_1HR);
        PARM_MAP.put(LegacyParameterKey.PRECIPITATION_3HR,
                ParameterKey.PRECIPITATION_3HR);
        PARM_MAP.put(LegacyParameterKey.PRECIPITATION_6HR,
                ParameterKey.PRECIPITATION_6HR);
        PARM_MAP.put(LegacyParameterKey.PRECIPITATION_24HR,
                ParameterKey.PRECIPITATION_24HR);
    }

    @Transient
    private String sampleType = null;

    @Transient
    private boolean isSkyCoverageSorted = false;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private String report;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private String wmoHeader;

    /** Nominal Time extracted from WMO header * */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private String nominalTime;

    /** A string denoting the time of observation */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private Calendar timeObs;

    /** Report type extracted from WMO header * */
    @XmlElement
    @DynamicSerializeElement
    @Column
    @DataURI(position = 1)
    protected String reportType;

    /** A string denoting if this report is a correction */
    @XmlElement
    @DynamicSerializeElement
    @Column
    @DataURI(position = 2)
    private String correction;

    @Embedded
    @DataURI(position = 3, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    /** A string denoting the type of automated station (AO1 or AO2) */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private String autoStationType;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private String skyKey;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private Set<SkyCover> skyCoverage = new HashSet<SkyCover>();

    /** A string denoting the vertical visibility */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private int vertVisibility = -9999;

    /** A string denoting the lowest layer of clouds */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private int skyLayerBase = -9999;

    /** The visibility in statute miles */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float visibility = -9999;

    /** A String used as the foreign key to the present weather table * */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private String weatherKey;

    /** A Set of present weather conditions * */
    @DynamicSerializeElement
    @XmlElement
    @Transient
    private List<WeatherCondition> weatherCondition = new ArrayList<WeatherCondition>();

    /** A string denoting the sea level pressure in millibars */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float seaLevelPress = -9999;

    /** A string denoting the temperature in degrees Celsius */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private int temperature = -9999;

    /** A string denoting the current temperature in tenths of degrees Celsius */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float tempFromTenths = -9999;

    /** A string denoting the current dew point in degrees Celsius */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private int dewPoint = -9999;

    /** A string denoting the current dew point in tenths of degrees Celsius */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float dewPointFromTenths = -9999;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private String windDir;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private int windSpeed = -9999;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private int windGust = -9999;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private int pkWndDir = -9999;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private int pkWndSpd = -9999;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private Calendar pkWndTime = null;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float altimeterInPa = -9999;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float altimeter = -9999;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float minTemp24Hour = -9999;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float maxTemp24Hour = -9999;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float minTemp6Hour = -9999;

    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float maxTemp6Hour = -9999;

    /** A string denoting inches of precipitation observed in the last hour */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float precip1Hour = -9999;

    /** A string denoting inches of precipitation observed in the last 3 hours */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float precip3Hour = -9999;

    /** A string denoting inches of precipitation observed in the last 6 hours */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float precip6Hour = -9999;

    /** A string denoting inches of precipitation observed in the last 24 hours */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float precip24Hour = -9999;

    /** A string denoting the pressure tendency(rising or falling) */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private String pressChangeChar;

    /** A string denoting the pressure change observed in the past 3 hrs. */
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float pressChange3Hour = -9999;

    // Amount of snow on the ground in inches.
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private int snowDepth = -9999;

    // Water equivalent in 0.1 inch increments.
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float snowWater = -9999;

    // Snow fall last 6 hours.
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private float snowFall_6Hours = -9999;

    // Number of minutes of sunshine.
    @XmlElement
    @DynamicSerializeElement
    @Transient
    private int sunshine = -9999;

    @XmlElement
    @DynamicSerializeElement
    @Column
    private Calendar refHour;

    @DynamicSerializeElement
    @Embedded
    private PointDataView pointDataView;

    public MetarRecord() {
    }

    /**
     * Constructs a metar record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public MetarRecord(String uri) {
        super(uri);
    }

    /**
     * @return the serialVersionUID
     */
    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * @return the altimeter
     */
    public float getAltimeter() {
        return altimeter;
    }

    /**
     * @param altimeter
     *            the altimeter to set
     */
    public void setAltimeter(float altimeter) {
        this.altimeter = altimeter;
    }

    /**
     * @return the altimeterInPa
     */
    public float getAltimeterInPa() {
        return altimeterInPa;
    }

    /**
     * @param altimeterInPa
     *            the altimeterInPa to set
     */
    public void setAltimeterInPa(float altimeterInPa) {
        this.altimeterInPa = altimeterInPa;
    }

    /**
     * @return the autoStationType
     */
    public String getAutoStationType() {
        return autoStationType;
    }

    /**
     * @param autoStationType
     *            the autoStationType to set
     */
    public void setAutoStationType(String autoStationType) {
        this.autoStationType = autoStationType;
    }

    /**
     * @return the correction
     */
    public String getCorrection() {
        return correction;
    }

    /**
     * @param correction
     *            the correction to set
     */
    public void setCorrection(String correction) {
        this.correction = correction;
    }

    /**
     * @return the dewPoint
     */
    public int getDewPoint() {
        return dewPoint;
    }

    /**
     * @param dewPoint
     *            the dewPoint to set
     */
    public void setDewPoint(int dewPoint) {
        this.dewPoint = dewPoint;
    }

    /**
     * @return the dewPointFromTenths
     */
    public float getDewPointFromTenths() {
        return dewPointFromTenths;
    }

    /**
     * @param dewPointFromTenths
     *            the dewPointFromTenths to set
     */
    public void setDewPointFromTenths(float dewPointFromTenths) {
        this.dewPointFromTenths = dewPointFromTenths;
    }

    /**
     * @return the maxTemp24Hour
     */
    public float getMaxTemp24Hour() {
        return maxTemp24Hour;
    }

    /**
     * @param maxTemp24Hour
     *            the maxTemp24Hour to set
     */
    public void setMaxTemp24Hour(float maxTemp24Hour) {
        this.maxTemp24Hour = maxTemp24Hour;
    }

    /**
     * @return the minTemp24Hour
     */
    public float getMinTemp24Hour() {
        return minTemp24Hour;
    }

    /**
     * @param minTemp24Hour
     *            the minTemp24Hour to set
     */
    public void setMinTemp24Hour(float minTemp24Hour) {
        this.minTemp24Hour = minTemp24Hour;
    }

    /**
     * @return the minTemp6Hour
     */
    public float getMinTemp6Hour() {
        return minTemp6Hour;
    }

    /**
     * @param minTemp6Hour
     *            the minTemp6Hour to set
     */
    public void setMinTemp6Hour(float minTemp6Hour) {
        this.minTemp6Hour = minTemp6Hour;
    }

    /**
     * @return the maxTemp6Hour
     */
    public float getMaxTemp6Hour() {
        return maxTemp6Hour;
    }

    /**
     * @param maxTemp6Hour
     *            the maxTemp6Hour to set
     */
    public void setMaxTemp6Hour(float maxTemp6Hour) {
        this.maxTemp6Hour = maxTemp6Hour;
    }

    /**
     * @return the precip1Hour
     */
    public float getPrecip1Hour() {
        return precip1Hour;
    }

    /**
     * @param precip1Hour
     *            the precip1Hour to set
     */
    public void setPrecip1Hour(float precip1Hour) {
        this.precip1Hour = precip1Hour;
    }

    /**
     * @return the precip3Hour
     */
    public float getPrecip3Hour() {
        return precip3Hour;
    }

    /**
     * @param precip3Hour
     *            the precip3Hour to set
     */
    public void setPrecip3Hour(float precip3Hour) {
        this.precip3Hour = precip3Hour;
    }

    /**
     * @return the precip6Hour
     */
    public float getPrecip6Hour() {
        return precip6Hour;
    }

    /**
     * @param precip6Hour
     *            the precip6Hour to set
     */
    public void setPrecip6Hour(float precip6Hour) {
        this.precip6Hour = precip6Hour;
    }

    /**
     * @return the precip24Hour
     */
    public float getPrecip24Hour() {
        return precip24Hour;
    }

    /**
     * @param precip24Hour
     *            the precip24Hour to set
     */
    public void setPrecip24Hour(float precip24Hour) {
        this.precip24Hour = precip24Hour;
    }

    /**
     * @return the pressChange3Hour
     */
    public float getPressChange3Hour() {
        return pressChange3Hour;
    }

    /**
     * @param pressChange3Hour
     *            the pressChange3Hour to set
     */
    public void setPressChange3Hour(float pressChange3Hour) {
        this.pressChange3Hour = pressChange3Hour;
    }

    /**
     * @return the pressChangeChar
     */
    public String getPressChangeChar() {
        return pressChangeChar;
    }

    /**
     * @param pressChangeChar
     *            the pressChangeChar to set
     */
    public void setPressChangeChar(String pressChangeChar) {
        this.pressChangeChar = pressChangeChar;
    }

    /**
     * @return the seaLevelPress
     */
    public float getSeaLevelPress() {
        return seaLevelPress;
    }

    /**
     * @param seaLevelPress
     *            the seaLevelPress to set
     */
    public void setSeaLevelPress(float seaLevelPress) {
        this.seaLevelPress = seaLevelPress;
    }

    /**
     * @return the skyLayerBase
     */
    public int getSkyLayerBase() {
        return skyLayerBase;
    }

    /**
     * @param skyLayerBase
     *            the skyLayerBase to set
     */
    public void setSkyLayerBase(int skyLayerBase) {
        this.skyLayerBase = skyLayerBase;
    }

    /**
     * @return the temperature
     */
    public int getTemperature() {
        return temperature;
    }

    /**
     * @param temperature
     *            the temperature to set
     */
    public void setTemperature(int temperature) {
        this.temperature = temperature;
    }

    /**
     * @return the tempFromTenths
     */
    public float getTempFromTenths() {
        return tempFromTenths;
    }

    /**
     * @param tempFromTenths
     *            the tempFromTenths to set
     */
    public void setTempFromTenths(float tempFromTenths) {
        this.tempFromTenths = tempFromTenths;
    }

    /**
     * @return the vertVisibility
     */
    public int getVertVisibility() {
        return vertVisibility;
    }

    /**
     * @param vertVisibility
     *            the vertVisibility to set
     */
    public void setVertVisibility(int vertVisibility) {
        this.vertVisibility = vertVisibility;
    }

    /**
     * @return the visibility
     */
    public float getVisibility() {

        return visibility;
    }

    /**
     * @param visibility
     *            the visibility to set
     */
    public void setVisibility(float visibility) {
        this.visibility = visibility;
    }

    /**
     * @return the windDir
     */
    public String getWindDir() {
        return windDir;
    }

    /**
     * @param windDir
     *            the windDir to set
     */
    public void setWindDir(String windDir) {
        this.windDir = windDir;
    }

    /**
     * @return the windGust
     */
    public int getWindGust() {
        return windGust;
    }

    /**
     * @param windGust
     *            the windGust to set
     */
    public void setWindGust(int windGust) {
        this.windGust = windGust;
    }

    /**
     * @return the windSpeed
     */
    public int getWindSpeed() {
        return windSpeed;
    }

    /**
     * @param windSpeed
     *            the windSpeed to set
     */
    public void setWindSpeed(int windSpeed) {
        this.windSpeed = windSpeed;
    }

    /**
     * @return the pkWndDir
     */
    public int getPkWndDir() {
        return pkWndDir;
    }

    /**
     * @param pkWndDir
     *            the pkWndDir to set
     */
    public void setPkWndDir(int pkWndDir) {
        this.pkWndDir = pkWndDir;
    }

    /**
     * @return the pkWndSpd
     */
    public int getPkWndSpd() {
        return pkWndSpd;
    }

    /**
     * @param pkWndSpd
     *            the pkWndSpd to set
     */
    public void setPkWndSpd(int pkWndSpd) {
        this.pkWndSpd = pkWndSpd;
    }

    /**
     * @return the pkWndTime
     */
    public Calendar getPkWndTime() {
        return pkWndTime;
    }

    /**
     * @param pkWndTime
     *            the pkWndTime to set
     */
    public void setPkWndTime(Calendar pkWndTime) {
        this.pkWndTime = pkWndTime;
    }

    /**
     * @return the timeObs
     */
    public Calendar getTimeObs() {
        if (this.dataTime == null) {
            return null;
        }
        return this.dataTime.getRefTimeAsCalendar();
    }

    /**
     * @param timeObs
     *            the timeObs to set
     */
    public void setTimeObs(Calendar timeObs) {
        this.nominalTime = TimeUtil.formatCalendar(timeObs);
        this.timeObs = timeObs;
    }

    /**
     * @return the skyCoverage
     */
    public Set<SkyCover> getSkyCoverage() {
        return skyCoverage;
    }

    /**
     * @param skyCoverage
     *            the skyCoverage to set
     */
    public void setSkyCoverage(Set<SkyCover> skyCoverage) {
        this.skyCoverage = skyCoverage;
        if ((skyCoverage != null) && (skyCoverage.size() > 0)) {
            for (SkyCover cover : skyCoverage) {
                cover.setParentMetar(this);
            }
        }
    }

    public void addSkyCoverage(SkyCover cover) {
        skyCoverage.add(cover);
        cover.setParentMetar(this);
    }

    /**
     * @return the skyKey
     */
    public String getSkyKey() {
        return skyKey;
    }

    /**
     * @param skyKey
     *            the skyKey to set
     */
    public void setSkyKey(String skyKey) {
        this.skyKey = skyKey;
    }

    /**
     * @return the weatherCondition
     */
    public List<WeatherCondition> getWeatherCondition() {
        return weatherCondition;
    }

    /**
     * @param weatherCondition
     *            the weatherCondition to set
     */
    public void setWeatherCondition(List<WeatherCondition> weatherCondition) {
        this.weatherCondition = weatherCondition;
        if ((weatherCondition != null) && (weatherCondition.size() > 0)) {
            for (WeatherCondition cond : weatherCondition) {
                cond.setParentMetar(this);
            }
        }
    }

    public void addWeatherCondition(WeatherCondition condition) {
        this.weatherCondition.add(condition);
        condition.setParentMetar(this);
    }

    /**
     * @return the weatherKey
     */
    public String getWeatherKey() {
        return weatherKey;
    }

    /**
     * @param weatherKey
     *            the weatherKey to set
     */
    public void setWeatherKey(String weatherKey) {
        this.weatherKey = weatherKey;
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
     * @return the snowDepth
     */
    public int getSnowDepth() {
        return snowDepth;
    }

    /**
     * @param snowDepth
     *            the snowDepth to set
     */
    public void setSnowDepth(int snowDepth) {
        this.snowDepth = snowDepth;
    }

    /**
     * @return the snowFall_6Hours
     */
    public float getSnowFall_6Hours() {
        return snowFall_6Hours;
    }

    /**
     * @param snowFall_6Hours
     *            the snowFall_6Hours to set
     */
    public void setSnowFall_6Hours(float snowFall_6Hours) {
        this.snowFall_6Hours = snowFall_6Hours;
    }

    /**
     * @return the sunshine
     */
    public int getSunshine() {
        return sunshine;
    }

    /**
     * @param sunshine
     *            the sunshine to set
     */
    public void setSunshine(int sunshine) {
        this.sunshine = sunshine;
    }

    /**
     * @return the snowWater
     */
    public float getSnowWater() {
        return snowWater;
    }

    /**
     * @param snowWater
     *            the snowWater to set
     */
    public void setSnowWater(float snowWater) {
        this.snowWater = snowWater;
    }

    public String getReportType() {
        return reportType;
    }

    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    /**
     * Get the station identifier for this observation.
     * 
     * @return the stationId
     */
    public String getStationId() {
        return location.getStationId();
    }

    public String getNominalTime() {
        return nominalTime;
    }

    public void setNominalTime(String nominalTime) {
        this.nominalTime = nominalTime;
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
            return getStationId();
        }
        if ("WX".matches(paramName)) {
            return this.weatherKey;
        }

        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataplugin.IDecoderGettable#getValue(java.lang
     * .String)
     */
    @Override
    public Amount getValue(String paramName) {
        Amount a = null;

        String pName = paramName;
        if (PARM_MAP.containsKey(paramName)) {
            // we have recieved an AWIPS I parameter name
            pName = PARM_MAP.get(paramName);
        }

        if (SFC_TEMP.equals(pName)) {
            if (tempFromTenths != -9999) {
                a = new Amount(tempFromTenths, TEMPERATURE_UNIT);
            } else if (temperature != -9999) {
                a = new Amount(temperature, TEMPERATURE_UNIT);
            }
        } else if (SFC_DWPT.equals(pName)) {
            if (dewPointFromTenths != -9999) {
                a = new Amount(dewPointFromTenths, TEMPERATURE_UNIT);
            } else if (dewPoint != -9999) {
                a = new Amount(dewPoint, TEMPERATURE_UNIT);
            }
        } else if (SFC_WNDSPD.equals(pName)) {
            a = new Amount(windSpeed, WIND_SPEED_UNIT);
        } else if (SFC_WNDGST.equals(pName)) {
            a = new Amount(windGust, WIND_SPEED_UNIT);
        } else if (ParameterKey.PRESSURE_CHANGE.equals(pName)
                && (pressChange3Hour != -9999)) {
            a = new Amount(pressChange3Hour, PRESSURE_UNIT);
        } else if (SFC_WNDDIR.equals(pName)) {
            String windDir = getWindDir();
            if ((windDir != null) && !windDir.equalsIgnoreCase("VRB")) {
                Double result = Double.parseDouble(windDir);
                a = new Amount(result, WIND_DIR_UNIT);
            }
        } else if (PRES_ALTSG.equals(pName)) {
            a = new Amount(altimeterInPa, ALTIMETER_UNIT);
        } else if (STA_LAT.equals(pName)) {
            a = new Amount(getLatitude(), LOCATION_UNIT);
        } else if (STA_LON.equals(pName)) {
            a = new Amount(getLongitude(), LOCATION_UNIT);
        } else if (PRES_SLP.equals(pName)) {
            a = new Amount(this.seaLevelPress, PRESSURE_UNIT);
        } else if (pName.startsWith("HGT")) {
            int start = "HGT".length();
            int index = Integer.parseInt(pName.substring(start));
            if ((index < skyCoverage.size())
                    && (getSkyCover(index).getHeight() != null)) {
                a = new Amount(getSkyCover(index).getHeight(), HEIGHT_UNIT);
            }
        } else if (pName.startsWith("HMSL")) {
            int start = "HMSL".length();
            int index = Integer.parseInt(pName.substring(start));
            if ((index < skyCoverage.size())
                    && (getSkyCover(index).getHeight() != null)
                    && (getSpatialObject() != null)
                    && (getSpatialObject().getElevation() != null)) {
                a = new Amount(getSkyCover(index).getHeight()
                        + getSpatialObject().getElevation(), HEIGHT_UNIT);
            }
        } else if (ParameterKey.PRECIPITATION_1HR.equals(pName)
                || ParameterKey.PRECIPITATION_24HR.equals(pName)) {
            sampleType = "PR";
            if (precip1Hour != -9999) {
                a = new Amount(precip1Hour, PRECIP_UNIT);
            }
        } else if (ParameterKey.PRECIPITATION_3HR.equals(pName)) {
            sampleType = "PR";
            if (precip3Hour != -9999) {
                a = new Amount(precip3Hour, PRECIP_UNIT);
            }
        } else if (ParameterKey.PRECIPITATION_6HR.equals(pName)) {
            sampleType = "PR";
            if (precip6Hour != -9999) {
                a = new Amount(precip6Hour, PRECIP_UNIT);
            }
        } else if (ParameterKey.VISIBILITY.equals(pName)) {
            a = new Amount(visibility, VISIBILITY_UNIT);
        }

        return a;
    }

    /**
     * Get the value and units of a named parameter within this observation that
     * has a multiplicity greater than 1.
     * 
     * @param paramName
     *            The name of the parameter value to retrieve.
     * @return An Amount with value and units. If the parameter is unknown, a
     *         null reference is returned.
     */
    @Override
    public Collection<Amount> getValues(String paramName) {
        return null;
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

    @Override
    public String[] getStrings(String paramName) {
        if ("SCV".matches(paramName)) {
            ArrayList<String> skyCoverage = new ArrayList<String>();
            for (SkyCover sky : this.skyCoverage) {
                skyCoverage.add(sky.getType());
            }
            if (skyCoverage.size() > 0) {
                return skyCoverage.toArray(new String[skyCoverage.size()]);
            }
        } else if ("WX".matches(paramName)) {
            if (this.weatherKey != null) {
                String[] presentWeather = { this.weatherKey };
                return presentWeather;
            }
        } else if (paramName.startsWith("CLD")) {
            int start = "CLD".length();
            int index = Integer.parseInt(paramName.substring(start));
            String[] retVal = { "BLNK" };
            if (index < skyCoverage.size()) {
                if (getSkyCover(index).getType() != null) {
                    retVal[0] = getSkyCover(index).getType();
                }
            }
            return retVal;
        } else if (paramName.matches("CCHAR") && (pressChangeChar != null)) {
            String[] changeChar = { pressChangeChar };
            return changeChar;
        }
        return null;
    }

    @Override
    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = (PRIME * result) + Float.floatToIntBits(altimeter);
        result = (PRIME * result) + Float.floatToIntBits(altimeterInPa);
        result = (PRIME * result)
                + ((autoStationType == null) ? 0 : autoStationType.hashCode());
        result = (PRIME * result)
                + ((correction == null) ? 0 : correction.hashCode());
        result = (PRIME * result) + dewPoint;
        result = (PRIME * result) + Float.floatToIntBits(dewPointFromTenths);
        result = (PRIME * result) + Float.floatToIntBits(maxTemp24Hour);
        result = (PRIME * result) + Float.floatToIntBits(minTemp24Hour);
        result = (PRIME * result)
                + ((nominalTime == null) ? 0 : nominalTime.hashCode());
        result = (PRIME * result) + Float.floatToIntBits(precip1Hour);
        result = (PRIME * result) + Float.floatToIntBits(precip3Hour);
        result = (PRIME * result) + Float.floatToIntBits(precip6Hour);
        result = (PRIME * result) + Float.floatToIntBits(pressChange3Hour);
        result = (PRIME * result)
                + ((pressChangeChar == null) ? 0 : pressChangeChar.hashCode());
        result = (PRIME * result)
                + ((refHour == null) ? 0 : refHour.hashCode());
        result = (PRIME * result)
                + ((reportType == null) ? 0 : reportType.hashCode());
        result = (PRIME * result) + Float.floatToIntBits(seaLevelPress);
        result = (PRIME * result)
                + ((skyCoverage == null) ? 0 : skyCoverage.hashCode());
        result = (PRIME * result) + ((skyKey == null) ? 0 : skyKey.hashCode());
        result = (PRIME * result) + skyLayerBase;
        result = (PRIME * result)
                + ((getStationId() == null) ? 0 : getStationId().hashCode());
        long temp;
        temp = Double.doubleToLongBits(getLatitude());
        result = (PRIME * result) + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(getLongitude());
        result = (PRIME * result) + (int) (temp ^ (temp >>> 32));
        result = (PRIME * result) + Float.floatToIntBits(tempFromTenths);
        result = (PRIME * result) + temperature;
        result = (PRIME * result)
                + ((timeObs == null) ? 0 : timeObs.hashCode());
        result = (PRIME * result) + vertVisibility;
        result = (PRIME * result) + +Float.floatToIntBits(visibility);

        result = (PRIME * result)
                + ((weatherCondition == null) ? 0 : weatherCondition.hashCode());
        result = (PRIME * result)
                + ((weatherKey == null) ? 0 : weatherKey.hashCode());
        result = (PRIME * result)
                + ((windDir == null) ? 0 : windDir.hashCode());
        result = (PRIME * result) + windGust;
        result = (PRIME * result) + windSpeed;
        return result;
    }

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
        final MetarRecord other = (MetarRecord) obj;
        if (Float.floatToIntBits(altimeter) != Float
                .floatToIntBits(other.altimeter)) {
            return false;
        }
        if (Float.floatToIntBits(altimeterInPa) != Float
                .floatToIntBits(other.altimeterInPa)) {
            return false;
        }
        if (autoStationType == null) {
            if (other.autoStationType != null) {
                return false;
            }
        } else if (!autoStationType.equals(other.autoStationType)) {
            return false;
        }
        if (correction == null) {
            if (other.correction != null) {
                return false;
            }
        } else if (!correction.equals(other.correction)) {
            return false;
        }
        if (dewPoint != other.dewPoint) {
            return false;
        }
        if (Float.floatToIntBits(dewPointFromTenths) != Float
                .floatToIntBits(other.dewPointFromTenths)) {
            return false;
        }
        if (Float.floatToIntBits(maxTemp24Hour) != Float
                .floatToIntBits(other.maxTemp24Hour)) {
            return false;
        }
        if (Float.floatToIntBits(minTemp24Hour) != Float
                .floatToIntBits(other.minTemp24Hour)) {
            return false;
        }
        if (nominalTime == null) {
            if (other.nominalTime != null) {
                return false;
            }
        } else if (!nominalTime.equals(other.nominalTime)) {
            return false;
        }
        if (Float.floatToIntBits(precip1Hour) != Float
                .floatToIntBits(other.precip1Hour)) {
            return false;
        }
        if (Float.floatToIntBits(precip3Hour) != Float
                .floatToIntBits(other.precip3Hour)) {
            return false;
        }
        if (Float.floatToIntBits(precip6Hour) != Float
                .floatToIntBits(other.precip6Hour)) {
            return false;
        }
        if (Float.floatToIntBits(pressChange3Hour) != Float
                .floatToIntBits(other.pressChange3Hour)) {
            return false;
        }
        if (pressChangeChar == null) {
            if (other.pressChangeChar != null) {
                return false;
            }
        } else if (!pressChangeChar.equals(other.pressChangeChar)) {
            return false;
        }
        if (refHour == null) {
            if (other.refHour != null) {
                return false;
            }
        } else if (!refHour.equals(other.refHour)) {
            return false;
        }
        if (reportType == null) {
            if (other.reportType != null) {
                return false;
            }
        } else if (!reportType.equals(other.reportType)) {
            return false;
        }
        if (Float.floatToIntBits(seaLevelPress) != Float
                .floatToIntBits(other.seaLevelPress)) {
            return false;
        }
        if (skyCoverage == null) {
            if (other.skyCoverage != null) {
                return false;
            }
        } else if (!skyCoverage.equals(other.skyCoverage)) {
            return false;
        }
        if (skyKey == null) {
            if (other.skyKey != null) {
                return false;
            }
        } else if (!skyKey.equals(other.skyKey)) {
            return false;
        }
        if (skyLayerBase != other.skyLayerBase) {
            return false;
        }

        if (getStationId() == null) {
            if (other.getStationId() != null) {
                return false;
            }
        } else if (!getStationId().equals(other.getStationId())) {
            return false;
        }

        Double lat = location.getLatitude();
        if (lat == null) {
            if (other.location.getLatitude() != null) {
                return false;
            }
        } else {
            if (!lat.equals(other.location.getLatitude())) {
                return false;
            }
        }
        Double lon = location.getLongitude();
        if (lon == null) {
            if (other.location.getLongitude() != null) {
                return false;
            }
        } else {
            if (!lon.equals(other.location.getLongitude())) {
                return false;
            }
        }

        if (Float.floatToIntBits(tempFromTenths) != Float
                .floatToIntBits(other.tempFromTenths)) {
            return false;
        }
        if (temperature != other.temperature) {
            return false;
        }
        if (timeObs == null) {
            if (other.timeObs != null) {
                return false;
            }
        } else if (!timeObs.equals(other.timeObs)) {
            return false;
        }
        if (vertVisibility != other.vertVisibility) {
            return false;
        }
        if (Float.floatToIntBits(visibility) != Float
                .floatToIntBits(other.visibility)) {
            return false;
        }

        if (weatherCondition == null) {
            if (other.weatherCondition != null) {
                return false;
            }
        } else if (!weatherCondition.equals(other.weatherCondition)) {
            return false;
        }
        if (weatherKey == null) {
            if (other.weatherKey != null) {
                return false;
            }
        } else if (!weatherKey.equals(other.weatherKey)) {
            return false;
        }
        if (windDir == null) {
            if (other.windDir != null) {
                return false;
            }
        } else if (!windDir.equals(other.windDir)) {
            return false;
        }
        if (windGust != other.windGust) {
            return false;
        }
        if (windSpeed != other.windSpeed) {
            return false;
        }
        return true;
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

    public String getReport() {
        return report;
    }

    public void setReport(String report) {
        this.report = report;
    }

    /**
     * 
     * @return
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * 
     * @param header
     */
    public void setWmoHeader(String header) {
        wmoHeader = header;
    }

    @Override
    public void setMessageData(Object message) {
        this.messageData = message;
        this.report = (String) message;
    }

    @Override
    public String getMessageData() {
        if ((sampleType != null) && sampleType.equals("PR")) {
            return getStationId();
        }
        return report;
    }

    private SkyCover getSkyCover(int index) {
        if (!isSkyCoverageSorted) {
            isSkyCoverageSorted = true;
            sort(skyCoverage);
        }
        SkyCover[] sc = skyCoverage.toArray(new SkyCover[skyCoverage.size()]);
        return sc[index];

    }

    public void sort(Set<SkyCover> skySet) {
        SortedSet<SkyCover> skSet = new TreeSet<SkyCover>();
        for (SkyCover sc : skySet) {
            skSet.add(sc);
        }

        skyCoverage = skSet;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.pointdata.IPointData#getPointDataView()
     */
    @Override
    public PointDataView getPointDataView() {
        return this.pointDataView;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointData#setPointDataView(com.raytheon
     * .uf.common.pointdata.PointDataView)
     */
    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    public static Set<String> getAvailableParameters() {
        return PARM_MAP.keySet();
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "obs";
    }
}
