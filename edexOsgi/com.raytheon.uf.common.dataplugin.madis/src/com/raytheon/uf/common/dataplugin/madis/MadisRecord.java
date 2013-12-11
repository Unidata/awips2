package com.raytheon.uf.common.dataplugin.madis;

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

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.measure.quantity.Angle;
import javax.measure.quantity.Dimensionless;
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
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

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

/**
 * MadisRecord Record store for MADIS point data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2013 1746       dhladky     MADIS data record creation
 * May 15, 2013 1658       djohnson    Add sequence.
 * May 16, 2013 753        dhladky     Restored dataUri as unique key
 * Jun 03, 2013 1763       dhladky     Added ValMap lookups for QCD
 * Jul 08, 2013 2171       dhladky     Removed dataURI
 * Jul 12, 2013 2096       mpduff      Changed temperature unit to F.
 * Jul 14, 2013 2180       dhladky     GUI update for mouse over display
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Oct 14, 2013 2361       njensen     Removed IDecoderGettable
 * Dec 10, 2013 2616       mpduff      Added stationId to the unique constraint
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "madisseq")
@Table(name = "madis", uniqueConstraints = { @UniqueConstraint(columnNames = {
        "location", "stationId", "refTime", "provider", "subProvider", "restriction" }) })
@org.hibernate.annotations.Table(appliesTo = "madis", indexes = { @Index(name = "madis_wfsQueryIndex", columnNames = {
        "refTime", "location" }), })
@DynamicSerialize
public class MadisRecord extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData {

    private static final long serialVersionUID = -2234739310998758367L;

    /** A string denoting the provider network */
    @DynamicSerializeElement
    @Column
    @DataURI(position = 1)
    protected String provider;

    /** A string denoting the sub provider */
    @DynamicSerializeElement
    @Column
    @DataURI(position = 2)
    private String subProvider;

    @Embedded
    @DataURI(position = 3, embedded = true)
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    /** An integer denoting the dataset */
    @DynamicSerializeElement
    @Transient
    private int dataset;

    /** An integer denoting the restriction level */
    @DynamicSerializeElement
    @Column
    @DataURI(position = 4)
    private int restriction;

    /** A string denoting the time of observation */
    @DynamicSerializeElement
    @Transient
    private Date timeObs;

    /** A float denoting the dewpoint temp */
    @DynamicSerializeElement
    @Transient
    private float dewpoint = -99999;

    /** A QCD denoting the dewpoint quality */
    @DynamicSerializeElement
    @Transient
    private QCD dewpoint_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int dewpoint_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int dewpoint_qcr = -99999;

    /** A float denoting the relative humidity */
    @DynamicSerializeElement
    @Transient
    private float rh = -99999;

    /** A string denoting the provider sub network */
    @DynamicSerializeElement
    @Transient
    private QCD rh_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int rh_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int rh_qcr = -99999;

    /** A float denoting the altimeter */
    @DynamicSerializeElement
    @Transient
    private float altimeter = -99999;

    /** A QCD denoting the altimeter quality */
    @DynamicSerializeElement
    @Transient
    private QCD altimeter_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int altimeter_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int altimeter_qcr = -99999;

    /** A float denoting the temperature */
    @DynamicSerializeElement
    @Transient
    private float temperature = -99999;

    /** A QCD denoting the temperature quality */
    @DynamicSerializeElement
    @Transient
    private QCD temperature_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int temperature_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int temperature_qcr = -99999;

    /** An int denoting the windDirection */
    @DynamicSerializeElement
    @Transient
    private int windDirection = -99999;

    /** A QCD denoting the wind Direction quality */
    @DynamicSerializeElement
    @Transient
    private QCD windDirection_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int windDirection_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int windDirection_qcr = -99999;

    /** A QCD denoting the altimeter quality */
    @DynamicSerializeElement
    @Transient
    private QCD elevation_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int elevation_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int elevation_qcr = -99999;

    /** A QCD denoting the latitude quality */
    @DynamicSerializeElement
    @Transient
    private QCD latitude_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int latitude_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int latitude_qcr = -99999;

    /** A QCD denoting the longitude quality */
    @DynamicSerializeElement
    @Transient
    private QCD longitude_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int longitude_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int longitude_qcr = -99999;

    /** A float denoting the preciprate */
    @DynamicSerializeElement
    @Transient
    private float precipRate = -99999;

    /** A QCD denoting the preciprate quality */
    @DynamicSerializeElement
    @Transient
    private QCD precipRate_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int precipRate_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int precipRate_qcr = -99999;

    /** A float denoting the wind speed */
    @DynamicSerializeElement
    @Transient
    private float windSpeed = -99999;

    /** A QCD denoting the windSpeed(wind) quality */
    @DynamicSerializeElement
    @Transient
    private QCD windSpeed_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int windSpeed_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int windSpeed_qcr = -99999;

    /** A float denoting the windSpeedgust(wind) */
    @DynamicSerializeElement
    @Transient
    private float windGust = -99999;

    /** A QCD denoting the windSpeedgust(wind) quality */
    @DynamicSerializeElement
    @Transient
    private QCD windGust_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int windGust_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int windGust_qcr = -99999;

    /** A float denoting the precipitalWater */
    @DynamicSerializeElement
    @Transient
    private float precipitalWater = -99999;

    /** A QCD denoting the precipitalWater */
    @DynamicSerializeElement
    @Transient
    private QCD precipitalWater_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int precipitalWater_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int precipitalWater_qcr = -99999;

    /** A float denoting the pressure */
    @DynamicSerializeElement
    @Transient
    private float pressure = -99999;

    /** A QCD denoting the precipitalWater */
    @DynamicSerializeElement
    @Transient
    private QCD pressure_qcd = QCD.MISSING;

    @DynamicSerializeElement
    @Transient
    private int pressure_qca = -99999;

    @DynamicSerializeElement
    @Transient
    private int pressure_qcr = -99999;

    @DynamicSerializeElement
    @Embedded
    private PointDataView pointDataView;

    public static final String PLUGIN_NAME = "madis";

    public static final String STATION_ID = "stationId";

    public static final Unit<Temperature> TEMPERATURE_UNIT = NonSI.FAHRENHEIT;

    public static final Unit<Dimensionless> HUMIDITY_UNIT = NonSI.PERCENT;

    public static final Unit<Velocity> WIND_SPEED_UNIT = NonSI.KNOT;

    public static final Unit<Length> HEIGHT_UNIT = SI.METER;

    public static final Unit<Angle> WIND_DIR_UNIT = NonSI.DEGREE_ANGLE;

    public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

    public static final Unit<Pressure> PRESSURE_UNIT = SI.HECTO(SI.PASCAL);

    public static final Unit<Pressure> ALTIMETER_UNIT = SI.PASCAL;

    public static final Unit<Length> PRECIP_UNIT = NonSI.INCH;

    /** MADIS specific parameter keys */
    public static final class ParameterKey {

        public static final String RH = "RH";

        public static final String RH_QCA = "RH_QCA";

        public static final String RH_QCR = "RH_QCR";

        public static final String RH_QCD = "RH_QCD";

        public static final String TEMPERATURE_QCD = "TEMPERATURE_QCD";

        public static final String TEMPERATURE_QCA = "TEMPERATURE_QCA";

        public static final String TEMPERATURE_QCR = "TEMPERATURE_QCR";

        public static final String DEWPOINT_QCD = "DEWPOINT_QCD";

        public static final String DEWPOINT_QCA = "DEWPOINT_QCA";

        public static final String DEWPOINT_QCR = "DEWPOINT_QCR";

        public static final String WINDDIRECTION_QCD = "WINDDIRECTION_QCD";

        public static final String WINDDIRECTION_QCA = "WINDDIRECTION_QCA";

        public static final String WINDDIRECTION_QCR = "WINDDIRECTION_QCR";

        public static final String WINDSPEED_QCD = "WINDSPEED_QCD";

        public static final String WINDSPEED_QCA = "WINDSPEED_QCA";

        public static final String WINDSPEED_QCR = "WINDSPEED_QCR";

        public static final String WINDGUST_QCD = "WINDGUST_QCD";

        public static final String WINDGUST_QCA = "WINDGUST_QCA";

        public static final String WINDGUST_QCR = "WINDGUST_QCR";

        public static final String PRECIPRATE = "PCPRATE";

        public static final String PRECIPRATE_QCA = "PCPRATE_QCA";

        public static final String PRECIPRATE_QCR = "PCPRATE_QCR";

        public static final String PRECIPRATE_QCD = "PCPRATE_QCD";

        public static final String PRECIPITALWATER = "PWV";

        public static final String PRECIPITALWATER_QCA = "PWV_QCA";

        public static final String PRECIPITALWATER_QCR = "PWV_QCR";

        public static final String PRECIPITALWATER_QCD = "PWV_QCD";

        public static final String PRESSURE = "P";

        public static final String PRESSURE_QCA = "P_QCA";

        public static final String PRESSURE_QCR = "P_QCR";

        public static final String PRESSURE_QCD = "P_QCD";

        public static final String RELATIVEHUMIDITY = "RH";

        public static final String RELATIVEHUMIDITY_QCA = "RH_QCA";

        public static final String RELATIVEHUMIDITY_QCR = "RH_QCR";

        public static final String RELATIVEHUMIDITY_QCD = "RH_QCD";

        public static final String PROVIDER = "PROVIDER";

        public static final String SUB_PROVIDER = "SUB_PROVIDER";

        public static final String STATIONID = "STATIONID";

        public static final String RESTRICTION = "RESTRICTION";
    }

    /**
     * Get the pointdata view
     */
    @Override
    public PointDataView getPointDataView() {
        return this.pointDataView;
    }

    /**
     * Set the pointdata view
     */
    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    /**
     * Enumeration of Quality types, we know so far
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Mar 16, 2013            dhladky     Initial creation
     * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */
    @XmlEnum
    public enum QCD {
        // C - Coarse pass, passed level 1
        // S - Screened, passed levels 1 and 2
        // V - Verified, passed levels 1, 2, and 3
        // X - Rejected/erroneous, failed level 1
        // Q - Questioned, passed level 1, failed 2 or 3
        // G - Subjective good
        // B - Subjective bad
        @XmlEnumValue(QCD.V)
        VERIFIED("V"), @XmlEnumValue(QCD.S)
        SCREENED("S"), @XmlEnumValue(QCD.Q)
        QUESTIONED("Q"), @XmlEnumValue(QCD.B)
        BAD("B"), @XmlEnumValue(QCD.C)
        COARSEPASS("C"), @XmlEnumValue(QCD.G)
        GOOD("G"), @XmlEnumValue(QCD.Z)
        MISSING("Z"), @XmlEnumValue(QCD.X)
        REJECTED("X");

        private static final String V = "V";

        private static final String S = "S";

        private static final String Q = "Q";

        private static final String C = "C";

        private static final String B = "B";

        private static final String G = "G";

        private static final String Z = "Z";

        private static final String X = "X";

        private static final Map<String, QCD> qcdMap;

        private static final Map<String, QCD> valMap;

        static {
            Map<String, QCD> map = new HashMap<String, QCD>();
            map.put(V, QCD.VERIFIED);
            map.put(S, QCD.SCREENED);
            map.put(Q, QCD.QUESTIONED);
            map.put(C, QCD.COARSEPASS);
            map.put(B, QCD.BAD);
            map.put(G, QCD.GOOD);
            map.put(Z, QCD.MISSING);
            map.put(X, QCD.REJECTED);
            qcdMap = Collections.unmodifiableMap(map);

            Map<String, QCD> map2 = new HashMap<String, QCD>();
            map2.put(QCD.VERIFIED.name(), QCD.VERIFIED);
            map2.put(QCD.SCREENED.name(), QCD.SCREENED);
            map2.put(QCD.QUESTIONED.name(), QCD.QUESTIONED);
            map2.put(QCD.COARSEPASS.name(), QCD.COARSEPASS);
            map2.put(QCD.BAD.name(), QCD.BAD);
            map2.put(QCD.GOOD.name(), QCD.GOOD);
            map2.put(QCD.MISSING.name(), QCD.MISSING);
            map2.put(QCD.REJECTED.name(), QCD.REJECTED);
            valMap = Collections.unmodifiableMap(map2);
        }

        private final String qcd;

        private QCD(String name) {
            qcd = name;
        }

        @Override
        public String toString() {
            return qcd;
        }

        public static QCD fromString(String val) {
            return qcdMap.get(val);
        }

        public static QCD fromVal(String val) {
            return valMap.get(val);
        }

    }

    public String getProvider() {
        return provider;
    }

    public void setProvider(String provider) {
        this.provider = provider;
    }

    public String getSubProvider() {
        return subProvider;
    }

    public void setSubProvider(String subProvider) {
        this.subProvider = subProvider;
    }

    public float getDewpoint() {
        return dewpoint;
    }

    public void setDewpoint(float dewpoint) {
        this.dewpoint = dewpoint;
    }

    public QCD getDewpoint_qcd() {
        return dewpoint_qcd;
    }

    public void setDewpoint_qcd(QCD dewpoint_qcd) {
        this.dewpoint_qcd = dewpoint_qcd;
    }

    public float getRh() {
        return rh;
    }

    public void setRh(float rh) {
        this.rh = rh;
    }

    public QCD getRh_qcd() {
        return rh_qcd;
    }

    public void setRh_qcd(QCD rh_qcd) {
        this.rh_qcd = rh_qcd;
    }

    public float getAltimeter() {
        return altimeter;
    }

    public void setAltimeter(float altimeter) {
        this.altimeter = altimeter;
    }

    public QCD getAltimeter_qcd() {
        return altimeter_qcd;
    }

    public void setAltimeter_qcd(QCD altimeter_qcd) {
        this.altimeter_qcd = altimeter_qcd;
    }

    public float getTemperature() {
        return temperature;
    }

    public void setTemperature(float temperature) {
        this.temperature = temperature;
    }

    public QCD getTemperature_qcd() {
        return temperature_qcd;
    }

    public void setTemperature_qcd(QCD temperature_qcd) {
        this.temperature_qcd = temperature_qcd;
    }

    public int getWindDirection() {
        return windDirection;
    }

    public void setWindDirection(int windDirection) {
        this.windDirection = windDirection;
    }

    public QCD getWindDirection_qcd() {
        return windDirection_qcd;
    }

    public void setWindDirection_qcd(QCD windDirection_qcd) {
        this.windDirection_qcd = windDirection_qcd;
    }

    public float getPrecipRate() {
        return precipRate;
    }

    public void setPrecipRate(float precipRate) {
        this.precipRate = precipRate;
    }

    public QCD getPrecipRate_qcd() {
        return precipRate_qcd;
    }

    public void setPrecipRate_qcd(QCD precipRate_qcd) {
        this.precipRate_qcd = precipRate_qcd;
    }

    public float getWindSpeed() {
        return windSpeed;
    }

    public void setWindSpeed(float windSpeed) {
        this.windSpeed = windSpeed;
    }

    public QCD getWindSpeed_qcd() {
        return windSpeed_qcd;
    }

    public void setWindSpeed_qcd(QCD windSpeed_qcd) {
        this.windSpeed_qcd = windSpeed_qcd;
    }

    public float getWindGust() {
        return windGust;
    }

    public void setWindGust(float windGust) {
        this.windGust = windGust;
    }

    public QCD getWindGust_qcd() {
        return windGust_qcd;
    }

    public void setWindGust_qcd(QCD windGust_qcd) {
        this.windGust_qcd = windGust_qcd;
    }

    public float getPrecipitalWater() {
        return precipitalWater;
    }

    public void setPrecipitalWater(float precipitalWater) {
        this.precipitalWater = precipitalWater;
    }

    public QCD getPrecipitalWater_qcd() {
        return precipitalWater_qcd;
    }

    public void setPrecipitalWater_qcd(QCD precipitalWater_qcd) {
        this.precipitalWater_qcd = precipitalWater_qcd;
    }

    public float getPressure() {
        return pressure;
    }

    public void setPressure(float pressure) {
        this.pressure = pressure;
    }

    public QCD getPressure_qcd() {
        return pressure_qcd;
    }

    public void setPressure_qcd(QCD pressure_qcd) {
        this.pressure_qcd = pressure_qcd;
    }

    public int getDewpoint_qca() {
        return dewpoint_qca;
    }

    public void setDewpoint_qca(int dewpoint_qca) {
        this.dewpoint_qca = dewpoint_qca;
    }

    public int getDewpoint_qcr() {
        return dewpoint_qcr;
    }

    public void setDewpoint_qcr(int dewpoint_qcr) {
        this.dewpoint_qcr = dewpoint_qcr;
    }

    public int getRh_qca() {
        return rh_qca;
    }

    public void setRh_qca(int rh_qca) {
        this.rh_qca = rh_qca;
    }

    public int getRh_qcr() {
        return rh_qcr;
    }

    public void setRh_qcr(int rh_qcr) {
        this.rh_qcr = rh_qcr;
    }

    public int getAltimeter_qca() {
        return altimeter_qca;
    }

    public void setAltimeter_qca(int altimeter_qca) {
        this.altimeter_qca = altimeter_qca;
    }

    public int getAltimeter_qcr() {
        return altimeter_qcr;
    }

    public void setAltimeter_qcr(int altimeter_qcr) {
        this.altimeter_qcr = altimeter_qcr;
    }

    public int getTemperature_qca() {
        return temperature_qca;
    }

    public void setTemperature_qca(int temperature_qca) {
        this.temperature_qca = temperature_qca;
    }

    public int getTemperature_qcr() {
        return temperature_qcr;
    }

    public void setTemperature_qcr(int temperature_qcr) {
        this.temperature_qcr = temperature_qcr;
    }

    public int getWindDirection_qca() {
        return windDirection_qca;
    }

    public void setWindDirection_qca(int windDirection_qca) {
        this.windDirection_qca = windDirection_qca;
    }

    public int getWindDirection_qcr() {
        return windDirection_qcr;
    }

    public void setWindDirection_qcr(int windDirection_qcr) {
        this.windDirection_qcr = windDirection_qcr;
    }

    public int getPrecipRate_qca() {
        return precipRate_qca;
    }

    public void setPrecipRate_qca(int precipRate_qca) {
        this.precipRate_qca = precipRate_qca;
    }

    public int getPrecipRate_qcr() {
        return precipRate_qcr;
    }

    public void setPrecipRate_qcr(int precipRate_qcr) {
        this.precipRate_qcr = precipRate_qcr;
    }

    public int getWindSpeed_qca() {
        return windSpeed_qca;
    }

    public void setWindSpeed_qca(int windSpeed_qca) {
        this.windSpeed_qca = windSpeed_qca;
    }

    public int getWindSpeed_qcr() {
        return windSpeed_qcr;
    }

    public void setWindSpeed_qcr(int windSpeed_qcr) {
        this.windSpeed_qcr = windSpeed_qcr;
    }

    public int getWindGust_qca() {
        return windGust_qca;
    }

    public void setWindGust_qca(int windGust_qca) {
        this.windGust_qca = windGust_qca;
    }

    public int getWindGust_qcr() {
        return windGust_qcr;
    }

    public void setWindGust_qcr(int windGust_qcr) {
        this.windGust_qcr = windGust_qcr;
    }

    public int getPrecipitalWater_qca() {
        return precipitalWater_qca;
    }

    public void setPrecipitalWater_qca(int precipitalWater_qca) {
        this.precipitalWater_qca = precipitalWater_qca;
    }

    public int getPrecipitalWater_qcr() {
        return precipitalWater_qcr;
    }

    public void setPrecipitalWater_qcr(int precipitalWater_qcr) {
        this.precipitalWater_qcr = precipitalWater_qcr;
    }

    public int getPressure_qca() {
        return pressure_qca;
    }

    public void setPressure_qca(int pressure_qca) {
        this.pressure_qca = pressure_qca;
    }

    public int getPressure_qcr() {
        return pressure_qcr;
    }

    public void setPressure_qcr(int pressure_qcr) {
        this.pressure_qcr = pressure_qcr;
    }

    public void setTimeObs(Date timeObs) {
        this.timeObs = timeObs;
    }

    public Date getTimeObs() {
        return timeObs;
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

    public void setLongitude_qcr(int longitude_qcr) {
        this.longitude_qcr = longitude_qcr;
    }

    public int getLongitude_qcr() {
        return longitude_qcr;
    }

    public void setLongitude_qca(int longitude_qca) {
        this.longitude_qca = longitude_qca;
    }

    public int getLongitude_qca() {
        return longitude_qca;
    }

    public void setLongitude_qcd(QCD longitude_qcd) {
        this.longitude_qcd = longitude_qcd;
    }

    public QCD getLongitude_qcd() {
        return longitude_qcd;
    }

    public void setLatitude_qcr(int latitude_qcr) {
        this.latitude_qcr = latitude_qcr;
    }

    public int getLatitude_qcr() {
        return latitude_qcr;
    }

    public void setLatitude_qca(int latitude_qca) {
        this.latitude_qca = latitude_qca;
    }

    public int getLatitude_qca() {
        return latitude_qca;
    }

    public void setLatitude_qcd(QCD latitude_qcd) {
        this.latitude_qcd = latitude_qcd;
    }

    public QCD getLatitude_qcd() {
        return latitude_qcd;
    }

    public void setElevation_qcr(int elevation_qcr) {
        this.elevation_qcr = elevation_qcr;
    }

    public int getElevation_qcr() {
        return elevation_qcr;
    }

    public void setElevation_qca(int elevation_qca) {
        this.elevation_qca = elevation_qca;
    }

    public int getElevation_qca() {
        return elevation_qca;
    }

    public void setElevation_qcd(QCD elevation_qcd) {
        this.elevation_qcd = elevation_qcd;
    }

    public QCD getElevation_qcd() {
        return elevation_qcd;
    }

    public void setDataset(int dataset) {
        this.dataset = dataset;
    }

    public int getDataset() {
        return dataset;
    }

    public void setRestriction(int restriction) {
        this.restriction = restriction;
    }

    public int getRestriction() {
        return restriction;
    }

    /**
     * Get the station identifier for this observation.
     * 
     * @return the stationId
     */
    public String getStationId() {
        return location.getStationId();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result)
                + ((getDataURI() == null) ? 0 : getDataURI().hashCode());
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
        MadisRecord other = (MadisRecord) obj;
        if (getDataURI() == null) {
            if (other.getDataURI() != null) {
                return false;
            }
        } else if (!getDataURI().equals(other.getDataURI())) {
            return false;
        }
        return true;
    }

    @Override
    public String getPluginName() {
        return PLUGIN_NAME;
    }
}
