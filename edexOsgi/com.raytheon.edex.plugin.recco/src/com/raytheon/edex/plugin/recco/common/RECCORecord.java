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
package com.raytheon.edex.plugin.recco.common;

import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;

import javax.measure.quantity.Angle;
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
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080103            384 jkorman     Initial Coding.
 * 20080107            720 jkorman     remove default assignments from attributes.
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "reccoseq")
@Table(name = "recco", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "recco",
		indexes = {
				@Index(name = "recco_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class RECCORecord extends PluginDataObject implements ISpatialEnabled,
        IDecoderGettable {

    private static final long serialVersionUID = 1L;

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
        PARM_MAP.put("Px", PRES_STATION);
        PARM_MAP.put("PMSL", PRES_SLP);
        PARM_MAP.put("ASET", PRES_ALTSG);
        PARM_MAP.put("NLAT", STA_LAT);
        PARM_MAP.put("NLON", STA_LON);
    }

    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private Integer obsId;

    // Time of the observation.
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar timeObs;

    // Time of the observation to the nearest hour.
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar refHour;

    // 
    @Column
    @DataURI(position = 1)
    @DynamicSerializeElement
    @XmlAttribute
    private Integer reportType;

    // Text of the WMO header
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String wmoHeader;

    // Correction indicator from wmo header
    @Column
    @DataURI(position = 2)
    @DynamicSerializeElement
    @XmlElement
    private String corIndicator;

    // Observation air temperature in degrees Kelvin.
    // Decimal(5,2)
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double temp;

    // Observation dewpoint temperature in degrees Kelvin.
    // Decimal(5,2)
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double dwpt;

    // Relative Humidity in percent. Decimal(5,2)
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double humidity;

    // sea surface temperature.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double seaTemp;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double wetBulb;

    // Observation wind direction in angular degrees. Integer
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer windDirection;

    // Observation wind speed in meters per second.
    // Decimal(5,2)
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double windSpeed;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer wx_past_1;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer wx_past_2;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer wx_present;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer wx_report_type;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer horzVisibility;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer totalCloudCover;

    @Embedded
    @DataURI(position = 3, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private AircraftObsLocation location;

    /**
     * 
     */
    public RECCORecord() {
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
    public RECCORecord(String uri) {
        super(uri);
    }

    public Integer getObsId() {
        return obsId;
    }

    public void setObsId(Integer obsId) {
        this.obsId = obsId;
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
     * Get the elevation, in meters, of the observing platform or location.
     * 
     * @return The observation elevation, in meters.
     */
    public Boolean getLocationDefined() {
        return location.getLocationDefined();
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
    public Integer getFlightLevel() {
        return location.getFlightLevel();
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
     * Get the observation dewpoint temperature in degrees Kelvin.
     * 
     * @return The dewpoint temperature in degrees Kelvin.
     */
    public Double getDwpt() {
        return dwpt;
    }

    /**
     * Set the observation dewpoint temperature in degrees Kelvin.
     * 
     * @param dwpt
     *            The observation dewpoint temperature in degrees Kelvin.
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
     * @return the windspeed
     */
    public Double getWindSpeed() {
        return windSpeed;
    }

    /**
     * @param windspeed
     *            the windspeed to set
     */
    public void setWindSpeed(Double windSpeed) {
        this.windSpeed = windSpeed;
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
        if ("WX".matches(paramName)) {
            return this.getWx_present().toString();
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

        if (SFC_TEMP.equals(pName) && (temp != null)) {
            a = new Amount(temp, TEMPERATURE_UNIT);
        } else if (SFC_DWPT.equals(pName) && (dwpt != null)) {
            a = new Amount(dwpt, TEMPERATURE_UNIT);
        } else if (SFC_WNDSPD.equals(pName) && (windSpeed != null)) {
            a = new Amount(windSpeed, WIND_SPEED_UNIT);
        } else if (SFC_WNDDIR.equals(pName) && (windDirection != null)) {
            a = new Amount(windDirection, WIND_DIR_UNIT);
        } else if (STA_LAT.equals(pName)) {
            a = new Amount(this.getLatitude(), LOCATION_UNIT);
        } else if (STA_LON.equals(pName)) {
            a = new Amount(this.getLongitude(), LOCATION_UNIT);
        }
        return a;
    }

    public String[] getStrings(String paramName) {
        return null;
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

    public AircraftObsLocation getLocation() {
        return location;
    }

    public void setLocation(AircraftObsLocation location) {
        this.location = location;
    }

    @Override
    public AircraftObsLocation getSpatialObject() {
        return location;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}