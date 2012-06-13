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
package com.raytheon.uf.common.dataplugin.airep;

import java.math.RoundingMode;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;

import javax.measure.converter.UnitConverter;
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
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

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
 * 20120405            435 dgilling    Prevent NullPointerExceptions in
 *                                     buildMessageData().
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@Table(name = "airep", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AirepRecord extends PluginDataObject implements ISpatialEnabled,
        IDecoderGettable {

    private static final long serialVersionUID = 1L;

    public static final Unit<Temperature> TEMPERATURE_UNIT = SI.CELSIUS;

    public static final Unit<Velocity> WIND_SPEED_UNIT = NonSI.KNOT;

    public static final Unit<Angle> WIND_DIR_UNIT = NonSI.DEGREE_ANGLE;

    public static final Unit<Length> ALTITUDE_UNIT = NonSI.FOOT;

    public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

    private static UnitConverter ftToHft = NonSI.FOOT.getConverterTo(SI
            .HECTO(NonSI.FOOT));

    private static final HashMap<String, String> PARM_MAP = new HashMap<String, String>();

    // private static final HashMap<Integer, String> WX_MAP = new
    // HashMap<Integer, String>();

    static {
        PARM_MAP.put("T", SFC_TEMP);
        PARM_MAP.put("WS", SFC_WNDSPD);
        PARM_MAP.put("WD", SFC_WNDDIR);
        PARM_MAP.put("NLAT", STA_LAT);
        PARM_MAP.put("NLON", STA_LON);
        PARM_MAP.put("FLT_LVL", UA_FLTLVL);

        // WX_MAP.put(0, "CLR");
        // WX_MAP.put(1, "SCT");
        // WX_MAP.put(2, "BKN");
        // WX_MAP.put(3, "CONT");
        // WX_MAP.put(4, "LIGHTNING");
        // WX_MAP.put(5, "DZRA");
        // WX_MAP.put(6, "CONT RA");
        // WX_MAP.put(7, "CONT SN");
        // WX_MAP.put(8, "SH");
        // WX_MAP.put(9, "TSRA");
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
    @DataURI(position = 1)
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private Integer reportType;

    // Text of the WMO header
    @Column(length = 32)
    @DynamicSerializeElement
    @XmlElement
    private String wmoHeader;

    // Correction indicator from wmo header
    @DataURI(position = 2)
    @Column(length = 8)
    @DynamicSerializeElement
    @XmlElement
    private String corIndicator;

    // Observation air temperature in degrees Kelvin.
    // Decimal(5,2)
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double temp;

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
    private Integer flightHazard;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer flightWeather;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer flightConditions;

    @Embedded
    @DataURI(position = 3, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private AircraftObsLocation location;

    /**
     * 
     */
    public AirepRecord() {
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
    public AirepRecord(String uri) {
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
        if (messageData != null && messageData instanceof String) {
            s = (String) messageData;
        } else {
            s = buildMessageData();
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
     * @return the flightHazard
     */
    public Integer getFlightHazard() {
        return flightHazard;
    }

    /**
     * @param flightHazard
     *            the wx_past_1 to set
     */
    public void setFlightHazard(Integer flightHazard) {
        this.flightHazard = flightHazard;
    }

    /**
     * @return the flightWeather
     */
    public Integer getFlightWeather() {
        return flightWeather;
    }

    /**
     * @param flightWeather
     *            the getFlightWeather to set
     */
    public void setFlightWeather(Integer flightWeather) {
        this.flightWeather = flightWeather;
    }

    /**
     * @return the flightConditions
     */
    public Integer getFlightConditions() {
        return flightConditions;
    }

    /**
     * @param flightConditions
     *            the flightConditions to set
     */
    public void setFlightConditions(Integer flightConditions) {
        this.flightConditions = flightConditions;
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
        } else if (SFC_WNDSPD.equals(pName) && (windSpeed != null)) {
            a = new Amount(windSpeed, WIND_SPEED_UNIT);
        } else if (SFC_WNDDIR.equals(pName) && (windDirection != null)) {
            a = new Amount(windDirection, WIND_DIR_UNIT);
        } else if (STA_LAT.equals(pName)) {
            a = new Amount(this.getLatitude(), LOCATION_UNIT);
        } else if (STA_LON.equals(pName)) {
            a = new Amount(this.getLongitude(), LOCATION_UNIT);
        } else if (UA_FLTLVL.equals(pName) && getFlightLevel() != null) {
            a = new Amount(this.getFlightLevel().intValue(), ALTITUDE_UNIT);

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

    @Override
    public String[] getStrings(String paramName) {
        if ("FLT_HZD".matches(paramName) && flightHazard != null) {
            String[] flightHazards = { flightHazard.toString() };
            return flightHazards;
        }
        return null;
    }

    @Override
    public AircraftObsLocation getSpatialObject() {
        return location;
    }

    public AircraftObsLocation getLocation() {
        return location;
    }

    public void setLocation(AircraftObsLocation location) {
        this.location = location;
    }

    @Override
    public String getMessageData() {
        return getReportData();
    }

    private String buildMessageData() {
        boolean validLocation = (location != null);

        StringBuilder messageData = new StringBuilder("ARP ");
        if (validLocation && getStationId() != null) {
            messageData.append(getStationId());
        }
        messageData.append(' ');

        if ((validLocation) && (!Double.isNaN(getLatitude()))
                && (!Double.isNaN(getLongitude()))) {
            messageData.append(formatLatLon(getLatitude(), true));
            messageData.append(' ');
            messageData.append(formatLatLon(getLongitude(), false));
            messageData.append(' ');
        }

        if (timeObs != null) {
            DateFormat df = new SimpleDateFormat("HHmm");
            messageData.append(df.format(timeObs.getTime()));
        }
        messageData.append(" F");

        if (validLocation && getFlightLevel() != null) {
            int flightLevel = (int) ftToHft.convert(getFlightLevel());
            messageData.append(flightLevel);
        }
        messageData.append(' ');

        if (temp != null) {
            if (temp > 0) {
                messageData.append('P');
            } else {
                messageData.append('M');
            }
            messageData.append(Math.abs(temp.intValue()));
        }
        messageData.append(' ');

        if ((windDirection != null) && (windSpeed != null)) {
            messageData.append(windDirection.intValue());
            messageData.append('/');
            messageData.append(windSpeed.intValue());
            messageData.append("KT");
        }
        messageData.append("TB");

        return messageData.toString();
    }

    private String formatLatLon(double value, boolean isLatitude) {
        char dir;
        if (isLatitude) {
            if (value > 0) {
                dir = 'N';
            } else {
                dir = 'S';
            }
        } else {
            if (value > 0) {
                dir = 'E';
            } else {
                dir = 'W';
            }
        }

        DecimalFormat df = new DecimalFormat("###.000");
        df.setRoundingMode(RoundingMode.DOWN);

        return df.format(Math.abs(value)) + dir;
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
        AirepRecord other = (AirepRecord) obj;
        if (getDataURI() == null) {
            if (other.getDataURI() != null) {
                return false;
            }
        } else if (!getDataURI().equals(other.getDataURI())) {
            return false;
        }
        return true;
    }

}