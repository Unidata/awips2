package gov.noaa.nws.ncep.common.dataplugin.airep;

/**
 * This software was modified from Raytheon's pirep plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/

import java.math.RoundingMode;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Angle;
import javax.measure.quantity.Length;
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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * AirepRecord is the Data Access component for pirep observation data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2011            F. J. Yen   Initial creation from Raytheon's pirep.
 *                                     Change temp, windSpeed from Double to
 *                                     Float.  Change windDirection from Integer
 *                                     to Float
 * Aug 30, 2011 286        qzhou       Use IDecoderConstantsN.INTEGER_MISSING
 *                                     instead -9999 in visibility.
 * Aug 31, 2011 286        qzhou       Moved this from ~edex.plugin.airep
 * Sep 20, 2011 286        qzhou       Change reportType to String
 * Apr 05, 2012 420        dgilling    Prevent NullPointerExceptions in
 *                                     buildMessageData().
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 08, 2013 1293       bkowal      Removed references to hdffileid.
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Sep 05, 2013 2316       bsteffen    Unify airep and ncairep.
 * Dec 03, 2013 2551       rjpeter     Extend PersistablePluginDataObject
 * Jan 21, 2013 2724       rjpeter     Update getter/setter to use same Object as
 *                                     internal variable to prevent auto unboxing
 *                                     NPE on serialization.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "airepseq")
@Table(name = "airep", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "airep", indexes = { @Index(name = "airep_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AirepRecord extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData {

    private static final long serialVersionUID = 1L;

    public static final Unit<Temperature> TEMPERATURE_UNIT = SI.CELSIUS;

    public static final Unit<Velocity> WIND_SPEED_UNIT = NonSI.KNOT;

    public static final Unit<Angle> WIND_DIR_UNIT = NonSI.DEGREE_ANGLE;

    public static final Unit<Length> ALTITUDE_UNIT = NonSI.FOOT;

    public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

    private static UnitConverter ftToHft = NonSI.FOOT.getConverterTo(SI
            .HECTO(NonSI.FOOT));

    // private static final HashMap<Integer, String> WX_MAP = new
    // HashMap<Integer, String>();

    @Transient
    @DynamicSerializeElement
    @XmlAttribute
    private Integer obsId;

    // Time of the observation.
    @Transient
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
    @Column(length = 8)
    @DynamicSerializeElement
    @XmlAttribute
    private String reportType;

    // Text of the WMO header
    @Transient
    @DynamicSerializeElement
    @XmlElement
    private String wmoHeader;

    // Correction indicator from wmo header
    @DataURI(position = 2)
    @Column(length = 8)
    @DynamicSerializeElement
    @XmlElement
    private String corIndicator;

    // Observation air temperature in degrees Celsius.
    // Decimal(5,2)
    @Transient
    @DynamicSerializeElement
    @XmlElement
    private Float temp;

    // Observation wind direction in angular degrees. Integer
    @Transient
    @DynamicSerializeElement
    @XmlElement
    private Float windDirection;

    // Observation wind speed in knots.
    // Decimal(5,2)
    @Transient
    // @Column
    @DynamicSerializeElement
    @XmlElement
    private Float windSpeed;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private Integer flightHazard;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private Integer flightWeather;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private Integer flightConditions;

    // @Transient
    // @DynamicSerializeElement
    // @XmlElement
    // private Float latitude;
    //
    // @Transient
    // @DynamicSerializeElement
    // @XmlElement
    // private Float longitude;
    //
    // @Transient
    // @DynamicSerializeElement
    // @XmlElement
    // private String stationId;

    // @Transient
    // @DynamicSerializeElement
    // @XmlElement
    // private String dataURI;

    @Transient
    @DynamicSerializeElement
    // @XmlElement
    @XmlAttribute
    private String turbInten;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private String iceInten;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private String skyCover;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private String turbType;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private String iceType;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private String turbFreq;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private int skyBaseHeight;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private int skyTopHeight;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private String suspectTimeFlag;

    @Embedded
    @DataURI(position = 3, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private AircraftObsLocation location;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

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
        if ((messageData != null) && (messageData instanceof String)) {
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
     * Boolean for whether location is defined in the spatial tables.
     * 
     * @return true or false (Is location defined in the spatial tables?)
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
    public Float getTemp() {
        return temp;
    }

    /**
     * @param temp
     *            the temp to set
     */
    public void setTemp(Float temp) {
        this.temp = temp;
    }

    /**
     * @return the windDirection
     */
    public Float getWindDirection() {
        return windDirection;
    }

    /**
     * @param windDirection
     *            the windDirection to set
     */
    public void setWindDirection(Float windDirection) {
        this.windDirection = windDirection;
    }

    /**
     * @return the windspeed
     */
    public Float getWindSpeed() {
        return windSpeed;
    }

    /**
     * @param windspeed
     *            the windspeed to set
     */
    public void setWindSpeed(Float windSpeed) {
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

    /**
     * @return the wmoHeader
     */
    public String getTurbInten() {
        return turbInten;
    }

    public void setTurbInten(String turbInten) {
        this.turbInten = turbInten;
    }

    public String getIceInten() {
        return iceInten;
    }

    public void setIceInten(String iceInten) {
        this.iceInten = iceInten;
    }

    public String getSkyCover() {
        return skyCover;
    }

    public void setSkyCover(String skyCover) {
        this.skyCover = skyCover;
    }

    public String getTurbType() {
        return turbType;
    }

    public void setTurbType(String turbType) {
        this.turbType = turbType;
    }

    public String getIceType() {
        return iceType;
    }

    public void setIceType(String iceType) {
        this.iceType = iceType;
    }

    public String getTurbFreq() {
        return turbFreq;
    }

    public void setTurbFreq(String turbFreq) {
        this.turbFreq = turbFreq;
    }

    public int getSkyBaseHeight() {
        return skyBaseHeight;
    }

    public void setSkyBaseHeight(int skyBaseHeight) {
        this.skyBaseHeight = skyBaseHeight;
    }

    public int getSkyTopHeight() {
        return skyTopHeight;
    }

    public void setSkyTopHeight(int skyTopHeight) {
        this.skyTopHeight = skyTopHeight;
    }

    public String getSuspectTimeFlag() {
        return suspectTimeFlag;
    }

    public void setSuspectTimeFlag(String suspectTimeFlag) {
        this.suspectTimeFlag = suspectTimeFlag;
    }

    @Override
    public void setDataURI(String dataURI) {
        identifier = dataURI;
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
        if (validLocation && (getStationId() != null)) {
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

        if (validLocation && (getFlightLevel() != null)) {
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
        result = (prime * result)
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
    public void setPointDataView(PointDataView pdv) {
        this.pointDataView = pdv;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "airep";
    }
}
