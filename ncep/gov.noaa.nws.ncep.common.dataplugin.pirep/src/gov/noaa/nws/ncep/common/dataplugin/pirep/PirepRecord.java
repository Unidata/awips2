package gov.noaa.nws.ncep.common.dataplugin.pirep;

/**
 * This software was modified from Raytheon's pirep plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

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
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.vividsolutions.jts.geom.Geometry;

/**
 * PirepRecord is the Data Access component for pirep observation data.
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
 * Aug 31, 2011 286        qzhou       Created project and moved this from
 *                                     ~edex.plugin.pirep
 * Sep 19, 2011 286        Q.Zhou      Changed reportType to string,
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 08, 2013 1293       bkowal      Removed references to hdffileid.
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Sep 05, 2013 2316       bsteffen    Unify pirep and ncpirep.
 * Dec 03, 2013 2551       rjpeter     Extend PersistablePluginDataObject.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "pirepseq")
@Table(name = "pirep", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "pirep", indexes = { @Index(name = "pirep_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PirepRecord extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData {

    private static final long serialVersionUID = 1L;

    public static final Unit<Temperature> TEMPERATURE_UNIT = SI.CELSIUS;

    public static final Unit<Length> ALTITUDE_UNIT = NonSI.FOOT;

    public static final Unit<Velocity> WIND_SPEED_UNIT = NonSI.KNOT;

    public static final Unit<Angle> WIND_DIR_UNIT = NonSI.DEGREE_ANGLE;

    public static final Unit<Pressure> PRESSURE_UNIT = SI.PASCAL;

    public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

    @Transient
    private final PirepLayerData maxPirepLayerData = null;

    @Transient
    private final boolean display = true;

    @Transient
    @XmlAttribute
    @DynamicSerializeElement
    private Integer obsId;

    // Time of the observation.
    @Transient
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar timeObs;

    // Time of the observation to the nearest hour.
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Calendar refHour;

    //
    @Column(length = 8)
    @DataURI(position = 1)
    @XmlAttribute
    @DynamicSerializeElement
    private String reportType;

    // Text of the WMO header
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private String wmoHeader;

    @Transient
    @DynamicSerializeElement
    @XmlElement
    private String suspectTimeFlag;

    // Correction indicator from wmo header
    @Column(length = 8)
    @DataURI(position = 2)
    @XmlElement
    @DynamicSerializeElement
    private String corIndicator;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private String aircraftType;

    // Observation air temperature in degrees Celsius, if not converted.
    // Observation air temperature in degrees Kelvin.
    // Decimal(5,2)
    @Transient
    @XmlElement
    @DynamicSerializeElement
    // private Double temp;
    private Float temp;

    // Observation wind direction in angular degrees. Integer
    @Transient
    @XmlElement
    @DynamicSerializeElement
    // private Integer windDirection;
    private Float windDirection;

    // Observation wind speed in knots, if not converted.
    // Observation wind speed in meters per second.
    // Decimal(5,2)
    @Transient
    @XmlElement
    @DynamicSerializeElement
    // private Double windSpeed;
    private Float windSpeed;

    @Transient
    @XmlElement
    @DynamicSerializeElement
    // private Integer horzVisibility;
    private Integer horzVisibility = IDecoderConstants.VAL_MISSING; // -9999

    @Transient
    @XmlElement
    @DynamicSerializeElement
    private String obsText;

    // @Column(length = 16)
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private String weatherGroup;

    @DynamicSerializeElement
    @XmlElement
    // @OneToMany(cascade = CascadeType.ALL, mappedBy = "parent", fetch =
    // FetchType.EAGER)
    // private Set<PirepLayerData> ancPirepData;
    @Transient
    private Set<PirepLayerData> ancPirepData = new HashSet<PirepLayerData>();

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
    public PirepRecord() {
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
    public PirepRecord(String uri) {
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
     * Is the location defined in the spatial tables.
     * 
     * @return Is the location defined in the spatial tables.
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
     * Get the elevation, in feet, of the observing platform or location.
     * 
     * @return The observation elevation, in feet.
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

    public String getSuspectTimeFlag() {
        return suspectTimeFlag;
    }

    public void setSuspectTimeFlag(String suspectTimeFlag) {
        this.suspectTimeFlag = suspectTimeFlag;
    }

    /**
     * @param aircraftType
     *            the aircraftType to set
     */
    public void setAircraftType(String aircraftType) {
        this.aircraftType = aircraftType;
    }

    /**
     * @return the aircraftType
     */
    public String getAircraftType() {
        return aircraftType;
    }

    /**
     * @return the temp
     */
    // public Double getTemp() {
    public float getTemp() {
        return temp;
    }

    /**
     * @param temp
     *            the temp to set
     */
    // public void setTemp(Double temp) {
    public void setTemp(float temp) {
        this.temp = temp;
    }

    /**
     * @return the windDirection
     */
    // public Integer getWindDirection() {
    public float getWindDirection() {
        return windDirection;
    }

    /**
     * @param windDirection
     *            the windDirection to set
     */
    // public void setWindDirection(Integer windDirection) {
    public void setWindDirection(float windDirection) {
        this.windDirection = windDirection;
    }

    /**
     * @return the windspeed
     */
    // public Double getWindSpeed() {
    public float getWindSpeed() {
        return windSpeed;
    }

    /**
     * @param windspeed
     *            the windspeed to set
     */
    // public void setWindSpeed(Double windSpeed) {
    public void setWindSpeed(float windSpeed) {
        this.windSpeed = windSpeed;
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
     * @return the weatherGroup
     */
    public String getWeatherGroup() {
        return weatherGroup;
    }

    /**
     * @param weatherGroup
     *            the weatherGroup to set
     */
    public void setWeatherGroup(String weatherGroup) {
        this.weatherGroup = weatherGroup;
    }

    /**
     * @return the ancPirepData
     */
    public Set<PirepLayerData> getAncPirepData() {
        return ancPirepData;
    }

    /**
     * @param ancPirepData
     *            the ancPirepData to set
     */
    public void setAncPirepData(Set<PirepLayerData> ancPirepData) {
        this.ancPirepData = ancPirepData;
    }

    /**
     * 
     * @param cloud
     */
    public void addLayer(PirepLayerData layer) {
        layer.setParent(this);
        if (ancPirepData == null) {
            ancPirepData = new HashSet<PirepLayerData>();
        }
        ancPirepData.add(layer);
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
        return getObsText();
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
        PirepRecord other = (PirepRecord) obj;
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
        return "pirep";
    }
}
