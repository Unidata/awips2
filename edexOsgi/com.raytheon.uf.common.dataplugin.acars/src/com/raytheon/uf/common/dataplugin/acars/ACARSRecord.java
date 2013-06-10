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
package com.raytheon.uf.common.dataplugin.acars;

import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;

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

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2009       1939 jkorman     Initial creation
 * Apr 09, 2009        952 jsanchez    Updated getValue method.
 *                                      Added a getMessageData method.
 * Apr 21, 2009       2245 jsanchez    Returned temperature unit to kelvin.
 * May 21, 2009       2338 jsanchez    Updated the getMessageData.
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
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "acarsseq")
@Table(name = "acars", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "acars",
		indexes = {
				@Index(name = "acars_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ACARSRecord extends PluginDataObject implements ISpatialEnabled,
        IDecoderGettable, Comparable<ACARSRecord> {

    private static final long serialVersionUID = 1L;

    private static final String TEXT_FMT = "%1$s %2$td/%2$tH%2$tM";

    public static final Unit<Length> FLIGHT_LEVEL_UNIT = SI.METER;

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
        PARM_MAP.put("NLAT", STA_LAT);
        PARM_MAP.put("NLON", STA_LON);

        PARM_MAP.put("altitude", UA_FLTLVL);
        PARM_MAP.put("turbc", "turbc");
    }

    public static final String TAIL_NUM = "tailNumber";

    public static final String FLIGHT_NUM = "tailNumber";

    // Time of the observation.
    @Column
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar timeObs;

    // Text of the WMO header
    @Column(length = 32)
    @DynamicSerializeElement
    @XmlElement
    private String wmoHeader;

    @DataURI(position = 1)
    @Column(length = 32)
    @DynamicSerializeElement
    @XmlElement
    private String tailNumber;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private AircraftObsLocation location;

    @Column(length = 5)
    @DynamicSerializeElement
    @XmlElement
    private String receiver;

    // Flight phase
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer flightPhase;

    // Observation air temperature in degrees Kelvin.
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

    // Pressure in Pascals.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double pressure;

    // Relative Humidity in percent.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double humidity;

    // Mixing Ratio in Kg/Kg.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double mixingRatio;

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

    // Base height of reported icing.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer iceBaseHgt;

    // Top height of reported icing.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer iceTopHgt;

    // Intensity of reported icing.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer icing;

    // Base height of reported turbulence.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer turbBaseHgt;

    // Top height of reported turbulence.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer turbTopHgt;

    // Intensity of reported turbulence.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer turbulence;

    // Aircraft roll angle quality
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Integer rollAngleQuality;

    @Transient
    private boolean usedInSounding = false;

    /**
     * Create an empty ACARSRecord.
     */
    public ACARSRecord() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A data uri applicable to this class.
     */
    public ACARSRecord(String uri) {
        super(uri);
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
     * @return the tailNumber
     */
    public String getTailNumber() {
        return tailNumber;
    }

    /**
     * @param tailNumber
     *            the tailNumber to set
     */
    public void setTailNumber(String tailNumber) {
        this.tailNumber = tailNumber;
    }

    /**
     * 
     */
    @Override
    public AircraftObsLocation getSpatialObject() {
        return location;
    }

    /**
     * 
     * @return
     */
    public AircraftObsLocation getLocation() {
        return location;
    }

    /**
     * 
     * @param location
     */
    public void setLocation(AircraftObsLocation location) {
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
     * Get the elevation, in meters, of the observing platform or location.
     * 
     * @return The observation elevation, in meters.
     */
    public Boolean getLocationDefined() {
        return location.getLocationDefined();
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
     * 
     * @return
     */
    public String getFlightNumber() {
        return location.getStationId();
    }

    /**
     * Get the receiving station.
     * 
     * @return the receiver
     */
    public String getReceiver() {
        return receiver;
    }

    /**
     * Set the receiving station.
     * 
     * @param receiver
     *            the receiver to set
     */
    public void setReceiver(String receiver) {
        this.receiver = receiver;
    }

    /**
     * @return the flightPhase
     */
    public Integer getFlightPhase() {
        return flightPhase;
    }

    /**
     * @param flightPhase
     *            the flightPhase to set
     */
    public void setFlightPhase(Integer flightPhase) {
        this.flightPhase = flightPhase;
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
     * @return the pressure
     */
    public Double getPressure() {
        return pressure;
    }

    /**
     * @param pressure
     *            the pressure to set
     */
    public void setPressure(Double pressure) {
        this.pressure = pressure;
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
     * @return the mixingRatio
     */
    public Double getMixingRatio() {
        return mixingRatio;
    }

    /**
     * @param mixingRatio
     *            the mixingRatio to set
     */
    public void setMixingRatio(Double mixingRatio) {
        this.mixingRatio = mixingRatio;
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
     * @return the iceBaseHgt
     */
    public Integer getIceBaseHgt() {
        return iceBaseHgt;
    }

    /**
     * @param iceBaseHgt
     *            the iceBaseHgt to set
     */
    public void setIceBaseHgt(Integer iceBaseHgt) {
        this.iceBaseHgt = iceBaseHgt;
    }

    /**
     * @return the iceTopHgt
     */
    public Integer getIceTopHgt() {
        return iceTopHgt;
    }

    /**
     * @param iceTopHgt
     *            the iceTopHgt to set
     */
    public void setIceTopHgt(Integer iceTopHgt) {
        this.iceTopHgt = iceTopHgt;
    }

    /**
     * @return the icing
     */
    public Integer getIcing() {
        return icing;
    }

    /**
     * @param icing
     *            the icing to set
     */
    public void setIcing(Integer icing) {
        this.icing = icing;
    }

    /**
     * @return the turbBaseHgt
     */
    public Integer getTurbBaseHgt() {
        return turbBaseHgt;
    }

    /**
     * @param turbBaseHgt
     *            the turbBaseHgt to set
     */
    public void setTurbBaseHgt(Integer turbBaseHgt) {
        this.turbBaseHgt = turbBaseHgt;
    }

    /**
     * @return the turbTopHgt
     */
    public Integer getTurbTopHgt() {
        return turbTopHgt;
    }

    /**
     * @param turbTopHgt
     *            the turbTopHgt to set
     */
    public void setTurbTopHgt(Integer turbTopHgt) {
        this.turbTopHgt = turbTopHgt;
    }

    /**
     * @return the turbulence
     */
    public Integer getTurbulence() {
        return turbulence;
    }

    /**
     * @param turbulence
     *            the turbulence to set
     */
    public void setTurbulence(Integer turbulence) {
        this.turbulence = turbulence;
    }

    /**
     * 
     * @return the rollAngleQuality
     */
    public Integer getRollAngleQuality() {
        return rollAngleQuality;
    }

    /**
     * 
     * @param rollAngleQuality
     *            the rollAngleQuality to set
     */
    public void setRollAngleQuality(Integer rollAngleQuality) {
        this.rollAngleQuality = rollAngleQuality;
    }

    /**
     * @return the usedInSounding
     */
    public boolean isUsedInSounding() {
        return usedInSounding;
    }

    /**
     * @param usedInSounding
     *            the usedInSounding to set
     */
    public void setUsedInSounding(boolean usedInSounding) {
        this.usedInSounding = usedInSounding;
    }

    /**
     * 
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return this;
    }

    @Override
    public String getString(String paramName) {
        String retValue = null;
        if (TAIL_NUM.equals(paramName)) {
            retValue = getTailNumber();
        } else if (FLIGHT_NUM.equals(paramName)) {
            retValue = getFlightNumber();
        } else if ("TEXT".equals(paramName)) {
            retValue = String.format(TEXT_FMT, getTailNumber(), getTimeObs());
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

        if (SFC_TEMP.equals(pName) && (temp != null)) {
            a = new Amount(temp, TEMPERATURE_UNIT);
        } else if (SFC_WNDSPD.equals(pName) && (windSpeed != null)) {
            a = new Amount(windSpeed, WIND_SPEED_UNIT);
        } else if (SFC_WNDDIR.equals(pName) && (windDirection != null)) {
            a = new Amount(windDirection, WIND_DIR_UNIT);
        } else if (STA_LAT.equals(pName)) {
            a = new Amount(getLatitude(), LOCATION_UNIT);
        } else if (STA_LON.equals(pName)) {
            a = new Amount(getLongitude(), LOCATION_UNIT);
        } else if (UA_FLTLVL.equals(pName) && getFlightLevel() != null) {
            a = new Amount(getFlightLevel().intValue(), FLIGHT_LEVEL_UNIT);
        } else if (SFC_DWPT.equals(pName) && getDwpt() != null) {
            a = new Amount(getDwpt(), TEMPERATURE_UNIT);
        }

        return a;
    }

    @Override
    public Collection<Amount> getValues(String paramName) {
        return null;
    }

    public static final Double calcHumidity(Double temp, Double dwpt) {
        Double humidity = null;
        if ((temp != null) && (dwpt != null)) {
        }
        return humidity;
    }

    @Override
    public String getMessageData() {
        return String.format(TEXT_FMT, getTailNumber(), getTimeObs());
    }


    
    
    
    
    
    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((tailNumber == null) ? 0 : tailNumber.hashCode());
        result = prime * result + ((timeObs == null) ? 0 : timeObs.hashCode());
        return result;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ACARSRecord other = (ACARSRecord) obj;
        if (tailNumber == null) {
            if (other.tailNumber != null)
                return false;
        } else if (!tailNumber.equals(other.tailNumber))
            return false;
        if (timeObs == null) {
            if (other.timeObs != null)
                return false;
        } else if (!timeObs.equals(other.timeObs))
            return false;
        return true;
    }

    /**
     * 
     */
    @Override
    public int compareTo(ACARSRecord other) {

        final int BEFORE = -1;
        final int EQUAL = 0;
        final int AFTER = 1;

        int result = EQUAL;
        if (this == other) {
            result = EQUAL;
        } else {
            if(getTailNumber().equals(getTailNumber())) {
                result = timeObs.compareTo(other.timeObs);
            } else {
                result = getTailNumber().compareTo(other.getTailNumber());
            }
        }
        return result;
    }
    
    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
