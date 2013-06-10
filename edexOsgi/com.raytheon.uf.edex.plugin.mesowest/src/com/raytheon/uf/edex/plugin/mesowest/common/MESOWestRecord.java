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
package com.raytheon.uf.edex.plugin.mesowest.common;

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

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
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
 * Mar 3, 2009            jkorman     Initial creation
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "mesowestseq")
@Table(name = "mesowest", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MESOWestRecord extends PluginDataObject implements ISpatialEnabled,
IDecoderGettable {
    
    private static final long serialVersionUID = 1L;

    public static final String OBS_TEXT = "text";
    
    public static final Unit<Length> LENGTH_UNIT = SI.METER;
    
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
        PARM_MAP.put("WGS", SFC_WNDGST);
        PARM_MAP.put("ASET", "SFC.PRESS.ALTIMETER");
        PARM_MAP.put("PMSL", PRES_SLP);
        PARM_MAP.put("NLAT", STA_LAT);
        PARM_MAP.put("NLON", STA_LON);
        PARM_MAP.put("STA","STA");
        PARM_MAP.put("stationid","STA");
        PARM_MAP.put("message",OBS_TEXT);
        PARM_MAP.put(OBS_TEXT,OBS_TEXT);
    }

    @DataURI(position = 1)
    @XmlElement
    @DynamicSerializeElement
    private String networkType;

    // Time of the observation.
    @DataURI(position = 2)
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar timeObs;

    @Embedded
    @DataURI(position = 3, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;
    
    // Observation air temperature in degrees Kelvin.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double temp;

    // Observation dewpoint temperature in degrees Kelvin.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double dwpt;

    // 24 Hour maximum temperature in degrees Kelvin.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double maxT24;

    // 24 Hour minimum temperature in degrees Kelvin.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double minT24;
    
    // Relative Humidity in percent.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double humidity;

    // Observation wind direction in angular degrees. Integer
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double windDirection;

    // Observation wind speed in meters per second.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double windSpeed;

    // Observation wind gust in meters per second.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double windGust;

    // Observation pressure in Pa.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double pressure;

    // Observation pressure in Pa.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double seaLevelPressure;

    // Observation pressure in Pa.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double altimeter;
    
    // Observation precip in mm.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double precip;

    // 1 minute precip in inches.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double precip_01M;
    
    // 5 minute precip in inches.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double precip_05M;

    // 10 minute precip in inches.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double precip_10M;

    // 15 minute precip in inches.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double precip_15M;

    // 30 minute precip in inches.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double precip_30M;

    // 1 hour precip in inches.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double precip_01H;

    // 3 hour precip in inches.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double precip_03H;

    // 6 hour precip in inches.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double precip_06H;

    // 24 hour precip in inches.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Double precip_24H;
    
    // Raw observation text
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String obsText;
    
    /**
     * 
     */
    public MESOWestRecord() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A data uri applicable to this class.
     */
    public MESOWestRecord(String uri) {
        super(uri);
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
     * @return the location
     */
    public SurfaceObsLocation getLocation() {
        return location;
    }

    /**
     * @param location the location to set
     */
    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
    }

    /**
     * 
     * @return 
     */
    public String getNetworkType() {
        return networkType;
    }

    /**
     * 
     * @return 
     */
    public void setNetworkType(String type) {
        networkType = type;
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

    // ******************************************
    
    /**
     * @return the temp
     */
    public Double getTemp() {
        return temp;
    }

    /**
     * @param temp the temp to set
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
     * @param dwpt the dwpt to set
     */
    public void setDwpt(Double dwpt) {
        this.dwpt = dwpt;
    }
    
    /**
     * @return the maxT24
     */
    public Double getMaxT24() {
        return maxT24;
    }

    /**
     * @param maxT24 the maxT24 to set
     */
    public void setMaxT24(Double maxT24) {
        this.maxT24 = maxT24;
    }

    /**
     * @return the minT24
     */
    public Double getMinT24() {
        return minT24;
    }

    /**
     * @param minT24 the minT24 to set
     */
    public void setMinT24(Double minT24) {
        this.minT24 = minT24;
    }

    /**
     * @return the humidity
     */
    public Double getHumidity() {
        return humidity;
    }

    /**
     * @param humidity the humidity to set
     */
    public void setHumidity(Double humidity) {
        this.humidity = humidity;
    }

    /**
     * @return the windDirection
     */
    public Double getWindDirection() {
        return windDirection;
    }

    /**
     * @param windDirection the windDirection to set
     */
    public void setWindDirection(Double windDirection) {
        this.windDirection = windDirection;
    }

    /**
     * @return the windSpeed
     */
    public Double getWindSpeed() {
        return windSpeed;
    }

    /**
     * @param windSpeed the windSpeed to set
     */
    public void setWindSpeed(Double windSpeed) {
        this.windSpeed = windSpeed;
    }

    /**
     * @return the windGust
     */
    public Double getWindGust() {
        return windGust;
    }

    /**
     * @param windGust the windGust to set
     */
    public void setWindGust(Double windGust) {
        this.windGust = windGust;
    }
    
    /**
     * @return the pressure
     */
    public Double getPressure() {
        return pressure;
    }

    /**
     * @param pressure the pressure to set
     */
    public void setPressure(Double pressure) {
        this.pressure = pressure;
    }
    
    /**
     * @return the seaLevelPressure
     */
    public Double getSeaLevelPressure() {
        return seaLevelPressure;
    }

    /**
     * @param seaLevelPressure the seaLevelPressure to set
     */
    public void setSeaLevelPressure(Double seaLevelPressure) {
        this.seaLevelPressure = seaLevelPressure;
    }

    /**
     * @return the altimeter
     */
    public Double getAltimeter() {
        return altimeter;
    }

    /**
     * @param altimeter the altimeter to set
     */
    public void setAltimeter(Double altimeter) {
        this.altimeter = altimeter;
    }
    

    /**
     * @return the precip
     */
    public Double getPrecip() {
        return precip;
    }

    /**
     * @param precip the precip to set
     */
    public void setPrecip(Double precip) {
        this.precip = precip;
    }

    /**
     * @return the precip_01M
     */
    public Double getPrecip_01M() {
        return precip_01M;
    }

    /**
     * @param precip_01M the precip_01M to set
     */
    public void setPrecip_01M(Double precip_01M) {
        this.precip_01M = precip_01M;
    }

    /**
     * @return the precip_05M
     */
    public Double getPrecip_05M() {
        return precip_05M;
    }

    /**
     * @param precip_05M the precip_05M to set
     */
    public void setPrecip_05M(Double precip_05M) {
        this.precip_05M = precip_05M;
    }

    /**
     * @return the precip_10M
     */
    public Double getPrecip_10M() {
        return precip_10M;
    }

    /**
     * @param precip_10M the precip_10M to set
     */
    public void setPrecip_10M(Double precip_10M) {
        this.precip_10M = precip_10M;
    }

    /**
     * @return the precip_15M
     */
    public Double getPrecip_15M() {
        return precip_15M;
    }

    /**
     * @param precip_15M the precip_15M to set
     */
    public void setPrecip_15M(Double precip_15M) {
        this.precip_15M = precip_15M;
    }

    /**
     * @return the precip_30M
     */
    public Double getPrecip_30M() {
        return precip_30M;
    }

    /**
     * @param precip_30M the precip_30M to set
     */
    public void setPrecip_30M(Double precip_30M) {
        this.precip_30M = precip_30M;
    }

    /**
     * @return the precip_01H
     */
    public Double getPrecip_01H() {
        return precip_01H;
    }

    /**
     * @param precip_01H the precip_01H to set
     */
    public void setPrecip_01H(Double precip_01H) {
        this.precip_01H = precip_01H;
    }

    /**
     * @return the precip_03H
     */
    public Double getPrecip_03H() {
        return precip_03H;
    }

    /**
     * @param precip_03H the precip_03H to set
     */
    public void setPrecip_03H(Double precip_03H) {
        this.precip_03H = precip_03H;
    }

    /**
     * @return the precip_06H
     */
    public Double getPrecip_06H() {
        return precip_06H;
    }

    /**
     * @param precip_06H the precip_06H to set
     */
    public void setPrecip_06H(Double precip_06H) {
        this.precip_06H = precip_06H;
    }

    /**
     * @return the precip_24H
     */
    public Double getPrecip_24H() {
        return precip_24H;
    }

    /**
     * @param precip_24H the precip_24H to set
     */
    public void setPrecip_24H(Double precip_24H) {
        this.precip_24H = precip_24H;
    }

    /**
     * @return the obsText
     */
    public String getObsText() {
        return obsText;
    }

    /**
     * @param obsText the obsText to set
     */
    public void setObsText(String obsText) {
        this.obsText = obsText;
    }
    
    // ******************************************

    /**
     * 
     */
    public void setSpatialObject(SurfaceObsLocation loc) {
        location = loc;
    }
    
    /**
     * 
     */
    @Override
    public SurfaceObsLocation getSpatialObject() {
        return location;
    }
    
    /**
     * This class implements IDecoderGettable so return this
     * instance.
     * @return The reference to this instance.
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return this;
    }

    /**
     * 
     */
    @Override
    public String getString(String paramName) {
        String retValue = null;
        String pName = PARM_MAP.get(paramName);
        if ("STA".matches(pName)) {
            retValue = getStationId();
        } else if (OBS_TEXT.equals(pName)) {
            retValue = getStationId();
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

        if (SFC_TEMP.equals(pName)) {
            a = new Amount(temp, TEMPERATURE_UNIT);
        } else if (SFC_DWPT.equals(pName)) {
            a = new Amount(dwpt, TEMPERATURE_UNIT);
        } else if (SFC_WNDSPD.equals(pName)) {
            a = new Amount(windSpeed, WIND_SPEED_UNIT);
        } else if (SFC_WNDGST.equals(pName)) {
            a = new Amount(windGust, WIND_SPEED_UNIT);
        } else if (SFC_WNDDIR.equals(pName)) {
            a = new Amount(windDirection, WIND_DIR_UNIT);
        } else if (PRES_ALTSG.equals(pName)) {
            a = new Amount(altimeter, PRESSURE_UNIT);
        } else if (STA_LAT.equals(pName)) {
            a = new Amount(getLatitude(), LOCATION_UNIT);
        } else if (STA_LON.equals(pName)) {
            a = new Amount(getLongitude(), LOCATION_UNIT);
        } else if (PRES_SLP.equals(pName)) {
            a = new Amount(seaLevelPressure, PRESSURE_UNIT);
        }
        
        return a;
    }
    
    /**
     * 
     */
    @Override
    public Collection<Amount> getValues(String paramName) {
        return null;
    }
    
}
