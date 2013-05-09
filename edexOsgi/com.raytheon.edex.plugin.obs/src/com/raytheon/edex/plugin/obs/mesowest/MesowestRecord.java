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

package com.raytheon.edex.plugin.obs.mesowest;

import java.util.Calendar;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record implementation for mesowest plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 2/14/2007	139			Phillippe	Initial creation	
 * 20071129            472  jkorman     Added IDecoderGettable interface.
 * May 07, 2013	1869      	bsteffen   	Remove dataURI column from
 *                                      PluginDataObject.
 * 
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MesowestRecord extends PluginDataObject {

    private static final long serialVersionUID = 1L;

    /** A string denoting the report type */
    @XmlAttribute
    @DynamicSerializeElement
    private String reportType;

    /** A string denoting the reporting station */
    @XmlAttribute
    @DynamicSerializeElement
    private String stationID;

    /** A string denoting the time of observation */
    @XmlElement
    @DynamicSerializeElement
    private Calendar timeObs;

    /** A string denoting the temperature in degrees Celsius */
    @XmlAttribute
    @DynamicSerializeElement
    private Integer temperature;

    /** A string denoting the current temperature in tenths of degrees Celsius */
    @XmlAttribute
    @DynamicSerializeElement
    private Float tempFromTenths;

    /** A string denoting the wind direction in degrees from north */
    @XmlAttribute
    @DynamicSerializeElement
    private String windDir;

    /** A string denoting the wind speed in knots */
    @XmlAttribute
    @DynamicSerializeElement
    private Integer windSpeed;

    /** A string denoting the wind gusts in knots */
    @XmlAttribute
    @DynamicSerializeElement
    private Integer windGust;

    /** A string denoting the current dew point in degrees Celsius */
    @XmlAttribute
    @DynamicSerializeElement
    private Integer dewPoint;

    /** A string denoting the current dew point in tenths of degrees Celsius */
    @XmlAttribute
    @DynamicSerializeElement
    private Float dewPointFromTenths;

    /** A string denoting the altimeter reading in in/Hg */
    @XmlAttribute
    @DynamicSerializeElement
    private Float altimeter;

    /** A string denoting the sea level pressure in millibars */
    @XmlAttribute
    @DynamicSerializeElement
    private Float seaLevelPress;

    /** The reference hour * */
    @XmlElement
    @DynamicSerializeElement
    private Calendar refHour;

    /**
     * No argument constructor
     * 
     */
    public MesowestRecord() {
    }

    /**
     * Constructs a mesowest record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public MesowestRecord(String uri) {
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
     * @return the dewPoint
     */
    public Integer getDewPoint() {
        return dewPoint;
    }

    /**
     * @param dewPoint
     *            the dewPoint to set
     */
    public void setDewPoint(Integer dewPoint) {
        this.dewPoint = dewPoint;
    }

    /**
     * @return the dewPointFromTenths
     */
    public Float getDewPointFromTenths() {
        return dewPointFromTenths;
    }

    /**
     * @param dewPointFromTenths
     *            the dewPointFromTenths to set
     */
    public void setDewPointFromTenths(Float dewPointFromTenths) {
        this.dewPointFromTenths = dewPointFromTenths;
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
     * @return the seaLevelPress
     */
    public Float getSeaLevelPress() {
        return seaLevelPress;
    }

    /**
     * @param seaLevelPress
     *            the seaLevelPress to set
     */
    public void setSeaLevelPress(Float seaLevelPress) {
        this.seaLevelPress = seaLevelPress;
    }

    /**
     * @return the stationID
     */
    public String getStationID() {
        return stationID;
    }

    /**
     * @param stationID
     *            the stationID to set
     */
    public void setStationID(String stationID) {
        this.stationID = stationID;
    }

    /**
     * @return the temperature
     */
    public Integer getTemperature() {
        return temperature;
    }

    /**
     * @param temperature
     *            the temperature to set
     */
    public void setTemperature(Integer temperature) {
        this.temperature = temperature;
    }

    /**
     * @return the tempFromTenths
     */
    public Float getTempFromTenths() {
        return tempFromTenths;
    }

    /**
     * @param tempFromTenths
     *            the tempFromTenths to set
     */
    public void setTempFromTenths(Float tempFromTenths) {
        this.tempFromTenths = tempFromTenths;
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
    public Integer getWindGust() {
        return windGust;
    }

    /**
     * @param windGust
     *            the windGust to set
     */
    public void setWindGust(Integer windGust) {
        this.windGust = windGust;
    }

    /**
     * @return the windSpeed
     */
    public Integer getWindSpeed() {
        return windSpeed;
    }

    /**
     * @param windSpeed
     *            the windSpeed to set
     */
    public void setWindSpeed(Integer windSpeed) {
        this.windSpeed = windSpeed;
    }

    /**
     * Get the IDecoderGettable reference for this record.
     * 
     * @return The IDecoderGettable reference for this record. Null for this
     *         class.
     */
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
