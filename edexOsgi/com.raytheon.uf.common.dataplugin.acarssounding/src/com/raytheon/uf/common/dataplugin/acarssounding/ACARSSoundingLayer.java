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
package com.raytheon.uf.common.dataplugin.acarssounding;

import java.io.Serializable;
import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * Layer for an ACARS Sounding
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 30, 2009            jkorman     Initial creation
 * Oct 22, 2013 2361       njensen     Remove XML annotations
 * Jul 22, 2014 3392       nabowle     Change Double fields to Float.
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0
 */

@Entity
@Table(name = "acarssoundinglayer")
@DynamicSerialize
public class ACARSSoundingLayer implements Serializable, ISpatialEnabled {

    private static final long serialVersionUID = 1L;

    // This record id. PK
    @Id
    @GeneratedValue
    private Integer recordId = null;

    @ManyToOne
    @JoinColumn(name = "parent", nullable = false)
    private ACARSSoundingRecord parent;

    // Time of the observation.
    @Column
    @DynamicSerializeElement
    private Calendar timeObs;

    @Column(length = 32)
    @DynamicSerializeElement
    private String tailNumber;

    @Embedded
    @DynamicSerializeElement
    private AircraftObsLocation location;

    // Flight phase
    @Column
    @DynamicSerializeElement
    private Integer flightPhase;

    // Observation air temperature in degrees Kelvin.
    @Column
    @DynamicSerializeElement
    private Float temp;

    // Observation dewpoint temperature in degrees Kelvin.
    // Decimal(5,2)
    @Column
    @DynamicSerializeElement
    private Float dwpt;

    // Relative Humidity in percent. Decimal(5,2)
    @Column
    @DynamicSerializeElement
    private Float humidity;

    // Mixing ratio in Kg/Kg
    @Column
    @DynamicSerializeElement
    private Float mixingRatio;

    // Observation wind direction in angular degrees. Integer
    @Column
    @DynamicSerializeElement
    private Integer windDirection;

    // Observation wind speed in meters per second.
    // Decimal(5,2)
    @Column
    @DynamicSerializeElement
    private Float windSpeed;

    // Base height of reported icing.
    @Column
    @DynamicSerializeElement
    private Integer iceBaseHgt;

    // Top height of reported icing.
    @Column
    @DynamicSerializeElement
    private Integer iceTopHgt;

    // Intensity of reported icing.
    @Column
    @DynamicSerializeElement
    private Integer icing;

    // Base height of reported turbulence.
    @Column
    @DynamicSerializeElement
    private Integer turbBaseHgt;

    // Top height of reported turbulence.
    @Column
    @DynamicSerializeElement
    private Integer turbTopHgt;

    // Intensity of reported turbulence.
    @Column
    @DynamicSerializeElement
    private Integer turbulence;

    // Indicated or calculated pressure.
    @Column
    @DynamicSerializeElement
    private Float pressure;

    /**
     *
     */
    public ACARSSoundingLayer() {

    }

    /**
     * The record id for this entry. Set from Hibernate.
     *
     * @return the recordId
     */
    public Integer getRecordId() {
        return recordId;
    }

    /**
     * Set the record id. Should be set by Hibernate only!
     *
     * @param recordId
     *            the recordId to set
     */
    public void setRecordId(Integer recordId) {
        this.recordId = recordId;
    }

    /**
     * Get the parent instance that contains this level information.
     *
     * @return The parent instance.
     */
    public ACARSSoundingRecord getParent() {
        return parent;
    }

    /**
     * Set the parent instance that contains this level information.
     *
     * @param parent
     *            The parent instance.
     */
    public void setParent(ACARSSoundingRecord parent) {
        this.parent = parent;
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
     * @return the dwpt
     */
    public Float getDwpt() {
        return dwpt;
    }

    /**
     * @param dwpt
     *            the dwpt to set
     */
    public void setDwpt(Float dwpt) {
        this.dwpt = dwpt;
    }

    /**
     * @return the humidity
     */
    public Float getHumidity() {
        return humidity;
    }

    /**
     * @param humidity
     *            the humidity to set
     */
    public void setHumidity(Float humidity) {
        this.humidity = humidity;
    }

    /**
     * @return the mixingRatio
     */
    public Float getMixingRatio() {
        return mixingRatio;
    }

    /**
     * @param mixingRatio
     *            the mixingRatio to set
     */
    public void setMixingRatio(Float mixingRatio) {
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
    public Float getWindSpeed() {
        return windSpeed;
    }

    /**
     * @param windSpeed
     *            the windSpeed to set
     */
    public void setWindSpeed(Float windSpeed) {
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
     * @return the pressure
     */
    public Float getPressure() {
        return pressure;
    }

    /**
     * @param pressure
     *            the pressure to set
     */
    public void setPressure(Float pressure) {
        this.pressure = pressure;
    }

    /**
     *
     */
    @Override
    public ISpatialObject getSpatialObject() {
        return location;
    }

    /**
     * Provide a string representation of this observation.
     *
     * @return
     */
    @Override
    public String toString() {
        return String.format("%s %1$td%1$tH%1$tM ", getTailNumber(),
                getTimeObs());
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((location == null) ? 0 : location.hashCode());
        result = prime * result
                + ((tailNumber == null) ? 0 : tailNumber.hashCode());
        result = prime * result + ((timeObs == null) ? 0 : timeObs.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     *
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
        ACARSSoundingLayer other = (ACARSSoundingLayer) obj;
        if (location == null) {
            if (other.location != null)
                return false;
        } else {
            // Get the point geometries and compare those!
            Point p1 = location.getLocation();
            Point p2 = other.location.getLocation();
            if (!p1.equals(p2)) {
                return false;
            }
        }
        if (tailNumber == null) {
            if (other.tailNumber != null) {
                return false;
            }
        } else if (!tailNumber.equals(other.tailNumber))
            return false;
        if (timeObs == null) {
            if (other.timeObs != null)
                return false;
        } else if (!timeObs.equals(other.timeObs))
            return false;
        return true;
    }

    public static final void main(String[] args) {

        ACARSSoundingLayer layerA = new ACARSSoundingLayer();
        AircraftObsLocation locA = new AircraftObsLocation();
        locA.setLocation(40.2, -101.5);
        locA.setFlightLevel(1052);
        layerA.setLocation(locA);
        layerA.setTailNumber("ABCDEF");

        ACARSSoundingLayer layerB = new ACARSSoundingLayer();
        AircraftObsLocation locB = new AircraftObsLocation();
        locB.setLocation(40.2, -101.5);
        locB.setFlightLevel(1052);
        layerB.setLocation(locB);
        layerB.setTailNumber("ABCDEF");

        System.out.println(layerA.hashCode());

        System.out.println(layerB.hashCode());

    }

}
