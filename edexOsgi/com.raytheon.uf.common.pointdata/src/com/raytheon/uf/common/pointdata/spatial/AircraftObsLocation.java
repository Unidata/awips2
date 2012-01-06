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
package com.raytheon.uf.common.pointdata.spatial;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.adapters.GeometryAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * AircraftObsLocation represents an observation point above the surface of the
 * earth.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071026            384 jkorman     Initial Coding.
 * 20090408            952 jsanchez    Added @DynamicSerializeElement tags.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Embeddable
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AircraftObsLocation implements ISpatialObject {

    private static final long serialVersionUID = 1L;

    // Elevation of this location in meters.
    @Column
    @DataURI(position = 3)
    @DynamicSerializeElement
    private Integer flightLevel = null;

    // Id of the station making this observation.
    @Column(length = 16)
    @DataURI(position = 0)
    @DynamicSerializeElement
    @Index(name = "%TABLE%_stationIndex")
    private String stationId;

    // Default to mobile. If defined the base location data has been retrieved
    // from a data base.
    @Column
    @DynamicSerializeElement
    private Boolean locationDefined = Boolean.FALSE;

    @Column
    @DataURI(position = 1)
    @DynamicSerializeElement
    private double latitude;

    @Column
    @DataURI(position = 2)
    @DynamicSerializeElement
    private double longitude;

    @Column(name = "location", columnDefinition = "geometry")
    @Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
    @XmlJavaTypeAdapter(value = GeometryAdapter.class)
    @DynamicSerializeElement
    private Point location;

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * Create an empty instance of this class.
     */
    public AircraftObsLocation() {
    }

    /**
     * Create an instance of this class using a given station identifier.
     * 
     * @param stationIdentifier
     */
    public AircraftObsLocation(String stationIdentifier) {
        stationId = stationIdentifier;
    }

    // /**
    // * Set the geometry with a Point type. Values of the point are assumed to
    // be
    // * a latitude, longitude pair with values in degrees.
    // *
    // * @param geometry
    // * A Point geometry to set.
    // */
    // public void setGeometry(Geometry geometry) {
    // if (geometry instanceof Point) {
    // Point p = (Point) geometry;
    // double lat = p.getY();
    // double lon = p.getX();
    // if ((lat >= -90.0) && (lat <= 90.0)) {
    // if ((lon >= -180.0) && (lon <= 180)) {
    // super.setGeometry(geometry);
    // }
    // }
    // }
    // }

    // /**
    // * The the strike latitude/longitude location.
    // *
    // * @param latitude
    // * The strike latitude. (-90..90, positive north)
    // * @param longitude
    // * The strike longitude. (-180..180, negative west)
    // */
    // public void setLatLon(double latitude, double longitude) {
    // setGeometry(new Point(longitude, latitude));
    // }

    // /**
    // *
    // * @see
    // com.raytheon.edex.db.objects.SpatialPointDataObject#getCoordinate()
    // */
    // @Override
    // public Coordinate getCoordinate() {
    // Coordinate coordinate = null;
    // Point obsGeometry = (Point) getGeometry();
    // if (obsGeometry != null) {
    // coordinate = new Coordinate(obsGeometry.getX(), obsGeometry.getY());
    // }
    // return coordinate;
    // }

    // /**
    // *
    // * @see
    // com.raytheon.edex.db.objects.SpatialPointDataObject#getSecondCoordinate()
    // */
    // @Override
    // public Coordinate getSecondCoordinate() {
    // return null;
    // }

    /**
     * Get the elevation, in meters, of the observing platform or location.
     * 
     * @return The observation elevation, in meters.
     */
    public Integer getFlightLevel() {
        return flightLevel;
    }

    /**
     * Set the elevation, in meters, of the observing platform or location.
     * 
     * @param elevation
     *            The elevation to set
     */
    public void setFlightLevel(Integer flightLevel) {
        this.flightLevel = flightLevel;
    }

    /**
     * @return the stationId
     */
    public String getStationId() {
        return stationId;
    }

    /**
     * @param stationId
     *            the stationId to set
     */
    public void setStationId(String stationId) {
        this.stationId = stationId;
    }

    /**
     * Is this location a station lookup.
     * 
     * @return the locationDefined
     */
    public Boolean getLocationDefined() {
        return locationDefined;
    }

    /**
     * @param locationDefined
     *            the locationDefined to set
     */
    public void setLocationDefined(Boolean locationDefined) {
        this.locationDefined = locationDefined;
    }

    public double getLatitude() {
        return latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    @Override
    public CoordinateReferenceSystem getCrs() {
        return null;
    }

    @Override
    public Geometry getGeometry() {
        return location;
    }

    @Override
    public Integer getNx() {
        return 0;
    }

    @Override
    public Integer getNy() {
        return 0;
    }

    public Point getLocation() {
        return location;
    }

    public void setLocation(Point location) {
        this.location = location;
    }

    public void setLocation(double latitude, double longitude) {
        this.location = new GeometryFactory().createPoint(new Coordinate(
                MapUtil.correctLon(longitude), MapUtil.correctLat(latitude)));
    }

    public static long getSerialVersionUID() {
        return serialVersionUID;
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
                + ((flightLevel == null) ? 0 : flightLevel.hashCode());
        long temp;
        temp = Double.doubleToLongBits(latitude);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(longitude);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result
                + ((stationId == null) ? 0 : stationId.hashCode());
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
        AircraftObsLocation other = (AircraftObsLocation) obj;
        if (flightLevel == null) {
            if (other.flightLevel != null)
                return false;
        } else if (!flightLevel.equals(other.flightLevel))
            return false;
        if (Double.doubleToLongBits(latitude) != Double
                .doubleToLongBits(other.latitude))
            return false;
        if (Double.doubleToLongBits(longitude) != Double
                .doubleToLongBits(other.longitude))
            return false;
        if (stationId == null) {
            if (other.stationId != null)
                return false;
        } else if (!stationId.equals(other.stationId))
            return false;
        return true;
    }

}
