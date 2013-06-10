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

import java.text.DecimalFormat;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
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
 * SurfaceObsLocation represents an observation point on the surface of the
 * earth.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2007 391        jkorman     Initial Coding.
 * May 17, 2013 1869       bsteffen    Remove DataURI column from sat plot
 *                                     types.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Embeddable
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SurfaceObsLocation implements ISpatialObject, Cloneable {

    private static final long serialVersionUID = 1L;

    private static final ThreadLocal<DecimalFormat> LATLON_FORMAT = new ThreadLocal<DecimalFormat>(){

        @Override
        protected DecimalFormat initialValue() {
            return new DecimalFormat(
                    "###.###");
        }
        
    };

    // Elevation of this location in meters.
    @XmlAttribute
    @DynamicSerializeElement
    private Integer elevation = null;

    // Id of the station making this observation.
    @Column(length = 48)
    @Index(name = "%TABLE%_stationIndex")
    @DataURI(position = 0)
    @XmlAttribute
    @DynamicSerializeElement
    private String stationId;

    // Default to mobile. If defined the base location data has been retrieved
    // from a data base.
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Boolean locationDefined = Boolean.FALSE;

    @Column(name = "location", columnDefinition = "geometry")
    @Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
    @XmlJavaTypeAdapter(value = GeometryAdapter.class)
    @DynamicSerializeElement
    private Point location;

    @DataURI(position = 1)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Double latitude;

    @DataURI(position = 2)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Double longitude;

    /**
     * Create an empty instance of this class.
     */
    public SurfaceObsLocation() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public Object clone() throws CloneNotSupportedException {
        SurfaceObsLocation clone = (SurfaceObsLocation) super.clone();
        clone.elevation = new Integer(elevation);
        clone.latitude = new Double(latitude);
        clone.longitude = new Double(longitude);
        clone.locationDefined = new Boolean(locationDefined);
        clone.stationId = new String(stationId);
        return clone;
    }

    /**
     * Create an instance of this class using a given station identifier.
     * 
     * @param stationIdentifier
     */
    public SurfaceObsLocation(String stationIdentifier) {
        stationId = stationIdentifier;
    }

    public Double getLatitude() {
        return latitude;
    }

    public void setLatitude(Double latitude) {
        this.latitude = latitude;
    }

    public Double getLongitude() {
        return longitude;
    }

    public void setLongitude(Double longitude) {
        this.longitude = longitude;
    }

    /**
     * Get the elevation, in meters, of the observing platform or location.
     * 
     * @return The observation elevation, in meters.
     */
    public Integer getElevation() {
        return elevation;
    }

    /**
     * Set the elevation, in meters, of the observing platform or location.
     * 
     * @param elevation
     *            The elevation to set
     */
    public void setElevation(Integer elevation) {
        this.elevation = elevation;
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
     * Generate a stationId from the lat/lon values.
     */
    public void generateCoordinateStationId() {
        DecimalFormat format = LATLON_FORMAT.get();
        this.stationId = format.format(longitude) + ":"
                + format.format(latitude);
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
        return null;
    }

    @Override
    public Integer getNy() {
        return null;
    }

    public Point getLocation() {
        return location;
    }

    public void setLocation(Point location) {
        this.location = location;
    }

    public void assignLocation(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
        this.location = new GeometryFactory().createPoint(new Coordinate(
                MapUtil.correctLon(longitude), MapUtil.correctLat(latitude)));
    }

    public void setGeometry(Point point) {
        assignLocation(point.getY(), point.getX());
    }

    public void assignLatitude(double latitude) {
        this.latitude = latitude;
        if (longitude != null && location == null) {
            assignLocation(this.latitude, this.longitude);
        }
    }

    public void assignLongitude(double longitude) {
        this.longitude = longitude;
        if (latitude != null && location == null) {
            assignLocation(this.latitude, this.longitude);
        }
    }
}
