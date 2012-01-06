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
package com.raytheon.uf.common.geospatial.spi;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2009            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class SPIEntry {

    private String name;

    private String id;

    private int blockNumber;

    private int elevation;

    private Coordinate latlon = new Coordinate();

    private double latitude;
    
    private double longitude;
    
    private double distance;

    private String accessId;

    public SPIEntry() {
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the blockNumber
     */
    public int getBlockNumber() {
        return blockNumber;
    }

    /**
     * @param blockNumber
     *            the blockNumber to set
     */
    public void setBlockNumber(int blockNumber) {
        this.blockNumber = blockNumber;
    }

    /**
     * @return the elevation
     */
    public int getElevation() {
        return elevation;
    }

    /**
     * @param elevation
     *            the elevation to set
     */
    public void setElevation(int elevation) {
        this.elevation = elevation;
    }

    /**
     * @return the latlon
     */
    public Coordinate getLatlon() {
        return latlon;
    }

    /**
     * @param latlon
     *            the latlon to set
     */
    public void setLatlon(Coordinate latlon) {
        this.latlon = latlon;
        latitude = getCoordinateLatitude(latlon);
        longitude = getCoordinateLongitude(latlon);
    }

    public double getLatitude() {
        double lat = getCoordinateLatitude(latlon);
        if(latitude != lat) {
            latitude = lat;
        }
        return latitude;
    }
    
    public double getLongitude() {
        double lon = getCoordinateLongitude(latlon);
        if(longitude != lon) {
            longitude = lon;
        }
        return longitude;
    }
    
    /**
     * @return the distance
     */
    public double getDistance() {
        return distance;
    }

    /**
     * @param distance
     *            the distance to set
     */
    public void setDistance(double distance) {
        this.distance = distance;
    }

    /**
     * @return the accessId
     */
    public String getAccessId() {
        return accessId;
    }

    /**
     * @param accessId
     *            the accessId to set
     */
    public void setAccessId(String accessId) {
        this.accessId = accessId;
    }

    /**
     * Compute the distance from this entry and a given entry.
     * 
     * @param entry
     *            The entry to compute the distance to.
     * @return The distance to the given entry. Returns -1 if the distance
     *         cannot be computed.
     */
    public double distance(SPIEntry entry) {
        double dist = -1;
        if (entry != null) {
            dist = distance(entry.getLatlon());
        }
        return dist;
    }

    /**
     * Compute the distance from this entry and a given entry.
     * 
     * @param entry
     *            The entry to compute the distance to.
     * @return The distance to the given entry. Returns -1 if the distance
     *         cannot be computed.
     */
    public double distance(Coordinate latlon) {
        double dist = -1;
        if (latlon != null) {
            dist = distance(getCoordinateLatitude(latlon),
                    getCoordinateLatitude(latlon));
        }
        return dist;
    }

    /**
     * Compute the distance from this entry and a given entry.
     * 
     * @param entry
     *            The entry to compute the distance to.
     * @return The distance to the given entry. Returns -1 if the distance
     *         cannot be computed.
     */
    public double distance(double lat, double lon) {
        double dist = -1;

        double sLat = getLatitude();
        double sLon = getLongitude();
        
        double cosLat = Math.cos(Math.toRadians(lat));
        
        double dy = lat - sLat;

        double dx = cosLat*(lon - sLon);

        dist = Math.sqrt(dx*dx+dy*dy) * 111.12;
        
        return dist;
    }

    @Override
    public String toString() {
        return id;
    }

    /**
     * Create a Coordinate instance with a given latitude, longitude.
     * 
     * @param latitude
     *            The coordinate latitude.
     * @param longitude
     *            The coordinate longitude.
     * @return The created Coordinate instance.
     */
    public static Coordinate createCoordinate(double latitude, double longitude) {
        return new Coordinate(longitude, latitude);
    }

    /**
     * Helper method so we don't have to remember Coordinate x and y.
     * 
     * @param coord
     * @return The Coordinate longitude.
     */
    public static double getCoordinateLongitude(Coordinate coord) {
        return coord.x;
    }

    /**
     * Helper method so we don't have to remember Coordinate x and y.
     * 
     * @param coord
     * @return The Coordinate latitude.
     */
    public static double getCoordinateLatitude(Coordinate coord) {
        return coord.y;
    }

}
