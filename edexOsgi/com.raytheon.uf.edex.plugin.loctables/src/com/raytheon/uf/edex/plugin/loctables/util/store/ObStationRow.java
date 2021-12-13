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
package com.raytheon.uf.edex.plugin.loctables.util.store;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * Represents an ObStation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2010            jkorman     Initial creation
 * Oct 12, 2015 4911       rjpeter     Updated requiresUpdate.
 * Dec 04, 2015 4911       rjpeter     Added additional fields to requiresUpdate.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ObStationRow {
    private String icao;

    private Integer wmoIndex;

    private String stationId;

    private Integer catalogType;

    private String name;

    private String country;

    private String state;

    private Integer wmoRegion;

    // Surface observing location elevation
    private Integer elevation;

    // Surface observing location latitude/longitude
    private Point location;

    // Upperair observing location elevation
    private Integer upperAirElevation;

    // Upperair observing location latitude/longitude
    private Point upperAirGeometry;

    /**
     * 
     */
    public ObStationRow() {

    }

    /**
     * 
     * @param catType
     */
    public ObStationRow(Integer catType) {
        catalogType = catType;
    }

    /**
     * @return the icao
     */
    public String getIcao() {
        return icao;
    }

    /**
     * @param icao
     *            the icao to set
     */
    public void setIcao(String icao) {
        this.icao = icao;
    }

    /**
     * @return the wmoIndex
     */
    public Integer getWmoIndex() {
        return wmoIndex;
    }

    /**
     * @param wmoIndex
     *            the wmoIndex to set
     */
    public void setWmoIndex(Integer wmoIndex) {
        this.wmoIndex = wmoIndex;
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
     * @return the catalogType
     */
    public Integer getCatalogType() {
        return catalogType;
    }

    /**
     * @param catalogType
     *            the catalogType to set
     */
    public void setCatalogType(Integer catalogType) {
        this.catalogType = catalogType;
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
        if (name != null) {
            StringBuilder sb = new StringBuilder(name);
            for (int i = 0; i < sb.length(); i++) {
                switch (sb.charAt(i)) {
                case '\'': {
                    sb.setCharAt(i, ' ');
                    break;
                }
                case '\\': {
                    sb.setCharAt(i, '/');
                    break;
                }
                case ';': {
                    // This mod is required because the CoreDAO script
                    // runner splits on semicolons which breaks otherwise
                    // legal SQL.
                    sb.setCharAt(i, ':');
                    break;
                }
                }
            }
            name = sb.toString();
        }
        this.name = name;
    }

    /**
     * @return the country
     */
    public String getCountry() {
        return country;
    }

    /**
     * @param country
     *            the country to set
     */
    public void setCountry(String country) {
        this.country = country;
    }

    /**
     * @return the state
     */
    public String getState() {
        return state;
    }

    /**
     * @param state
     *            the state to set
     */
    public void setState(String state) {
        this.state = state;
    }

    /**
     * @return the wmoRegion
     */
    public Integer getWmoRegion() {
        return wmoRegion;
    }

    /**
     * @param wmoRegion
     *            the wmoRegion to set
     */
    public void setWmoRegion(Integer wmoRegion) {
        this.wmoRegion = wmoRegion;
    }

    /**
     * @return the elevation
     */
    public Integer getElevation() {
        return elevation;
    }

    /**
     * @param elevation
     *            the elevation to set
     */
    public void setElevation(Integer elevation) {
        this.elevation = elevation;
    }

    /**
     * @return the upperAirElevation
     */
    public Integer getUpperAirElevation() {
        return upperAirElevation;
    }

    /**
     * @param upperAirElevation
     *            the upperAirElevation to set
     */
    public void setUpperAirElevation(Integer upperAirElevation) {
        this.upperAirElevation = upperAirElevation;
    }

    /**
     * @return the upperAirGeometry
     */
    public Point getUpperAirGeometry() {
        return upperAirGeometry;
    }

    /**
     * @param upperAirGeometry
     *            the upperAirGeometry to set
     */
    public void setUpperAirGeometry(Point upperAirGeometry) {
        this.upperAirGeometry = upperAirGeometry;
    }

    /**
     * @return the location
     */
    public Point getLocation() {
        return location;
    }

    /**
     * @param location
     *            the location to set
     */
    public void setLocation(Point location) {
        this.location = location;
    }

    /**
     * @return the gid
     */
    public String getGid() {
        return ObStation.createGID(catalogType, stationId);
    }

    public static Point getPoint(double lat, double lon) {
        return new GeometryFactory().createPoint(new Coordinate(MapUtil
                .correctLon(lon), MapUtil.correctLat(lat)));
    }

    /**
     * 
     * @return
     */
    public ObStation toObStation() {
        ObStation station = new ObStation();
        // gid
        station.setGid(getGid());
        // catalogType
        station.setCatalogType(getCatalogType());
        // stationId
        station.setStationId(getStationId());
        // icao
        station.setIcao(getIcao());
        // wmoIndex
        station.setWmoIndex(getWmoIndex());
        // wmoRegion
        station.setWmoRegion(getWmoRegion());
        // country
        station.setCountry(getCountry());
        // state
        station.setState(getState());
        // elevation
        station.setElevation(getElevation());
        // the_geom
        station.setLocation(getLocation());
        // upperAirElevation
        station.setUpperAirElevation(getUpperAirElevation());
        // upperairgeom
        station.setUpperAirGeometry(getUpperAirGeometry());
        // name
        station.setName(getName());
        return station;
    }

    /**
     * Determine if a given ObStation instance needs to be updated from this
     * ObStationRow.
     * 
     * @param station
     *            Target ObStation instance to be updated.
     * @return Does the ObStation target instance need to be updated.
     */
    public boolean requiresUpdate(ObStation b) {
        boolean newStation = false;

        /*
         * For non ICAO stations this isn't part of the key
         */
        if (copyItem(getIcao(), b.getIcao())) {
            b.setIcao(getIcao());
            newStation = true;
        }

        if (copyItem(getName(), b.getName())) {
            b.setName(getName());
            newStation = true;
        }

        if (copyItem(getCountry(), b.getCountry())) {
            b.setCountry(getCountry());
            newStation = true;
        }

        if (copyItem(getState(), b.getState())) {
            b.setState(getState());
            newStation = true;
        }

        if (copyItem(getWmoIndex(), b.getWmoIndex())) {
            b.setWmoIndex(getWmoIndex());
            newStation = true;
        }

        if (copyItem(getWmoRegion(), b.getWmoRegion())) {
            b.setWmoRegion(getWmoRegion());
            newStation = true;
        }

        if (copyItem(getElevation(), b.getElevation())) {
            b.setElevation(getElevation());
            newStation = true;
        }

        if (copyItem(getUpperAirElevation(), b.getUpperAirElevation())) {
            b.setUpperAirElevation(getUpperAirElevation());
            newStation = true;
        }

        if (copyItem(getUpperAirGeometry(), b.getUpperAirGeometry())) {
            b.setUpperAirGeometry(getUpperAirGeometry());
            newStation = true;
        }

        if (copyItem(getLocation(), b.getLocation())) {
            b.setLocation(getLocation());
            newStation = true;
        }

        return newStation;
    }

    private static boolean copyItem(Object a, Object b) {
        boolean copy = false;
        if ((a == null)) {
            copy = (b != null);
            if (copy && (b instanceof String)) {
                // null and empty string are the same
                copy = !((String) b).trim().isEmpty();
            }
        } else {
            if (b != null) {
                if ((a instanceof Point) && (b instanceof Point)) {
                    Point aa = (Point) a;
                    Point bb = (Point) b;

                    copy = (aa.getX() != bb.getX()) || (aa.getY() != bb.getY());
                } else {
                    copy = (!a.equals(b));
                }
            } else {
                if (a instanceof String) {
                    copy = !((String) a).trim().isEmpty();
                } else {
                    copy = true;
                }
            }
        }
        return copy;
    }
}
