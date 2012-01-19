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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

public class SPIContainer implements SPI_InfoProvider {

    private static final String REGEX = "^\\s*(\\d+)\\s*(\\S+)\\s*(-?\\d+\\.\\d+)\\s*(-?\\d+\\.\\d+)\\s*(-?\\d+)\\s*(-?\\d+\\.\\d+)\\s*(\\S*)$";

    private Map<String, SPIEntry> idMap;

    private double maxLat;

    private double minLat;

    private double maxLon;

    private double minLon;

    /**
     * 
     * @param filePath
     */
    public SPIContainer(String filePath) {
        populateCatalog(new File(filePath));
    }

    /**
     * 
     * @param filePath
     */
    public SPIContainer(File filePath) {
        populateCatalog(filePath);
    }

    /**
     * 
     * @param filePath
     */
    private void populateCatalog(File filePath) {
        if (filePath != null) {
            idMap = new HashMap<String, SPIEntry>();

            BufferedReader input = null;
            try {
                input = new BufferedReader(new FileReader(filePath));
                String line = null;
                Pattern p = Pattern.compile(REGEX);
                int lineNum = 0;
                maxLat = -90;
                minLat = 90;
                maxLon = -180;
                minLon = 180;
                while ((line = input.readLine()) != null) {
                    lineNum++;
                    try {
                        Matcher match = p.matcher(line);
                        if (match.find()) {
                            SPIEntry entry = new SPIEntry();
                            entry.setBlockNumber(Integer.parseInt(match
                                    .group(1)));
                            entry.setId(match.group(2));
                            double lat = Double.parseDouble(match.group(3));
                            double lon = Double.parseDouble(match.group(4));
                            entry
                                    .setLatlon(SPIEntry.createCoordinate(lat,
                                            lon));
                            entry
                                    .setElevation(Integer.parseInt(match
                                            .group(5)));
                            entry.setDistance(Double
                                    .parseDouble(match.group(6)));
                            entry.setName(match.group(7));

                            idMap.put(entry.getId(), entry);

                            maxLat = Math.max(maxLat, lat);
                            minLat = Math.min(minLat, lat);

                            maxLon = Math.max(maxLon, lon);
                            minLon = Math.min(minLon, lon);
                        }
                    } catch (Exception e) {

                    }
                }
                input.close();
            } catch (FileNotFoundException ex) {
                idMap = null;
            } catch (IOException ex) {
                idMap = null;
            }
        }
    }

    /**
     * @see com.raytheon.uf.common.geospatial.spi.SPI_InfoProvider#getEntryById(java.lang.String)
     * @param id
     */
    @Override
    public SPIEntry getEntryById(String id) {
        SPIEntry location = null;
        if (idMap != null) {
            location = idMap.get(id);
        }
        return location;
    }

    /**
     * 
     * @see com.raytheon.uf.common.geospatial.spi.SPI_InfoProvider#getEntryByName(java.lang.String)
     * @param name
     */
    @Override
    public SPIEntry getEntryByName(String name) {
        SPIEntry location = null;

        return location;
    }

    /**
     * Find the closest catalog station to a given latitude,longitude position
     * that is also within a given distance to the catalog location.
     * 
     * @param latlon
     *            Latitude/Longitude coordinates of the target point.
     * @param maxDist
     *            The maximum distance (Kilometers) to allow between target and
     *            catalog points.
     * @return The closest catalog entry to the given latlon coordinate that is
     *         also closer that maxDist kilometers to the point.
     * @see com.raytheon.uf.common.geospatial.spi.SPI_InfoProvider#nearest(com.vividsolutions.jts.geom.Coordinate,
     *      float)
     */
    @Override
    public SPIEntry nearest(Coordinate latlon, float maxDist) {
        return nearest(SPIEntry.getCoordinateLatitude(latlon), SPIEntry
                .getCoordinateLongitude(latlon), maxDist);
    }

    /**
     * Find the closest catalog station to a given latitude,longitude position
     * that is also within a given distance to the catalog location.
     * 
     * @param lat
     *            Target latitude.
     * @param lon
     *            Target longitude.
     * @param maxDist
     *            The maximum distance (Kilometers) to allow between target and
     *            catalog points.
     * @return The closest catalog entry to the given latlon coordinate that is
     *         also closer that maxDist kilometers to the point.
     */
    public SPIEntry nearest(double lat, double lon, float maxDist) {
        SPIEntry location = null;
        if (idMap != null) {
            // adjust the bounding box

            double dist = maxDist / 111.12f;

            double miLat = minLat - dist;
            double mxLat = maxLat + dist;

            double miLon = minLon - dist;
            double mxLon = maxLon + dist;

            if ((lat >= miLat) && (lat <= mxLat)) {
                if ((lon >= miLon) && (lon <= mxLon)) {
                    double currDist = maxDist;
                    
                    for (SPIEntry s : idMap.values()) {
                        double d = s.distance(lat, lon);
                        if (d < currDist) {
                            location = s;
                            currDist = d;
                        }
                    }
                }
            }
        }
        return location;
    }

    /**
     * @param latlon
     * @see com.raytheon.uf.common.geospatial.spi.SPI_InfoProvider#nearest(com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    public SPIEntry nearest(Coordinate latlon) {
        return nearest(SPIEntry.getCoordinateLatitude(latlon), SPIEntry
                .getCoordinateLongitude(latlon));
    }

    /**
     * Find the closest catalog station to a given latitude,longitude position.
     * 
     * @param lat
     *            Target latitude.
     * @param lon
     *            Target longitude.
     * @return The closest catalog entry. Returns null if the catalog is empty.
     */
    public SPIEntry nearest(double lat, double lon) {
        SPIEntry location = null;
        if (idMap != null) {
            // Just a number bigger that the max distance on the Earth's
            // surface.
            double currDist = 999999;
            for (SPIEntry s : idMap.values()) {
                double d = s.distance(lat, lon);
                if (d < currDist) {
                    location = s;
                    currDist = d;
                }
            }
        }
        return location;
    }

    /**
     * Has the catalog been loaded?
     * @return The catalog load status.
     * @see com.raytheon.uf.common.geospatial.spi.SPI_InfoProvider#isLoaded()
     */
    @Override
    public boolean isLoaded() {
        return ((idMap != null) && (idMap.size() > 0));
    }

}
