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
package com.raytheon.edex.plugin.radar.util;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.edex.plugin.radar.dao.RadarStationDao;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Utilities for dealing with radar spatial data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2010 #4473      rjpeter     Initial creation.
 * Mar 19, 2013 1804       bsteffen    Cache db queries in radar decoder.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class RadarSpatialUtil {

    private static Map<Integer, RadarStation> rpgIdDec2radarStation = new ConcurrentHashMap<Integer, RadarStation>();

    /**
     * Gets the closest RadarStation entry for a given wfo, lat, lon
     * 
     * @param wfoId
     *            3 char wfo id
     * @param lat
     *            the latitude in degrees
     * @param lon
     *            longitude in degrees between -180 and 180
     * @return the radar station
     * @throws Exception
     */
    public static RadarStation getClosetRadarStation(double lat, double lon)
            throws Exception {
        RadarStationDao dao = new RadarStationDao();
        // This should work anywhere over the US
        List<RadarStation> stations = dao.queryBySpatialPolygon(Arrays.asList(
                new Coordinate(lon - 10, lat - 10), new Coordinate(lon + 10,
                        lat - 10), new Coordinate(lon + 10, lat + 10),
                new Coordinate(lon - 10, lat + 10)));
        RadarStation rval = null;

        if (stations != null && stations.size() > 0) {
            rval = stations.get(0);

            if (stations.size() > 0) {
                // need to determine closest
                GeodeticCalculator gc = new GeodeticCalculator();
                gc.setStartingGeographicPoint(lon, lat);
                gc.setDestinationGeographicPoint(rval.getLon(), rval.getLat());
                double minDist = gc.getOrthodromicDistance();

                for (int i = 1; i < stations.size(); i++) {
                    RadarStation tmp = stations.get(i);
                    gc.setDestinationGeographicPoint(tmp.getLon(), tmp.getLat());
                    double dist = gc.getOrthodromicDistance();
                    if (dist < minDist) {
                        rval = tmp;
                        minDist = dist;
                    }
                }
            }
        }

        return rval;
    }

    /**
     * Provides a level of caching around RadarStationDao.queryByRpgIdDec.
     * 
     * @param rpgIdDec
     * @return
     * @throws DataAccessLayerException
     */
    public static RadarStation getRadarStationByRpgIdDec(int rpgIdDec)
            throws DataAccessLayerException {
        RadarStation station = rpgIdDec2radarStation.get(rpgIdDec);
        if (station == null) {
            RadarStationDao stat = new RadarStationDao();
            station = stat.queryByRpgIdDec(String.format("%03d", rpgIdDec));
            rpgIdDec2radarStation.put(rpgIdDec, station);
        }
        return station;
    }

}
