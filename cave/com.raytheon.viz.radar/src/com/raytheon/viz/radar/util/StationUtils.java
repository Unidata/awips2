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
package com.raytheon.viz.radar.util;

import java.util.HashMap;
import java.util.Map;

import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.request.GetRadarSpatialRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;

/**
 * Utility for looking up home radar.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------------------------
 * Mar 19, 2010  4473     rjpeter   Initial creation
 * Feb 25, 2013  1659     bsteffen  Cache station in StationUtils
 * Jul 07, 2021  8576     randerso  Added method to get RadarStation by icao
 * Jan 27, 2022  8741     njensen   Added cache of RadarStations
 *
 * </pre>
 *
 * @author rjpeter
 */
public class StationUtils implements IPointChangedListener {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StationUtils.class);

    private RadarStation station = null;

    private static StationUtils instance;

    private final Map<String, RadarStation> radarStationCache = new HashMap<>();

    public static synchronized StationUtils getInstance() {
        if (instance == null) {
            instance = new StationUtils();
        }

        return instance;
    }

    private StationUtils() {
        PointsDataManager.getInstance().addHomeChangedListener(this);
    }

    public synchronized RadarStation getHomeRadarStation() {
        if (station == null) {
            Coordinate home = PointsDataManager.getInstance().getHome();
            station = getClosestRadarStation(home.x, home.y);
        }

        return station;
    }

    public RadarStation getClosestRadarStation(double lon, double lat) {
        GetRadarSpatialRequest request = new GetRadarSpatialRequest();
        request.setLat(lat);
        request.setLon(lon);
        try {
            Object response = ThriftClient.sendRequest(request);

            if (response != null && response instanceof RadarStation) {
                return (RadarStation) response;
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve home radar", e);
        }
        return null;
    }

    public RadarStation getRadarStation(String icao) {
        RadarStation radarStation = radarStationCache.get(icao);
        if (radarStation == null) {
            GetRadarSpatialRequest request = new GetRadarSpatialRequest();
            request.setIcao(icao);
            try {
                Object response = ThriftClient.sendRequest(request);

                if (response != null && response instanceof RadarStation) {
                    radarStation = (RadarStation) response;
                    radarStationCache.put(icao, radarStation);
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve radar " + icao, e);
            }
        }
        return radarStation;
    }

    @Override
    public void pointChanged() {
        station = null;
    }
}
