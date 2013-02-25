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

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.request.GetRadarSpatialRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Utility for looking up home radar.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2010 #4473      rjpeter     Initial creation
 * Feb 25, 2013 1659       bsteffen    Cache station in StationUtils
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class StationUtils implements IPointChangedListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StationUtils.class);

    private RadarStation station = null;

    private static StationUtils instance;

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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.IHomeChangedListener#homeLocationChanged()
     */
    @Override
    public void pointChanged() {
        station = null;
    }
}
