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
package com.raytheon.viz.grid.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.LatLonGridCoverage;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.grid.Activator;
import com.raytheon.viz.radar.util.StationUtils;

/**
 * Utility for generating grids of tilt heights for a Radar.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2009            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class TiltUtils {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(TiltUtils.class);

    private Map<Integer, double[]> gridRadiusCache = new HashMap<Integer, double[]>();

    private ConcurrentMap<Integer, Long> gridRadiusCacheTime = new ConcurrentHashMap<Integer, Long>();

    private Timer purgeTimer;

    private static TiltUtils instance;

    public static synchronized TiltUtils getInstance() {
        if (instance == null) {
            instance = new TiltUtils();
        }

        return instance;
    }

    private TiltUtils() {
        purgeTimer = new Timer();

        TimerTask purgeTask = new TimerTask() {
            @Override
            public void run() {
                // keep last 10 minutes of heights
                long purgeTime = System.currentTimeMillis() - 600000;
                Iterator<Entry<Integer, Long>> iter = gridRadiusCacheTime
                        .entrySet().iterator();
                while (iter.hasNext()) {
                    Entry<Integer, Long> entry = iter.next();
                    if (entry.getValue() < purgeTime) {
                        gridRadiusCache.remove(entry.getKey());
                        iter.remove();
                    }
                }
            }
        };
        purgeTimer.schedule(purgeTask, 600, 600000);
    }

    /**
     * 
     * @param coverage
     * @param tilt
     *            in degrees
     */
    public FloatDataRecord getHeightGrid(GridCoverage coverage, double tilt) {
        Integer coverageId = coverage.getId();

        RadarStation homeRadar = StationUtils.getInstance()
                .getHomeRadarStation();

        if (homeRadar != null) {
            GridGeometry2D geometry = MapUtil.getGridGeometry(coverage);
            GridEnvelope2D gridRange = geometry.getGridRange2D();
            double[] radius = gridRadiusCache.get(coverageId);
            if (radius == null) {
                try {

                    if (!(coverage instanceof LatLonGridCoverage)) {
                        MathTransform gridToCrs = geometry.getGridToCRS();
                        MathTransform fromLatLon = MapUtil
                                .getTransformFromLatLon(coverage.getCrs());

                        double[] radarLonLat = new double[] {
                                homeRadar.getLon(), homeRadar.getLat() };
                        double[] radarCrsCoord = new double[2];
                        fromLatLon.transform(radarLonLat, 0, radarCrsCoord, 0,
                                1);
                        int numPoints = gridRange.height * gridRange.width;
                        double[] gridCoordGrid = new double[numPoints * 2];
                        int offset = 0;
                        for (int j = 0; j < gridRange.height; j++) {
                            for (int i = 0; i < gridRange.width; i++) {
                                gridCoordGrid[offset++] = i;
                                gridCoordGrid[offset++] = j;
                            }
                        }
                        gridToCrs.transform(gridCoordGrid, 0, gridCoordGrid, 0,
                                numPoints);
                        radius = new double[numPoints];
                        offset = 0;
                        for (int i = 0; i < numPoints; i++) {
                            double xDist = radarCrsCoord[0]
                                    - gridCoordGrid[offset++];
                            double yDist = radarCrsCoord[1]
                                    - gridCoordGrid[offset++];
                            radius[i] = Math
                                    .sqrt(xDist * xDist + yDist * yDist);
                        }
                    } else {
                        // use geodetic calculator
                        MathTransform gridToLatLon = geometry.getGridToCRS();

                        radius = getRadius(gridRange, gridToLatLon);

                    }
                    gridRadiusCache.put(coverageId, radius);
                } catch (Exception e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Error occurred generating height grid for radar tilt",
                            e);
                }
            }
            return getHeightGrid(gridRange, radius, tilt);
        }
        return null;
    }

    public FloatDataRecord getHeightGrid(GridEnvelope2D gridRange,
            MathTransform gridToLatLon, double tilt) {
        try {
            double[] radius = getRadius(gridRange, gridToLatLon);
            return getHeightGrid(gridRange, radius, tilt);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred generating height grid for radar tilt", e);
        }
        return null;
    }

    private FloatDataRecord getHeightGrid(GridEnvelope2D gridRange,
            double[] radius, double tilt) {
        RadarStation homeRadar = StationUtils.getInstance()
                .getHomeRadarStation();
        float elevMeter = homeRadar.getElevMeter();
        double tanTilt = Math.tan(Math.toRadians(tilt));
        float[] floatData = new float[radius.length];
        for (int i = 0; i < radius.length; i++) {
            floatData[i] = (float) (elevMeter + tanTilt * radius[i]);
        }
        FloatDataRecord fdr = new FloatDataRecord();
        fdr.setFloatData(floatData);
        fdr.setDimension(2);
        long[] sizes = { gridRange.width, gridRange.height };
        fdr.setSizes(sizes);
        return fdr;
    }

    private double[] getRadius(GridEnvelope2D gridRange,
            MathTransform gridToLatLon) throws TransformException {
        RadarStation homeRadar = StationUtils.getInstance()
                .getHomeRadarStation();
        int numPoints = gridRange.height * gridRange.width;
        int offset = 0;
        double[] gridCoordGrid = new double[numPoints * 2];
        for (int j = 0; j < gridRange.height; j++) {
            for (int i = 0; i < gridRange.width; i++) {
                gridCoordGrid[offset++] = i;
                gridCoordGrid[offset++] = j;
            }
        }

        gridToLatLon.transform(gridCoordGrid, 0, gridCoordGrid, 0, numPoints);

        double[] radius = new double[numPoints];
        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(homeRadar.getLon(), homeRadar.getLat());
        offset = 0;

        for (int i = 0; i < numPoints; i++) {
            gc.setDestinationGeographicPoint(gridCoordGrid[offset],
                    gridCoordGrid[offset + 1]);
            offset += 2;
            radius[i] = gc.getOrthodromicDistance();
        }
        return radius;
    }
}
